use crate::{Condition, ContinueIfStatement, Expr, IfStatement, State, Statement, Type};

fn transpile_expr(expr: &Expr, state: &mut State) -> Result<(String, Option<String>), String> {
    match expr {
        Expr::Num(x) => Ok((format!("'{x}'"), None)),
        Expr::Str(s) => Ok((format!("'{s}'"), None)),
        Expr::Var(s) => {
            if let Some((sh_variable_name, _type)) = state.get_var(s) {
                Ok((format!("${sh_variable_name}"), None))
            } else {
                Err(format!("Could not find variable {s}"))
            }
        }
        Expr::Call(func, args) => {
            if let Some(f) = state.get_func(func) {
                if f.1.is_none() {
                    return Err(format!("Function {func} does not have a return value. Maybe you meant to call it piped? (<{func})"));
                }
            }
            Ok((
                format!("$__{func}_return_value"),
                Some(call_function(func, args, state)?),
            ))
        }
        Expr::CallPiped(f, args) => {
            let mut output = String::from("$(");
            output.push_str(&call_function(f, args, state)?);
            output.push(')');
            Ok((output, None))
        }
        Expr::Pipe(first, second) => {
            let mut output = String::from("$(");
            let (new_output, run_before) = transpile_repr(first, state)?;
            output.push_str(&new_output);
            output.push_str(" | ");

            let (second_output, second_run_before) = transpile_repr(second, state)?;
            if let Some(run) = second_run_before {
                run_before.unwrap_or_default().push_str(&run);
            }
            output.push_str(&second_output);
            output.push(')');
            Ok((output, None))
        }
    }
}

fn call_function(f: &str, args: &Vec<Expr>, state: &mut State) -> Result<String, String> {
    let mut output = String::new();
    if let Some((actual_args, _return_type)) = state.get_func(f) {
        if args.len() != actual_args.len() {
            return Err(format!(
                "{f} expected {} arguments, but got {}",
                actual_args.len(),
                args.len()
            ));
        }
        for (arg, (arg_name, possible_arg_type)) in args.iter().zip(actual_args) {
            if let Some(arg_type) = possible_arg_type {
                if &arg.get_type(state) != arg_type {
                    return Err(format!(
                        "Expected {:?} to match the type of {arg_name} ({:?})",
                        arg, arg_type
                    ));
                }
            }
        }
    }
    let mut function_call_output = String::from(f);
    for arg in args.iter() {
        function_call_output.push(' ');
        let (normal_expr, run_before) = transpile_expr(arg, state)?;
        if let Some(run) = run_before {
            output.push_str(&run);
            output.push('\n');
        }
        function_call_output.push_str(&normal_expr);
    }
    output.push_str(&function_call_output);
    Ok(output)
}

fn transpile_repr(expr: &Expr, state: &mut State) -> Result<(String, Option<String>), String> {
    match expr {
        Expr::Call(f, args) => call_function(f, args, state).map(|v| (v, None)),
        Expr::Pipe(first, second) => {
            let (mut output, mut run_before) = transpile_repr(first, state)?;
            output.push_str(" | ");

            let (second_output, second_run_before) = transpile_repr(second, state)?;
            if let Some(run) = second_run_before {
                run_before = Some(run_before.unwrap_or_default() + &run);
            }

            output.push_str(&second_output);
            Ok((output, run_before))
        }
        other => transpile_expr(other, state),
    }
}

enum AssignmentType {
    Local,
    Global,
}

fn assignment(
    ident: &str,
    var_type: &Option<Type>,
    value: &Expr,
    assignment_type: AssignmentType,
    state: &mut State,
) -> Result<(String, Option<String>), String> {
    let mut output = match assignment_type {
        AssignmentType::Local => String::from("local "),
        AssignmentType::Global => String::new(),
    };
    output.push_str(ident);
    output.push('=');

    if let Some(attempted_type) = var_type {
        let expr_type = &value.get_type(state);
        if attempted_type != expr_type {
            return Err(format!(
                "Type {:?} does not match {:?}",
                attempted_type, expr_type
            ));
        }
    }

    let (new_output, run_before) = transpile_expr(value, state)?;
    output.push_str(&new_output);

    let scope = match assignment_type {
        AssignmentType::Local => state.scopes.last_mut(),
        AssignmentType::Global => state.scopes.last_mut(),
    };
    scope
        .unwrap()
        .vars
        .insert(ident.to_owned(), (ident.to_owned(), *var_type));
    Ok((output, run_before))
}

fn transpile(statement: &Statement, state: &mut State) -> Result<(String, Option<String>), String> {
    match statement {
        Statement::Expression(expr) => transpile_repr(expr, state),
        Statement::Assignment(ident, var_type, value) => {
            assignment(ident, var_type, value, AssignmentType::Global, state)
        }
        Statement::LocalAssignment(ident, var_type, value) => {
            assignment(ident, var_type, value, AssignmentType::Local, state)
        }
        Statement::Function(ident, args, return_type, conts) => {
            state
                .scopes
                .first_mut()
                .unwrap()
                .functions
                .insert(ident.to_owned(), (args.to_owned(), *return_type));
            state.new_scope(ident);
            for (i, (arg, arg_type)) in args.iter().enumerate() {
                state
                    .scopes
                    .last_mut()
                    .unwrap()
                    .vars
                    .insert(arg.to_owned(), ((i + 1).to_string(), *arg_type));
            }

            let mut output = String::from(ident);
            output.push_str("() {\n");
            output.push_str(&transpile_from_ast(conts, state)?);
            output.push('}');
            state.end_scope();
            Ok((output, None))
        }
        Statement::Return(value) => {
            let func_name = &state.scopes.last().unwrap().name;
            if let Some(return_type) = state.get_func(func_name).unwrap().1 {
                assignment(
                    &format!("__{func_name}_return_value"),
                    &Some(return_type),
                    value,
                    AssignmentType::Global,
                    state,
                )
            } else {
                Err(format!("{func_name} does not return a value"))
            }
        }
        Statement::If(if_statement) => transpile_if(if_statement, state, true),
        Statement::Empty => Ok((String::new(), None)),
    }
}

fn transpile_condition(
    condition: &Condition,
    state: &mut State,
) -> Result<(String, Option<String>), String> {
    match condition {
        Condition::Expression(expr) => transpile_repr(expr, state),
        Condition::Operator(op, expr1, expr2) => {
            let mut output = String::from("[ ");
            let (new_output, mut run_before) = transpile_repr(expr1, state)?;
            output.push_str(&new_output);

            output.push(' ');
            let shell_op = match *op {
                "==" => "=",
                "!=" => "!=",
                ">=" => "-ge",
                ">" => "-gt",
                "<=" => "-le",
                "<" => "-lt",
                _ => unreachable!(),
            };
            output.push_str(shell_op);
            output.push(' ');

            let (second_output, second_run_before) = transpile_repr(expr2, state)?;
            if let Some(run) = second_run_before {
                run_before = Some(run_before.unwrap_or_default() + &run);
            }

            output.push_str(&second_output);
            output.push_str(" ]");
            Ok((output, run_before))
        }
        Condition::Not(cond) => {
            let mut output = String::from("not ");
            let (new_output, run_before) = transpile_condition(cond, state)?;
            output.push_str(&new_output);
            Ok((output, run_before))
        }
        Condition::InParens(cond) => {
            let mut output = String::from("(");
            let (new_output, run_before) = transpile_condition(cond, state)?;
            output.push_str(&new_output);
            output.push(')');
            Ok((output, run_before))
        }
        Condition::And(cond1, cond2) => {
            let (mut output, mut run_before) = transpile_condition(cond1, state)?;
            output.push_str(" && ");

            let (second_output, second_run_before) = transpile_condition(cond2, state)?;
            if let Some(run) = second_run_before {
                run_before = Some(run_before.unwrap_or_default() + &run);
            }
            output.push_str(&second_output);
            Ok((output, run_before))
        }
        Condition::Or(cond1, cond2) => {
            let (mut output, mut run_before) = transpile_condition(cond1, state)?;
            output.push_str(" || ");
            let (second_output, second_run_before) = transpile_condition(cond2, state)?;
            if let Some(run) = second_run_before {
                run_before = Some(run_before.unwrap_or_default() + &run);
            }
            output.push_str(&second_output);
            Ok((output, run_before))
        }
    }
}

fn transpile_if(
    if_statement: &IfStatement,
    state: &mut State,
    ends_if: bool,
) -> Result<(String, Option<String>), String> {
    let mut output = String::from("if ");
    let (new_output, run_before) = transpile_condition(&if_statement.cond, state)?;
    output.push_str(&new_output);
    output.push_str("; then\n");
    output.push_str(&transpile_from_ast(&if_statement.statements, state)?);
    if let Some(to_continue) = if_statement.continue_if.as_ref() {
        match to_continue {
            ContinueIfStatement::Else(statements) => {
                output.push_str("else \n");
                output.push_str(&transpile_from_ast(statements, state)?);
            }
            ContinueIfStatement::If(if_statement) => {
                output.push_str("el");
                // FIX: this (use else and then if prbly?)
                output.push_str(&transpile_if(if_statement, state, false)?.0)
            }
        }
    }
    if ends_if {
        output.push_str("fi");
    }
    Ok((output, run_before))
}

pub fn transpile_from_ast(conts: &Vec<Statement>, state: &mut State) -> Result<String, String> {
    eprintln!("{:#?}", conts);
    let mut compiled = String::new();
    for expr in conts {
        let (output, run_before) = transpile(expr, state)?;
        if let Some(run) = run_before {
            compiled.push_str(&run);
            compiled.push('\n');
        }
        compiled.push_str(&output);
        compiled.push('\n');
    }
    Ok(compiled)
}
