use chumsky::{prelude::Rich, span::Span};

use crate::{
    parser::Spanned, Condition, ContinueIfStatement, Expr, IfStatement, State, Statement, Type,
};

fn transpile_expr<'a>(
    expr: Spanned<&'a Expr>,
    state: &mut State,
) -> Result<(String, Option<String>), Rich<'a, char>> {
    match expr.0 {
        Expr::Num(x) => Ok((format!("'{x}'"), None)),
        Expr::Str(s) => Ok((format!("'{s}'"), None)),
        Expr::Var(s) => {
            if let Some((sh_variable_name, _type)) = state.get_var(s) {
                Ok((format!("${sh_variable_name}"), None))
            } else {
                Err(Rich::custom(expr.1, format!("Could not find variable {s}")))
            }
        }
        Expr::Call(func, args) => {
            if let Some(f) = state.get_func(func) {
                if f.return_value.is_none() {
                    return Err(Rich::custom(
                        expr.1,
                        format!("Function {func} does not have a return value. Maybe you meant to call it piped? (<{func})"))
                    );
                }
            }
            let var_name = format!("$__{func}_return_value_{}", state.get_times_called(func));
            Ok((
                var_name,
                Some(call_function(func, (&args.0, args.1), state)?),
            ))
        }
        Expr::CallPiped(f, args) => {
            let mut output = String::from("$(");
            output.push_str(&call_function(f, (&args.0, args.1), state)?);
            output.push(')');
            Ok((output, None))
        }
        Expr::Pipe(first, second) => {
            let mut output = String::from("$(");
            let (new_output, run_before) = transpile_repr((&first.0, first.1), state)?;
            output.push_str(&new_output);
            output.push_str(" | ");

            let (second_output, second_run_before) = transpile_repr((&second.0, second.1), state)?;
            if let Some(run) = second_run_before {
                run_before.unwrap_or_default().push_str(&run);
            }
            output.push_str(&second_output);
            output.push(')');
            Ok((output, None))
        }
    }
}

fn call_function<'a>(
    f: &str,
    args: Spanned<&'a Vec<Spanned<Expr>>>,
    state: &mut State,
) -> Result<String, Rich<'a, char>> {
    let (args, args_span) = args;
    let mut output = String::new();
    if let Some(func) = state.get_func(f) {
        if args.len() != func.args.len() {
            let span = match args.len() {
                0 => args_span,
                1 => args.first().unwrap().1,
                _ => args.first().unwrap().1.union(args.last().unwrap().1),
            };
            return Err(Rich::custom(
                span,
                format!(
                    "{f} expected {} arguments, but got {}",
                    func.args.len(),
                    args.len()
                ),
            ));
        }
        for ((arg, span), (arg_name, possible_arg_type)) in args.iter().zip(func.args.iter()) {
            if let Some(arg_type) = possible_arg_type {
                if &arg.get_type(state) != arg_type {
                    return Err(Rich::custom(
                        *span,
                        format!(
                            "Expected {:?} to match the type of {arg_name} ({:?})",
                            arg, arg_type
                        ),
                    ));
                }
            }
        }
    }
    let mut function_call_output = String::from(f);
    if f != "echo" {
        // TODO: make builtin commands return values, make this different
        function_call_output.push(' ');
        function_call_output.push_str(&state.get_times_called(f).to_string());
    }
    for (arg, span) in args.iter() {
        function_call_output.push(' ');
        let (normal_expr, run_before) = transpile_expr((arg, *span), state)?;
        if let Some(run) = run_before {
            output.push_str(&run);
            output.push('\n');
        }
        function_call_output.push_str(&normal_expr);
    }
    output.push_str(&function_call_output);
    state.call_func(f);
    Ok(output)
}

fn transpile_repr<'a>(
    expr: Spanned<&'a Expr>,
    state: &mut State,
) -> Result<(String, Option<String>), Rich<'a, char>> {
    match expr.0 {
        Expr::Call(f, args) => call_function(f, (&args.0, args.1), state).map(|v| (v, None)),
        Expr::Pipe(first, second) => {
            let (mut output, mut run_before) = transpile_repr((&first.0, first.1), state)?;
            output.push_str(" | ");

            let (second_output, second_run_before) = transpile_repr((&second.0, second.1), state)?;
            if let Some(run) = second_run_before {
                run_before = Some(run_before.unwrap_or_default() + &run);
            }

            output.push_str(&second_output);
            Ok((output, run_before))
        }
        _ => transpile_expr(expr, state),
    }
}

enum AssignmentType {
    Local,
    Global,
}

fn assignment<'state, 'src: 'state>(
    ident: String,
    var_type: &Option<Type>,
    value: Spanned<&'src Expr>,
    assignment_type: AssignmentType,
    state: &mut State<'state>,
) -> Result<(String, Option<String>), Rich<'src, char>> {
    let mut output = match assignment_type {
        AssignmentType::Local => String::from("local "),
        AssignmentType::Global => String::new(),
    };
    output.push_str(&ident);
    output.push('=');

    if let Some(attempted_type) = var_type {
        let expr_type = &value.0.get_type(state);
        if attempted_type != expr_type {
            return Err(Rich::custom(
                value.1,
                format!("Type {:?} does not match {:?}", attempted_type, expr_type),
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
        .insert(ident.clone(), (ident, *var_type));
    Ok((output, run_before))
}

fn transpile<'state, 'src: 'state>(
    statement: &'src Spanned<Statement<'src>>,
    state: &mut State<'state>,
) -> Result<(String, Option<String>), Rich<'src, char>> {
    match &statement.0 {
        Statement::Expression(expr) => transpile_repr((&expr.0, expr.1), state),
        Statement::Assignment(ident, var_type, value) => assignment(
            ident.to_string(),
            var_type,
            (&value.0, value.1),
            AssignmentType::Global,
            state,
        ),
        Statement::LocalAssignment(ident, var_type, value) => assignment(
            ident.to_string(),
            var_type,
            (&value.0, value.1),
            AssignmentType::Local,
            state,
        ),
        Statement::Function(ident, args, return_value, conts) => {
            state.scopes.first_mut().unwrap().functions.insert(
                ident.to_owned(),
                crate::Function {
                    args,
                    return_value: *return_value,
                    times_called: 0,
                },
            );
            state.new_scope(ident);
            for (i, (arg, arg_type)) in args.iter().enumerate() {
                state
                    .scopes
                    .last_mut()
                    .unwrap()
                    .vars
                    .insert(arg.to_string(), ((i + 2).to_string(), *arg_type));
            }

            let mut output = String::from(*ident);
            output.push_str("() {\n");
            output.push_str(&transpile_from_ast(&conts.0, state)?);
            output.push('}');
            state.end_scope();
            Ok((output, None))
        }
        Statement::Return(value) => {
            let func_name = state.scopes.last().unwrap().name;
            if let Some(return_type) = state.get_func(func_name).unwrap().return_value {
                let (mut output, run_before) = assignment(
                    format!("__return_val"),
                    &Some(return_type),
                    (&value.0, value.1),
                    AssignmentType::Global,
                    state,
                )?;
                output.push_str(&format!(
                    "\neval \"__{func_name}_return_value_$1=\\\"$__return_val\\\"\""
                ));
                Ok((output, run_before))
            } else {
                Err(Rich::custom(
                    statement.1,
                    format!("{func_name} does not return a value"),
                ))
            }
        }
        Statement::If(if_statement) => transpile_if((&if_statement.0, if_statement.1), state, true),
        Statement::Empty => Ok((String::new(), None)),
    }
}

fn transpile_condition<'src>(
    condition: Spanned<&'src Condition>,
    state: &mut State,
) -> Result<(String, Option<String>), Rich<'src, char>> {
    match condition.0 {
        Condition::Expression(expr) => transpile_repr((&expr.0, condition.1), state),
        Condition::Operator(op, expr1, expr2) => {
            let mut output = String::from("[ ");
            let (new_output, mut run_before) = transpile_repr((&expr1.0, condition.1), state)?;
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

            let (second_output, second_run_before) =
                transpile_repr((&expr2.0, condition.1), state)?;
            if let Some(run) = second_run_before {
                run_before = Some(run_before.unwrap_or_default() + &run);
            }

            output.push_str(&second_output);
            output.push_str(" ]");
            Ok((output, run_before))
        }
        Condition::Not(cond) => {
            let mut output = String::from("not ");
            let (new_output, run_before) = transpile_condition((cond, condition.1), state)?;
            output.push_str(&new_output);
            Ok((output, run_before))
        }
        Condition::InParens(cond) => {
            let mut output = String::from("(");
            let (new_output, run_before) = transpile_condition((cond, condition.1), state)?;
            output.push_str(&new_output);
            output.push(')');
            Ok((output, run_before))
        }
        Condition::And(cond1, cond2) => {
            let (mut output, mut run_before) = transpile_condition((cond1, condition.1), state)?;
            output.push_str(" && ");

            let (second_output, second_run_before) =
                transpile_condition((cond2, condition.1), state)?;
            if let Some(run) = second_run_before {
                run_before = Some(run_before.unwrap_or_default() + &run);
            }
            output.push_str(&second_output);
            Ok((output, run_before))
        }
        Condition::Or(cond1, cond2) => {
            let (mut output, mut run_before) = transpile_condition((cond1, condition.1), state)?;
            output.push_str(" || ");
            let (second_output, second_run_before) =
                transpile_condition((cond2, condition.1), state)?;
            if let Some(run) = second_run_before {
                run_before = Some(run_before.unwrap_or_default() + &run);
            }
            output.push_str(&second_output);
            Ok((output, run_before))
        }
    }
}

fn transpile_if<'state, 'src: 'state>(
    if_statement: Spanned<&'src IfStatement>,
    state: &mut State<'state>,
    ends_if: bool,
) -> Result<(String, Option<String>), Rich<'src, char>> {
    let mut output = String::from("if ");
    let condition = &if_statement.0.cond;
    let (new_output, run_before) = transpile_condition((&condition.0, condition.1), state)?;
    output.push_str(&new_output);
    output.push_str("; then\n");
    output.push_str(&transpile_from_ast(&if_statement.0.statements, state)?);
    if let Some(to_continue) = if_statement.0.continue_if.as_ref() {
        match to_continue {
            ContinueIfStatement::Else(statements) => {
                output.push_str("else \n");
                output.push_str(&transpile_from_ast(statements, state)?);
            }
            ContinueIfStatement::If(if_statement) => {
                output.push_str("el");
                // FIX: this (use else and then if prbly?)
                output.push_str(&transpile_if((&if_statement.0, if_statement.1), state, false)?.0)
            }
        }
    }
    if ends_if {
        output.push_str("fi");
    }
    Ok((output, run_before))
}

pub fn transpile_from_ast<'state, 'src: 'state>(
    conts: &'src Vec<Spanned<Statement<'src>>>,
    state: &mut State<'state>,
) -> Result<String, Rich<'src, char>> {
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
