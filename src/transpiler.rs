use crate::{Condition, ContinueIfStatement, Expr, IfStatement, State, Statement};

fn transpile_expr(expr: &Expr, state: &mut State) -> Result<String, String> {
    match expr {
        Expr::Num(x) => Ok(format!("'{x}'")),
        Expr::Str(s) => Ok(format!("'{s}'")),
        Expr::Var(s) => {
            if let Some(sh_variable_name) = state.get_var(s) {
                Ok(format!("${sh_variable_name}"))
            } else {
                Err(format!("Could not find variable {s}"))
            }
        }
        Expr::Call(f, args) => {
            let mut output = String::from("$(");
            output.push_str(f);
            for arg in args {
                output.push(' ');
                output.push_str(&transpile_expr(arg, state)?);
            }
            output.push(')');
            Ok(output)
        }
        Expr::Pipe(first, second) => {
            let mut output = String::from("$(");
            output.push_str(&transpile_repr(first, state)?);
            output.push_str(" | ");
            output.push_str(&transpile_repr(second, state)?);
            output.push(')');
            Ok(output)
        }
    }
}

fn transpile_repr(expr: &Expr, state: &mut State) -> Result<String, String> {
    match expr {
        Expr::Call(f, args) => {
            let mut output = String::from(f);
            for arg in args {
                output.push(' ');
                output.push_str(&transpile_expr(arg, state)?);
            }
            Ok(output)
        }
        Expr::Pipe(first, second) => {
            let mut output = transpile_repr(first, state)?;
            output.push_str(" | ");
            output.push_str(&transpile_repr(second, state)?);
            Ok(output)
        }
        other => transpile_expr(other, state),
    }
}

fn transpile(statement: &Statement, state: &mut State) -> Result<String, String> {
    match statement {
        Statement::Expression(expr) => transpile_repr(expr, state),
        Statement::Assignment(ident, value) => {
            let mut output = String::from(ident);
            output.push('=');
            output.push_str(&transpile_expr(value, state)?);
            state
                .scopes
                .first_mut()
                .unwrap()
                .vars
                .insert(ident.to_owned(), ident.to_owned());
            Ok(output)
        }
        Statement::LocalAssignment(ident, value) => {
            let mut output = String::from("local ");
            output.push_str(ident);
            output.push('=');
            output.push_str(&transpile_expr(value, state)?);
            state
                .scopes
                .last_mut()
                .unwrap()
                .vars
                .insert(ident.to_owned(), ident.to_owned());
            Ok(output)
        }
        Statement::Function(ident, args, conts) => {
            state.new_scope();
            for (i, arg) in args.first().unwrap().iter().enumerate() {
                state
                    .scopes
                    .last_mut()
                    .unwrap()
                    .vars
                    .insert(arg.to_owned(), (i + 1).to_string());
            }
            let mut output = String::from(ident);
            output.push_str("() {\n");
            output.push_str(&transpile_from_ast(conts, state)?);
            output.push('}');
            state.end_scope();
            Ok(output)
        }
        Statement::If(if_statement) => transpile_if(if_statement, state, true),
        Statement::Empty => Ok(String::new()),
    }
}

fn transpile_condition(condition: &Condition, state: &mut State) -> Result<String, String> {
    match condition {
        Condition::Expression(expr) => transpile_repr(expr, state),
        Condition::Equal(expr1, expr2) => {
            let mut output = String::from("[ ");
            output.push_str(&transpile_repr(expr1, state)?);
            output.push_str(" = ");
            output.push_str(&transpile_repr(expr2, state)?);
            output.push_str(" ]");
            Ok(output)
        }
        Condition::Not(cond) => {
            let mut output = String::from("not ");
            output.push_str(&transpile_condition(cond, state)?);
            Ok(output)
        }
        Condition::And(cond1, cond2) => {
            let mut output = transpile_condition(cond1, state)?;
            output.push_str(" && ");
            output.push_str(&transpile_condition(cond2, state)?);
            Ok(output)
        }
    }
}

fn transpile_if(
    if_statement: &IfStatement,
    state: &mut State,
    ends_if: bool,
) -> Result<String, String> {
    let mut output = String::from("if ");
    output.push_str(&transpile_condition(&if_statement.cond, state)?);
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
                output.push_str(&transpile_if(if_statement, state, false)?)
            }
        }
    }
    if ends_if {
        output.push_str("fi");
    }
    Ok(output)
}

pub fn transpile_from_ast(conts: &Vec<Statement>, state: &mut State) -> Result<String, String> {
    eprintln!("{:#?}", conts);
    let mut compiled = String::new();
    for expr in conts {
        let output = transpile(expr, state)?;
        compiled.push_str(&output);
        compiled.push('\n');
    }
    Ok(compiled)
}
