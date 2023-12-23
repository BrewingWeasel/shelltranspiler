use crate::{parser::Spanned, ContinueIfStatement, Expr, IfStatement, State, Statement, Type};
use chumsky::prelude::Rich;

use super::{
    condition::transpile_condition,
    expressions::{transpile_expr, transpile_repr},
    transpile_from_ast,
};

pub fn transpile_statement<'state, 'src: 'state>(
    statement: &'src Spanned<Statement<'src>>,
    state: &mut State<'state>,
) -> Result<(String, Option<String>), Rich<'src, char>> {
    match &statement.0 {
        Statement::Expression(expr) => {
            if matches!(
                expr.0,
                Expr::Call(_, _, _)
                    | Expr::CallModule(_, _, _, _)
                    | Expr::Pipe(_, _)
                    | Expr::Macro(_, _)
            ) {
                transpile_repr((&expr.0, expr.1), state)
            } else {
                Ok((String::new(), None))
            }
        }
        Statement::Assignment(first_assignment, ident, var_type, value) => assignment(
            *first_assignment,
            (*ident).to_string(),
            var_type,
            (&value.0, value.1),
            AssignmentType::Global,
            state,
        ),
        Statement::LocalAssignment(first_assignment, ident, var_type, value) => assignment(
            *first_assignment,
            (*ident).to_string(),
            var_type,
            (&value.0, value.1),
            AssignmentType::Local,
            state,
        ),
        Statement::Function(ident, type_vars, args, kwargs, return_value, conts) => {
            state.new_scope(ident, return_value.clone());
            for (i, (arg, arg_type)) in args.iter().enumerate() {
                if let Some((generic_ident, _)) =
                    arg_type.as_ref().and_then(|v| v.get_generic_var())
                {
                    if let Some(type_vars) = type_vars {
                        if !type_vars.contains(&generic_ident) {
                            return Err(Rich::custom(statement.1, format!("Could not find generic variable %{generic_ident}. Other generic variables that were found: {type_vars:?}")));
                        }
                    } else {
                        return Err(Rich::custom(statement.1, format!("Tried to use generic variable %{generic_ident}, but couldn't find any generic variables")));
                    }
                }
                state.scopes.last_mut().unwrap().vars.insert(
                    (*arg).to_string(),
                    ((i + 1).to_string(), arg_type.clone().unwrap_or(Type::Any)),
                );
            }

            let mut output = String::from(*ident);
            output.push_str(
                "() {
local __n_timecalled=$1
shift
",
            );
            if !kwargs.is_empty() {
                let mut case_statement = String::from(
                    "while test $# -gt 0; do
case \"$1\" in
",
                );

                for kwarg in kwargs {
                    let (default_assignment, run_before) = raw_assignment(
                        kwarg.ident.to_owned(),
                        (&kwarg.default.0, kwarg.default.1),
                        AssignmentType::Local,
                        state,
                    )?;
                    output.push_str(&default_assignment);
                    output.push('\n');
                    if let Some(before) = run_before {
                        output.push_str(&before);
                        output.push('\n');
                    }
                    case_statement.push_str(&format!(
                        r#"--{})
shift
export {}="$1"
shift
;;
"#,
                        kwarg.ident, kwarg.ident
                    ));
                    state.scopes.last_mut().unwrap().vars.insert(
                        kwarg.ident.to_owned(),
                        (
                            kwarg.ident.to_owned(),
                            kwarg.kwarg_type.clone().unwrap_or(Type::Any),
                        ),
                    );
                }
                case_statement.push_str(
                    "*)
break
;;
esac
done
",
                );
                output.push_str(&case_statement);
            }

            output.push_str(&transpile_from_ast(&conts.0, state, false)?);
            output.push('}');
            state.end_scope();
            state.scopes.last_mut().unwrap().functions.insert(
                ident.to_owned(),
                crate::Function {
                    args: args.clone(),
                    kwargs: kwargs.clone(),
                    return_value: return_value.to_owned(),
                    times_called: 0,
                    contents: output,
                },
            );

            Ok((String::new(), None))
        }
        Statement::Return(value) => {
            let last_scope = state.scopes.last().unwrap();
            let func_name = last_scope.name;
            if let Some(_return_type) = last_scope.return_value.clone() {
                let (mut output, run_before) = raw_assignment(
                    "__return_val".to_string(),
                    (&value.0, value.1),
                    AssignmentType::Global,
                    state,
                )?;
                output.push_str(&format!(
                    "\neval \"__{func_name}_return_value_$__n_timecalled=\\\"$__return_val\\\"\""
                ));
                output.push_str("\nreturn 0");
                Ok((output, run_before))
            } else {
                Err(Rich::custom(
                    statement.1,
                    format!("{func_name} does not return a value"),
                ))
            }
        }
        Statement::For(var, loop_name, body) => {
            if let Type::List(looped_item_type) = loop_name.0.get_type(state) {
                let (list_refr, run_before) = transpile_repr((&loop_name.0, loop_name.1), state)?;
                let list_refr = list_refr.trim_matches(['$', '"']);
                let index_value = state.new_for_loop_index();
                let mut output = format!(
                    r#"{}=0; while eval "{}=\"\${{${{{}}}_${{{}}}}}\"; [ ${} -lt \"\$${{{}}}_len\" ]"; do"#,
                    &index_value, var, list_refr, &index_value, &index_value, list_refr
                );
                output.push('\n');
                state
                    .scopes
                    .last_mut()
                    .unwrap()
                    .vars
                    .insert((*var).to_string(), ((*var).to_string(), *looped_item_type));
                output.push_str(&transpile_from_ast(&body.0, state, false)?);
                output.push_str(&format!(
                    "\n{}=$(({} + 1))\ndone",
                    &index_value, &index_value
                ));
                Ok((output, run_before))
                // TODO:
            } else {
                Err(Rich::custom(
                    loop_name.1,
                    format!("{:?} does not return a list", loop_name.0),
                ))
            }
        }
        Statement::While(condition, body) => {
            let mut output = String::from("while ");
            let (new_output, run_before) = transpile_condition((&condition.0, condition.1), state)?;
            output.push_str(&new_output);
            output.push_str("; do\n");
            output.push_str(&transpile_from_ast(&body.0, state, false)?);
            output.push_str("\ndone");
            Ok((output, run_before))
        }
        Statement::If(if_statement) => transpile_if((&if_statement.0, if_statement.1), state),
        Statement::Empty | Statement::Import(_) => Ok((String::new(), None)),
        Statement::Pub(_) => unreachable!(),
    }
}

pub fn transpile_if<'state, 'src: 'state>(
    if_statement: Spanned<&'src IfStatement>,
    state: &mut State<'state>,
) -> Result<(String, Option<String>), Rich<'src, char>> {
    let mut output = String::from("if ");
    let condition = &if_statement.0.cond;
    let (new_output, run_before) = transpile_condition((&condition.0, condition.1), state)?;
    output.push_str(&new_output);
    output.push_str("; then\n");
    output.push_str(&transpile_from_ast(
        &if_statement.0.statements,
        state,
        false,
    )?);
    if let Some(to_continue) = if_statement.0.continue_if.as_ref() {
        match to_continue {
            ContinueIfStatement::Else(statements) => {
                output.push_str("else \n");
                output.push_str(&transpile_from_ast(statements, state, false)?);
            }
            ContinueIfStatement::If(if_statement) => {
                output.push_str("else\n");
                let (new_output, run_before) =
                    transpile_if((&if_statement.0, if_statement.1), state)?;
                output.push_str(&run_before.unwrap_or_default());
                output.push('\n');
                output.push_str(&new_output);
                output.push('\n');
            }
        }
    }
    output.push_str("fi");
    Ok((output, run_before))
}

#[derive(Clone, Copy)]
pub enum AssignmentType {
    Local,
    Global,
}

pub fn raw_assignment<'state, 'src: 'state>(
    ident: String,
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
    let (new_output, run_before) = transpile_expr(value, state)?;
    output.push_str(&new_output);
    Ok((output, run_before))
}

pub fn assignment<'state, 'src: 'state>(
    first_assignment: bool,
    ident: String,
    var_type: &Option<Type>,
    value: Spanned<&'src Expr>,
    assignment_type: AssignmentType,
    state: &mut State<'state>,
) -> Result<(String, Option<String>), Rich<'src, char>> {
    let expr_type = &value.0.get_type(state);
    if let Some(attempted_type) = var_type {
        if !attempted_type.matches(expr_type) {
            return Err(Rich::custom(
                value.1,
                format!("Type {attempted_type} does not match {expr_type}"),
            ));
        }
    }

    if let Some(var) = state.get_var(&ident) {
        if first_assignment {
            return Err(Rich::custom(
                value.1,
                format!("Variable {ident} already exists, shadowing is not supported (yet?)",),
            ));
        }
        if !var.1.matches(expr_type) {
            return Err(Rich::custom(
                value.1,
                format!(
                    "Mismatched types: expected {} but found {}",
                    var.1, expr_type
                ),
            ));
        }
    } else if !first_assignment {
        return Err(Rich::custom(
            value.1,
            format!("Variable {ident} does not exist yet, maybe you meant to put var in front?"),
        ));
    }

    let scope = match assignment_type {
        AssignmentType::Local => state.scopes.last_mut(),
        AssignmentType::Global => state.scopes.first_mut(),
    };

    scope
        .unwrap()
        .vars
        .insert(ident.clone(), (ident.clone(), expr_type.clone()));

    raw_assignment(ident, value, assignment_type, state)
}
