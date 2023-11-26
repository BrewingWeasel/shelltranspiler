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
        Statement::Expression(expr) => transpile_repr((&expr.0, expr.1), state),
        Statement::Assignment(ident, var_type, value) => assignment(
            (*ident).to_string(),
            var_type,
            (&value.0, value.1),
            AssignmentType::Global,
            state,
        ),
        Statement::LocalAssignment(ident, var_type, value) => assignment(
            (*ident).to_string(),
            var_type,
            (&value.0, value.1),
            AssignmentType::Local,
            state,
        ),
        Statement::Function(ident, args, kwargs, return_value, conts) => {
            state.scopes.first_mut().unwrap().functions.insert(
                ident.to_owned(),
                crate::Function {
                    args,
                    kwargs,
                    return_value: return_value.to_owned(),
                    times_called: 0,
                },
            );
            state.new_scope(ident);
            for (i, (arg, arg_type)) in args.iter().enumerate() {
                state.scopes.last_mut().unwrap().vars.insert(
                    (*arg).to_string(),
                    ((i + 1).to_string(), arg_type.to_owned()),
                );
            }

            let mut output = String::from(*ident);
            output.push_str(
                "() {
__n_timecalled=$1
",
            );
            if !kwargs.is_empty() {
                let mut case_statement = String::from(
                    "shift
while test $# -gt 0; do
case \"$1\" in
",
                );

                for kwarg in kwargs {
                    let (default_assignment, run_before) = assignment(
                        kwarg.ident.to_owned(),
                        &kwarg.kwarg_type,
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
            output.push_str(&transpile_from_ast(&conts.0, state)?);
            output.push('}');
            state.end_scope();
            Ok((output, None))
        }
        Statement::Return(value) => {
            let func_name = state.scopes.last().unwrap().name;
            if let Some(return_type) = state.get_func(func_name).unwrap().return_value.clone() {
                let (mut output, run_before) = assignment(
                    "__return_val".to_string(),
                    &Some(return_type),
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
                let index_value = state.new_for_loop_index();
                let mut output = format!(
                    r#"{}=0; while eval "{}=\"\${{$(echo {})_$(echo "${}")?unset}}\"" 2> /dev/null; do"#,
                    &index_value, var, list_refr, &index_value
                );
                output.push('\n');
                state.scopes.last_mut().unwrap().vars.insert(
                    (*var).to_string(),
                    ((*var).to_string(), Some(*looped_item_type)),
                );
                output.push_str(&transpile_from_ast(&body.0, state)?);
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
        Statement::If(if_statement) => transpile_if((&if_statement.0, if_statement.1), state),
        Statement::Empty => Ok((String::new(), None)),
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
    output.push_str(&transpile_from_ast(&if_statement.0.statements, state)?);
    if let Some(to_continue) = if_statement.0.continue_if.as_ref() {
        match to_continue {
            ContinueIfStatement::Else(statements) => {
                output.push_str("else \n");
                output.push_str(&transpile_from_ast(statements, state)?);
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

pub fn assignment<'state, 'src: 'state>(
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
                format!("Type {attempted_type:?} does not match {expr_type:?}"),
            ));
        }
    }

    let (new_output, run_before) = transpile_expr(value, state)?;
    output.push_str(&new_output);

    let scope = match assignment_type {
        AssignmentType::Local => state.scopes.last_mut(),
        AssignmentType::Global => state.scopes.first_mut(),
    };
    scope
        .unwrap()
        .vars
        .insert(ident.clone(), (ident, var_type.to_owned()));
    Ok((output, run_before))
}