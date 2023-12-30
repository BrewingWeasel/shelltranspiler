use crate::{
    parser::Spanned, utils::add_option_to_str, Condition, ExpressionToMatch, MatchStatement, State,
    Type,
};
use chumsky::prelude::Rich;

use super::expressions::transpile_expr;

pub fn transpile_condition<'src>(
    condition: Spanned<&'src Condition>,
    state: &mut State,
) -> Result<(String, Option<String>, Vec<(String, String, Type)>), Rich<'src, char>> {
    match condition.0 {
        Condition::Expression(expr) => {
            let get_type = expr.0.get_type(state);
            if get_type != Type::Bool {
                return Err(Rich::custom(
                    condition.1,
                    format!("Expected type of Bool, found {get_type:?}"),
                ));
            };
            let (output, run_before) = transpile_expr((&expr.0, condition.1), state)?;
            Ok((format!("[ {output} = 1 ]"), run_before, Vec::new()))
        }
        Condition::IfLet(match_statement, expr) => transpile_match(
            match_statement,
            (&ExpressionToMatch::Expr(&expr.0), expr.1),
            state,
        ),
        Condition::Operator(op, expr1, expr2) => {
            let mut output = String::from("[ ");
            let (new_output, mut run_before) = transpile_expr((&expr1.0, condition.1), state)?;
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
                transpile_expr((&expr2.0, condition.1), state)?;
            add_option_to_str(&mut run_before, &second_run_before);

            output.push_str(&second_output);
            output.push_str(" ]");
            Ok((output, run_before, Vec::new()))
        }
        Condition::Not(cond) => {
            let mut output = String::from("! ");
            let (new_output, run_before, run_after) =
                transpile_condition((cond, condition.1), state)?;
            output.push_str(&new_output);
            Ok((output, run_before, run_after))
        }
        Condition::InParens(cond) => {
            let mut output = String::from("(");
            let (new_output, run_before, set_vars) =
                transpile_condition((cond, condition.1), state)?;
            output.push_str(&new_output);
            output.push(')');
            Ok((output, run_before, set_vars))
        }
        Condition::And(cond1, cond2) => {
            let (mut output, mut run_before, mut set_vars) =
                transpile_condition((cond1, condition.1), state)?;
            output.push_str(" && ");

            let (second_output, second_run_before, mut second_set_vars) =
                transpile_condition((cond2, condition.1), state)?;
            output.push_str(&second_output);

            add_option_to_str(&mut run_before, &second_run_before);
            set_vars.append(&mut second_set_vars);

            Ok((output, run_before, set_vars))
        }
        Condition::Or(cond1, cond2) => {
            let (mut output, mut run_before, mut set_vars) =
                transpile_condition((cond1, condition.1), state)?;
            output.push_str(" || ");
            let (second_output, second_run_before, mut second_set_vars) =
                transpile_condition((cond2, condition.1), state)?;

            output.push_str(&second_output);

            add_option_to_str(&mut run_before, &second_run_before);
            set_vars.append(&mut second_set_vars);

            Ok((output, run_before, set_vars))
        }
    }
}

fn transpile_expr_to_match<'src>(
    expr: Spanned<&ExpressionToMatch<'src, '_>>,
    state: &mut State,
) -> Result<(String, Option<String>), Rich<'src, char>> {
    match expr.0 {
        ExpressionToMatch::Expr(e) => transpile_expr((&e, expr.1), state),
        ExpressionToMatch::EnumVal(enum_ident, _name, number) => {
            let (output, new_run_before) = transpile_expr_to_match((&**enum_ident, expr.1), state)?;
            Ok((format!("${output}_{number}"), new_run_before))
        }
    }
}

fn transpile_match<'src>(
    match_statement: &'src MatchStatement<'src>,
    expr: Spanned<&ExpressionToMatch<'src, '_>>,
    state: &mut State,
) -> Result<(String, Option<String>, Vec<(String, String, Type)>), Rich<'src, char>> {
    let mut run_before = None;
    let mut checking = Vec::new();
    let mut assignments = Vec::new();

    let (output, new_run_before) = transpile_expr_to_match(expr, state)?;

    match match_statement {
        MatchStatement::Enum(ident, opt_ident, matching) => {
            let get_type = expr.0.get_type(state);
            if let Type::Enum(enum_ident) = get_type {
                if enum_ident != *ident {
                    return Err(Rich::custom(
                        expr.1,
                        format!(
                            "Expected to find Enum of type {ident}, but found Enum {enum_ident}"
                        ),
                    ));
                }

                let enum_type = state.enums.get(*ident).expect("already checked enum");
                if let Some(types) = enum_type.opts.get(*opt_ident) {
                    if matching.len() != types.len() {
                        return Err(Rich::custom(
                            expr.1,
                            format!(
                                "Found {} arguments, expected {}",
                                matching.len(),
                                types.len()
                            ),
                        ));
                    }
                    for (matcher, opt_type) in matching.iter().zip(types.iter()) {
                        if let MatchStatement::LiteralValue(testing_expr) = matcher {
                            if !testing_expr.get_type(state).matches(opt_type) {
                                return Err(Rich::custom(
                                    expr.1,
                                    format!("Expecting type {opt_type}"),
                                ));
                            }
                        }
                    }
                    for (i, matcher) in matching.iter().enumerate() {
                        let (conditions, new_run_before, mut new_assignments) = transpile_match(
                            matcher,
                            (
                                &ExpressionToMatch::EnumVal(Box::new(expr.0), opt_ident, i),
                                expr.1,
                            ),
                            state,
                        )?;
                        checking.push(conditions);
                        add_option_to_str(&mut run_before, &new_run_before);
                        assignments.append(&mut new_assignments);
                    }

                    add_option_to_str(&mut run_before, &new_run_before);
                    checking.push(format!("eval \"[ ${output}_v = {opt_ident} ]\""));
                } else {
                    return Err(Rich::custom(
                        expr.1,
                        format!("Unable to find case {opt_ident} of {ident}"),
                    ));
                }
            } else {
                println!("{matching:?} {match_statement:?} {expr:?}");
                return Err(Rich::custom(
                    expr.1,
                    format!("Expected to find Enum of type {ident}, but found {get_type}"),
                ));
            }
        }
        MatchStatement::Assignment(var) => {
            add_option_to_str(&mut run_before, &new_run_before);
            assignments = vec![((*var).to_string(), output, expr.0.get_type(state))];
        }
        MatchStatement::LiteralValue(val) => {
            let (second_output, second_run_before) = transpile_expr((&val, expr.1), state)?;
            add_option_to_str(&mut run_before, &second_run_before);

            checking.push(format!("eval \"[ {output} = {second_output} ]\""));
        }
    }
    let final_checking = if checking.is_empty() {
        String::from("true") // TODO: hack
    } else {
        checking.join(" && ")
    };
    Ok((final_checking, run_before, assignments))
}

#[cfg(test)]
mod test {
    use chumsky::span::SimpleSpan;

    use crate::{transpiler::condition::transpile_condition, Condition, Expr, State};

    fn dummy_span() -> SimpleSpan {
        SimpleSpan::new(0, 0)
    }

    #[test]
    fn test_basic_equals() {
        let mut state = State::new();
        assert_eq!(
            transpile_condition(
                (
                    &Condition::Operator(
                        "==",
                        (Expr::Str(String::from("hello")), dummy_span()),
                        (Expr::Str(String::from("HELLO")), dummy_span()),
                    ),
                    dummy_span()
                ),
                &mut state,
            ),
            Ok((String::from("[ 'hello' = 'HELLO' ]"), None, Vec::new()))
        );
    }

    #[test]
    fn test_basic_not_expr() {
        let mut state = State::new();
        assert_eq!(
            transpile_condition(
                (
                    &Condition::Not(Box::new(Condition::Expression((
                        Expr::Call(
                            "false",
                            (Vec::new(), dummy_span()),
                            (Vec::new(), dummy_span()),
                        ),
                        dummy_span()
                    ))),),
                    dummy_span()
                ),
                &mut state,
            ),
            Ok((String::from("! false "), None, Vec::new()))
        );
    }

    #[test]
    fn test_not_with_and() {
        let mut state = State::new();
        assert_eq!(
            transpile_condition(
                (
                    &Condition::Not(Box::new(Condition::InParens(Box::new(Condition::And(
                        Box::new(Condition::Expression((
                            Expr::Call(
                                "false",
                                (Vec::new(), dummy_span()),
                                (Vec::new(), dummy_span()),
                            ),
                            dummy_span()
                        ))),
                        Box::new(Condition::Expression((
                            Expr::Call(
                                "true",
                                (Vec::new(), dummy_span()),
                                (Vec::new(), dummy_span()),
                            ),
                            dummy_span()
                        )))
                    )))),),
                    dummy_span()
                ),
                &mut state,
            ),
            Ok((String::from("! (false  && true )"), None, Vec::new()))
        );
    }

    #[test]
    fn test_not_with_and_and_or() {
        let mut state = State::new();
        assert_eq!(
            transpile_condition(
                (
                    &Condition::Not(Box::new(Condition::Or(
                        Box::new(Condition::InParens(Box::new(Condition::And(
                            Box::new(Condition::Expression((
                                Expr::Call(
                                    "false",
                                    (Vec::new(), dummy_span()),
                                    (Vec::new(), dummy_span()),
                                ),
                                dummy_span()
                            ))),
                            Box::new(Condition::Expression((
                                Expr::Call(
                                    "true",
                                    (Vec::new(), dummy_span()),
                                    (Vec::new(), dummy_span()),
                                ),
                                dummy_span()
                            )))
                        )))),
                        Box::new(Condition::Expression((
                            Expr::Call(
                                "false",
                                (Vec::new(), dummy_span()),
                                (Vec::new(), dummy_span()),
                            ),
                            dummy_span()
                        )))
                    ),),),
                    dummy_span()
                ),
                &mut state,
            ),
            Ok((
                String::from("! (false  && true ) || false "),
                None,
                Vec::new()
            ))
        );
    }
}
