use crate::Condition;
use chumsky::{prelude::*, Parser};

use super::expr::expression;
use super::ParseErr;

pub fn conditional<'src>() -> impl Parser<'src, &'src str, Condition<'src>, ParseErr<'src>> + Clone
{
    let expr = expression();
    recursive(|conditional| {
        let not = just("not")
            .padded()
            .ignore_then(conditional.clone())
            .map(|cond| Condition::Not(Box::new(cond)));

        let operator = |op| {
            expr.clone()
                .padded()
                .then_ignore(just(op))
                .padded()
                .then(expr.clone())
                .map(move |(val1, val2)| Condition::Operator(op, val1, val2))
        };

        let condition = choice((
            not,
            operator("=="),
            operator("!="),
            operator(">="),
            operator(">"),
            operator("<="),
            operator("<"),
            conditional
                .clone()
                .padded()
                .delimited_by(just('('), just(')'))
                .padded()
                .map(|cond| Condition::InParens(Box::new(cond))),
            expr.map(Condition::Expression),
        ));

        condition
            .clone()
            .padded()
            .then(choice((just("||"), just("&&"))).then(condition).or_not())
            .map(|(condition1, potential_condition)| {
                if let Some((operator, condition2)) = potential_condition {
                    if operator == "&&" {
                        Condition::And(Box::new(condition1), Box::new(condition2))
                    } else {
                        Condition::Or(Box::new(condition1), Box::new(condition2))
                    }
                } else {
                    condition1
                }
            })
            .labelled("condition")
    })
}

#[cfg(test)]
mod tests {

    use crate::Expr;

    use super::*;
    use pretty_assertions::assert_eq;

    #[test]
    fn test_basic_equals() {
        let parsed = conditional().parse("46 == 46").unwrap();
        assert_eq!(
            parsed,
            Condition::Operator(
                "==",
                (crate::Expr::Num(46), SimpleSpan::new(0, 3)),
                (crate::Expr::Num(46), SimpleSpan::new(6, 8))
            )
        );
    }

    #[test]
    fn test_greater_than_equals_complicated_expr() {
        let parsed = conditional()
            .parse("run_func(231, 1234, 21) >= 46")
            .unwrap();
        assert_eq!(
            parsed,
            Condition::Operator(
                ">=",
                (
                    crate::Expr::Call(
                        "run_func",
                        (
                            vec![
                                (Expr::Num(231), SimpleSpan::new(9, 12)),
                                (Expr::Num(1234), SimpleSpan::new(13, 18)),
                                (Expr::Num(21), SimpleSpan::new(19, 22))
                            ],
                            SimpleSpan::new(9, 22)
                        ),
                        (vec![], SimpleSpan::new(22, 22)),
                    ),
                    SimpleSpan::new(0, 23)
                ),
                (crate::Expr::Num(46), SimpleSpan::new(27, 29))
            )
        );
    }

    #[test]
    fn test_not_then_greater_than_equals_complicated_expr() {
        let parsed = conditional()
            .parse("not run_func(231, 1234, 21) >= 46")
            .unwrap();
        if let Condition::Not(cond) = parsed {
            assert!(matches!(
                *cond,
                Condition::Operator(">=", _, (crate::Expr::Num(46), _))
            ));
        } else {
            panic!("Condition should have been not")
        }
    }

    #[test]
    fn test_simple_expr() {
        let parsed = conditional().parse("do_thing()").unwrap();
        assert_eq!(
            parsed,
            Condition::Expression((
                crate::Expr::Call(
                    "do_thing",
                    (vec![], SimpleSpan::new(9, 9)),
                    (vec![], SimpleSpan::new(9, 9)),
                ),
                SimpleSpan::new(0, 10)
            ))
        );
    }

    #[test]
    fn test_simple_and() {
        let parsed = conditional().parse("x == 5 && do_thing()").unwrap();
        if let Condition::And(cond1, cond2) = parsed {
            assert!(matches!(
                *cond1,
                Condition::Operator("==", (crate::Expr::Var("x"), _), (crate::Expr::Num(5), _))
            ));
            assert!(matches!(
                *cond2,
                Condition::Expression((crate::Expr::Call("do_thing", _, _), _))
            ));
        } else {
            panic!("Condition should have been and")
        }
    }

    #[test]
    fn test_two_ands_joined_by_and() {
        let parsed = conditional()
            .parse("(x < 5 && do_thing()) && (do_other_thing() && do_other_thing2())")
            .unwrap();

        let mut got_to_correct_condition = (false, false);
        if let Condition::And(cond1, cond2) = parsed {
            if let Condition::InParens(cond1_and) = *cond1 {
                if let Condition::And(cond1_cond1, cond1_cond2) = *cond1_and {
                    assert!(matches!(
                        *cond1_cond1,
                        Condition::Operator(
                            "<",
                            (crate::Expr::Var("x"), _),
                            (crate::Expr::Num(5), _)
                        )
                    ));
                    assert!(matches!(
                        *cond1_cond2,
                        Condition::Expression((crate::Expr::Call("do_thing", _, _), _))
                    ));
                }
                got_to_correct_condition.0 = true;
            }
            if let Condition::InParens(cond2_and) = *cond2 {
                if let Condition::And(cond2_cond1, cond2_cond2) = *cond2_and {
                    assert!(matches!(
                        *cond2_cond1,
                        Condition::Expression((crate::Expr::Call("do_other_thing", _, _), _))
                    ));
                    assert!(matches!(
                        *cond2_cond2,
                        Condition::Expression((crate::Expr::Call("do_other_thing2", _, _), _))
                    ));
                    got_to_correct_condition.1 = true;
                }
            }
        }
        assert!(!(!got_to_correct_condition.0 || !got_to_correct_condition.1), "Did not reach all expected conditions");
    }
}
