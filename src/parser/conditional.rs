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
