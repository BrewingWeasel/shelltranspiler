use crate::Kwarg;
use crate::{ContinueIfStatement, IfStatement, Statement};
use chumsky::{prelude::*, Parser};

use super::conditional::conditional;
use super::expr::expression;
use super::{get_type, type_assignment, ParseErr};

fn if_statement<'src>(
    statement: impl Parser<'src, &'src str, Statement<'src>, ParseErr<'src>> + Clone + 'src,
) -> impl Parser<'src, &'src str, Statement<'src>, ParseErr<'src>> + Clone {
    recursive(|if_statement| {
        text::keyword("if")
            .padded()
            .ignore_then(conditional().map_with(|cond, e| (cond, e.span())))
            .padded()
            .then_ignore(just('{'))
            .then(
                statement
                    .clone()
                    .map_with(|s, e| (s, e.span()))
                    .repeated()
                    .collect::<Vec<_>>(),
            )
            .then_ignore(just('}'))
            .then(
                text::keyword("else")
                    .padded()
                    .then(
                        if_statement.map(ContinueIfStatement::If).or(just('{')
                            .ignore_then(
                                statement
                                    .clone()
                                    .map_with(|s, e| (s, e.span()))
                                    .repeated()
                                    .collect(),
                            )
                            .then_ignore(just('}'))
                            .map(ContinueIfStatement::Else)),
                    )
                    .or_not(),
            )
            .map(|((cond, body), to_continue)| IfStatement {
                cond,
                statements: body,
                continue_if: Box::new(to_continue.map(|(_, v)| v)),
            })
            .map_with(|if_statement, e| (if_statement, e.span()))
    })
    .map(|(if_statement, span)| Statement::If((if_statement, span)))
}

pub fn parse_statement<'src>(
) -> impl Parser<'src, &'src str, Statement<'src>, ParseErr<'src>> + Clone {
    let ident = text::ident().padded();

    recursive(|statement| {
        let expr = expression();

        let body = statement
            .clone()
            .map_with(|s, e| (s, e.span()))
            .repeated()
            .collect::<Vec<_>>()
            .map_with(|body, e| (body, e.span()))
            .delimited_by(just('{'), just('}'))
            .padded();

        let assignment = text::keyword("var")
            .or_not()
            .map(|v| v.is_some())
            .then(ident)
            .then(type_assignment())
            .then_ignore(just('='))
            .then(expr.clone())
            .labelled("variable assignment");

        let comment = just('#')
            .then_ignore(any().filter(|c| *c != '\n').repeated())
            .then_ignore(text::newline());

        let local_assignment = text::keyword("local")
            .padded()
            .ignore_then(assignment.clone())
            .map(|(((first_assignment, id), var_type), val)| {
                Statement::LocalAssignment(first_assignment, id, var_type, val)
            });

        let global_assignment = assignment.map(|(((first_assignment, id), var_type), val)| {
            Statement::Assignment(first_assignment, id, var_type, val)
        });

        let if_statement = if_statement(statement.clone());

        let for_loop = text::keyword("for")
            .padded()
            .ignore_then(ident)
            .padded()
            .then_ignore(text::keyword("in"))
            .padded()
            .then(expr.clone())
            .then(body.clone())
            .labelled("for loop")
            .map(|((var, loop_list), body)| Statement::For(var, loop_list, body));

        let while_loop = text::keyword("while")
            .padded()
            .ignore_then(conditional().map_with(|cond, e| (cond, e.span())))
            .then(body.clone())
            .labelled("while loop")
            .map(|(condition, body)| Statement::While(condition, body));

        let return_statement = text::keyword("return")
            .padded()
            .ignore_then(expr.clone())
            .map(Statement::Return)
            .labelled("return statement");

        let function = text::keyword("fun")
            .ignore_then(ident)
            .then_ignore(just('('))
            .then(
                ident
                    .then(type_assignment())
                    .labelled("type")
                    .separated_by(just(','))
                    .allow_trailing()
                    .collect::<Vec<_>>()
                    .labelled("arguments"),
            )
            .then(
                just('|')
                    .padded()
                    .ignore_then(
                        ident
                            .then(type_assignment())
                            .padded()
                            .labelled("type")
                            .then_ignore(just('='))
                            .padded()
                            .then(expr.clone())
                            .map(|((ident, kwarg_type), default)| Kwarg {
                                ident,
                                kwarg_type,
                                default,
                            })
                            .separated_by(just(','))
                            .allow_trailing()
                            .collect::<Vec<_>>()
                            .labelled("optional arguments"),
                    )
                    .or_not()
                    .map(std::option::Option::unwrap_or_default),
            )
            .then_ignore(just(')'))
            .padded()
            .then(just("->").padded().ignore_then(get_type()).or_not())
            .then(body)
            .map(|((((id, args), kwargs), return_type), body)| {
                Statement::Function(id, args, kwargs, return_type, body)
            })
            .labelled("function");

        choice((
            function,
            if_statement,
            return_statement,
            for_loop,
            while_loop,
            local_assignment,
            global_assignment,
            expr.map(Statement::Expression),
            comment.to(Statement::Empty),
        ))
        .then_ignore(comment.or_not())
        .then_ignore(just(';').ignored().or(text::newline()).or_not())
        .padded()
    })
}
