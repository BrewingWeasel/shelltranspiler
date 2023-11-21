use crate::Type;
use crate::{Condition, ContinueIfStatement, Expr, IfStatement, Statement};
use chumsky::prelude::*;
use chumsky::Parser;

fn get_type() -> impl Parser<char, Type, Error = Simple<char>> + Clone {
    choice((
        text::keyword("string").to(Type::Str),
        text::keyword("int").to(Type::Num),
    ))
    .padded()
}

fn type_assignment() -> impl Parser<char, Option<Type>, Error = Simple<char>> + Clone {
    just(':').ignore_then(get_type()).or_not()
}

fn expression() -> impl Parser<char, Expr, Error = Simple<char>> + Clone {
    let ident = text::ident().padded();
    recursive(|expr| {
        let int = text::int(10)
            .map(|s: String| Expr::Num(s.parse().unwrap()))
            .padded();

        let strvalue = filter::<_, _, Simple<char>>(|c: &char| *c != '"')
            .repeated()
            .map(|s: Vec<char>| Expr::Str(s.into_iter().collect()))
            .delimited_by(just('"'), just('"'))
            .padded();

        let var = ident.map(Expr::Var);

        let generic_call = ident.then(
            expr.clone()
                .separated_by(just(','))
                .allow_trailing()
                .delimited_by(just('('), just(')')),
        );

        let call_piped = just('<')
            .ignore_then(generic_call.clone())
            .padded()
            .map(|(f, args)| Expr::CallPiped(f, args));
        let call = generic_call.map(|(f, args)| Expr::Call(f, args));

        recursive(|part| {
            let atom = choice((
                int,
                expr.delimited_by(just('('), just(')')),
                strvalue,
                call_piped,
                call,
                var,
            ));
            atom.then(just("|>").padded().ignore_then(part).or_not())
                .map(|(first, into_piped)| {
                    if let Some(second) = into_piped {
                        Expr::Pipe(Box::new(first), Box::new(second))
                    } else {
                        first
                    }
                })
        })
    })
}

fn conditional<'a>() -> impl Parser<char, Condition<'a>, Error = Simple<char>> + Clone {
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
    })
}

fn if_statement<'a>(
    statement: impl Parser<char, Statement<'a>, Error = Simple<char>> + Clone + 'a,
) -> impl Parser<char, Statement<'a>, Error = Simple<char>> + Clone + 'a {
    recursive(|if_statement| {
        text::keyword("if")
            .padded()
            .ignore_then(conditional())
            .padded()
            .then_ignore(just('{'))
            .then(statement.clone().repeated())
            .then_ignore(just('}'))
            .then(
                text::keyword("else")
                    .padded()
                    .then(
                        if_statement.map(ContinueIfStatement::If).or(just('{')
                            .ignore_then(statement.clone().repeated())
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
    })
    .map(Statement::If)
}

pub fn parser<'a>() -> impl Parser<char, Vec<Statement<'a>>, Error = Simple<char>> {
    let ident = text::ident().padded();

    let statement = recursive(|statement| {
        let expr = expression();
        let assignment = ident
            .then(type_assignment())
            .then_ignore(just('='))
            .then(expr.clone());

        let comment = just::<_, _, Simple<char>>('#')
            .then_ignore(filter(|c| *c != '\n').repeated())
            .then_ignore(text::newline());

        let local_assignment = text::keyword("local")
            .padded()
            .ignore_then(assignment.clone())
            .map(|((id, var_type), val)| Statement::LocalAssignment(id, var_type, val));

        let global_assignment =
            assignment.map(|((id, var_type), val)| Statement::Assignment(id, var_type, val));

        let if_statement = if_statement(statement.clone());

        let return_statement = text::keyword("return")
            .padded()
            .ignore_then(expr.clone())
            .map(Statement::Return);

        let function = text::keyword("fun")
            .ignore_then(ident)
            .then_ignore(just('('))
            .then(
                ident
                    .then(type_assignment())
                    .separated_by(just(','))
                    .allow_trailing(),
            )
            .then_ignore(just(')'))
            .padded()
            .then(just("->").padded().ignore_then(get_type()).or_not())
            .then_ignore(just('{'))
            .then(statement.repeated())
            .then_ignore(just('}'))
            .map(|(((id, args), return_type), body)| {
                Statement::Function(id, args, return_type, body)
            });

        local_assignment
            .or(global_assignment)
            .or(function)
            .or(if_statement)
            .or(return_statement)
            .or(expr.map(Statement::Expression))
            .or(comment.to(Statement::Empty))
            .then_ignore(comment.or_not())
            .then_ignore(just(';').ignored().or(text::newline()).or_not())
            .padded()
    });

    statement.repeated().then_ignore(end())
}
