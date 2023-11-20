use crate::{Condition, ContinueIfStatement, Expr, IfStatement, Statement};
use chumsky::prelude::*;
use chumsky::Parser;

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

        let call = ident
            .then(
                expr.clone()
                    .separated_by(just(','))
                    .allow_trailing()
                    .delimited_by(just('('), just(')')),
            )
            .map(|(f, args)| Expr::Call(f, args));

        recursive(|part| {
            let atom = int
                .or(expr.delimited_by(just('('), just(')')))
                .or(strvalue)
                .or(call)
                .or(var);
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

fn conditional() -> impl Parser<char, Condition, Error = Simple<char>> + Clone {
    let expr = expression();
    recursive(|conditional| {
        let not = just("not")
            .padded()
            .ignore_then(conditional.clone())
            .map(|cond| Condition::Not(Box::new(cond)));

        let is_equal = expr
            .clone()
            .padded()
            .then_ignore(just("=="))
            .padded()
            .then(expr.clone())
            .map(|(val1, val2)| Condition::Equal(val1, val2));

        let condition = not.or(is_equal).or(expr.map(Condition::Expression));

        condition
            .clone()
            .padded()
            .then(just("&&").ignore_then(condition).or_not())
            .map(|(condition1, potential_condition)| {
                if let Some(condition2) = potential_condition {
                    Condition::And(Box::new(condition1), Box::new(condition2))
                } else {
                    condition1
                }
            })
    })
}

fn if_statement<'a>(
    statement: impl Parser<char, Statement, Error = Simple<char>> + Clone + 'a,
) -> impl Parser<char, Statement, Error = Simple<char>> + Clone + 'a {
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

pub fn parser() -> impl Parser<char, Vec<Statement>, Error = Simple<char>> {
    let ident = text::ident().padded();

    let statement = recursive(|statement| {
        let expr = expression();
        let assignment = ident.then_ignore(just('=')).then(expr.clone());

        let comment = just::<_, _, Simple<char>>('#')
            .then_ignore(filter(|c| *c != '\n').repeated())
            .then_ignore(text::newline());

        let local_assignment = text::keyword("local")
            .padded()
            .ignore_then(assignment.clone())
            .map(|(id, val)| Statement::LocalAssignment(id, val));

        let global_assignment = assignment.map(|(id, val)| Statement::Assignment(id, val));

        let if_statement = if_statement(statement.clone());

        let function = text::keyword("fun")
            .ignore_then(ident)
            .then_ignore(just('('))
            .then(ident.repeated().separated_by(just(',')).allow_trailing())
            .then_ignore(just(')'))
            .padded()
            .then_ignore(just('{'))
            .then(statement.repeated())
            .then_ignore(just('}'))
            .map(|((id, args), body)| Statement::Function(id, args, body));

        local_assignment
            .or(global_assignment)
            .or(function)
            .or(if_statement)
            .or(expr.map(Statement::Expression))
            .or(comment.to(Statement::Empty))
            .then_ignore(comment.or_not())
            .then_ignore(just(';').ignored().or(text::newline()).or_not())
            .padded()
    });

    statement.repeated().then_ignore(end())
}