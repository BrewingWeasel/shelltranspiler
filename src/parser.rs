use crate::Type;
use crate::{Condition, ContinueIfStatement, Expr, IfStatement, Statement};
use chumsky::extra::Err;
use chumsky::{prelude::*, Parser};

pub type Span = SimpleSpan<usize>;
pub type ParseErr<'src> = Err<Rich<'src, char, Span>>;

fn get_type<'src>() -> impl Parser<'src, &'src str, Type, ParseErr<'src>> + Clone {
    choice((
        text::keyword("string").to(Type::Str),
        text::keyword("int").to(Type::Num),
    ))
    .padded()
}

fn type_assignment<'src>() -> impl Parser<'src, &'src str, Option<Type>, ParseErr<'src>> + Clone {
    just(':').ignore_then(get_type()).or_not()
}

fn expression<'src>() -> impl Parser<'src, &'src str, Expr<'src>, ParseErr<'src>> + Clone {
    let ident = text::ident().padded();
    recursive(|expr| {
        let int = text::int(10)
            .map(|s: &str| Expr::Num(s.parse().unwrap()))
            .padded();

        let strvalue = any()
            .filter(|c: &char| *c != '"')
            .repeated()
            .collect()
            .map(|s: Vec<char>| Expr::Str(s.into_iter().collect()))
            .delimited_by(just('"'), just('"'))
            .padded();

        let var = ident.map(Expr::Var);

        let generic_call = ident.then(
            expr.clone()
                .separated_by(just(','))
                .allow_trailing()
                .collect::<Vec<_>>()
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

fn conditional<'src>() -> impl Parser<'src, &'src str, Condition<'src>, ParseErr<'src>> + Clone {
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

fn if_statement<'src>(
    statement: impl Parser<'src, &'src str, Statement<'src>, ParseErr<'src>> + Clone + 'src,
) -> impl Parser<'src, &'src str, Statement<'src>, ParseErr<'src>> + Clone {
    recursive(|if_statement| {
        text::keyword("if")
            .padded()
            .ignore_then(conditional())
            .padded()
            .then_ignore(just('{'))
            .then(statement.clone().repeated().collect::<Vec<_>>())
            .then_ignore(just('}'))
            .then(
                text::keyword("else")
                    .padded()
                    .then(
                        if_statement.map(ContinueIfStatement::If).or(just('{')
                            .ignore_then(statement.clone().repeated().collect())
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

pub fn parser<'src>() -> impl Parser<'src, &'src str, Vec<Statement<'src>>, ParseErr<'src>> {
    let ident = text::ident().padded();

    let statement = recursive(|statement| {
        let expr = expression();
        let assignment = ident
            .then(type_assignment())
            .then_ignore(just('='))
            .then(expr.clone());

        let comment = just('#')
            .then_ignore(any().filter(|c| *c != '\n').repeated())
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
                    .allow_trailing()
                    .collect::<Vec<_>>(),
            )
            .then_ignore(just(')'))
            .padded()
            .then(just("->").padded().ignore_then(get_type()).or_not())
            .then_ignore(just('{'))
            .then(statement.repeated().collect::<Vec<_>>())
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

    statement.repeated().collect().then_ignore(end())
}
