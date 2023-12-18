use crate::Expr;
use chumsky::{prelude::*, Parser};

use super::{ParseErr, Spanned};

pub fn expression<'src>(
) -> impl Parser<'src, &'src str, Spanned<Expr<'src>>, ParseErr<'src>> + Clone {
    let ident = text::ident().padded();
    recursive(|expr| {
        let int = just('-')
            .or_not()
            .then(text::int(10))
            .map(|(negative, s): (Option<_>, &str)| {
                Expr::Num(s.parse::<i64>().unwrap() * (if negative.is_some() { -1 } else { 1 }))
            })
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
                .map_with(|args, e| (args, e.span()))
                .then(
                    just('|')
                        .padded()
                        .ignore_then(
                            ident
                                .then_ignore(just('='))
                                .then(expr.clone())
                                .separated_by(just(','))
                                .allow_trailing()
                                .collect::<Vec<_>>(),
                        )
                        .or_not()
                        .map_with(|kwargs, e| (kwargs.unwrap_or_default(), e.span())),
                )
                .delimited_by(just('('), just(')')),
        );

        let macro_call = just('@')
            .ignore_then(ident)
            .then(
                expr.clone()
                    .separated_by(just(','))
                    .allow_trailing()
                    .collect::<Vec<_>>()
                    .map_with(|args, e| (args, e.span()))
                    .delimited_by(just('('), just(')')),
            )
            .padded()
            .map(|(f, args)| Expr::Macro(f, args));

        let call_piped = just('<')
            .ignore_then(generic_call.clone())
            .padded()
            .map(|(f, (args, kwargs))| Expr::CallPiped(f, args, kwargs));

        let call = generic_call.map(|(f, (args, kwargs))| Expr::Call(f, args, kwargs));

        recursive(|part| {
            let atom = choice((
                int,
                expr.clone()
                    .map(|(expr, _)| expr)
                    .delimited_by(just('('), just(')')),
                expr.clone()
                    .separated_by(just(','))
                    .allow_trailing()
                    .collect::<Vec<_>>()
                    .delimited_by(just('['), just(']'))
                    .map_with(|values, e| Expr::List((values, e.span())))
                    // jasdlkfjasdklj
                    .padded(),
                ident
                    .then(expr.delimited_by(just('['), just(']')))
                    .map(|(ident, expr)| Expr::ListIndex(ident, Box::new(expr))),
                strvalue,
                macro_call,
                call_piped,
                call,
                var,
            ))
            .map_with(|expr: Expr, e| -> Spanned<Expr> { (expr, e.span()) });

            atom.then(
                choice((
                    just("|>"),
                    just("+"),
                    just("**"),
                    just("-"),
                    just("/"),
                    just("*"),
                    just("%"),
                ))
                .padded()
                .then(part)
                .or_not(),
            )
            .map(
                |(first, second): (Spanned<Expr>, Option<(&str, Spanned<Expr>)>)| {
                    if let Some((operation, second_expr)) = second {
                        (
                            match operation {
                                "|>" => Expr::Pipe(
                                    Box::new(first.clone()),
                                    Box::new(second_expr.clone()),
                                ),
                                c => Expr::Operation(
                                    c,
                                    Box::new(first.clone()),
                                    Box::new(second_expr.clone()),
                                ),
                            },
                            first.1.union(second_expr.1),
                        )
                    } else {
                        first
                    }
                },
            )
        })
    })
}
