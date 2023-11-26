use crate::Expr;
use chumsky::{prelude::*, Parser};

use super::{ParseErr, Spanned};

pub fn expression<'src>(
) -> impl Parser<'src, &'src str, Spanned<Expr<'src>>, ParseErr<'src>> + Clone {
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
                call_piped,
                call,
                var,
            ))
            .map_with(|expr: Expr, e| -> Spanned<Expr> { (expr, e.span()) });

            atom.then(just("|>").padded().ignore_then(part).or_not())
                .map(
                    |(first, into_piped): (Spanned<Expr>, Option<Spanned<Expr>>)| {
                        if let Some(second) = into_piped {
                            (
                                Expr::Pipe(Box::new(first.clone()), Box::new(second.clone())),
                                first.1.union(second.1),
                            )
                        } else {
                            first
                        }
                    },
                )
        })
    })
}
