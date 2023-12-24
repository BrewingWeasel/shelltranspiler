use crate::Statement;
use crate::Type;
use chumsky::extra::Err;
use chumsky::{prelude::*, Parser};

use self::statement::parse_statement;

pub type Span = SimpleSpan<usize>;
pub type Spanned<T> = (T, Span);
pub type ParseErr<'src> = Err<Rich<'src, char, Span>>;

mod conditional;
mod expr;
mod statement;

fn get_type<'src>() -> impl Parser<'src, &'src str, Type, ParseErr<'src>> + Clone {
    recursive(|type_name| {
        choice((
            text::keyword("string").to(Type::Str),
            text::keyword("int").to(Type::Num),
            text::keyword("bool").to(Type::Bool),
            just('%')
                .ignore_then(text::ident())
                .map(|v: &str| Type::Generic(v.to_string())),
            type_name
                .delimited_by(just('['), just(']'))
                .map(|t| Type::List(Box::new(t))),
        ))
    })
    .padded()
    .boxed()
}

fn type_assignment<'src>() -> impl Parser<'src, &'src str, Option<Type>, ParseErr<'src>> + Clone {
    just(':')
        .ignore_then(get_type())
        .or_not()
        .labelled("type assignment")
}

pub fn parser<'src>(
) -> impl Parser<'src, &'src str, Vec<Spanned<Statement<'src>>>, ParseErr<'src>> + Clone {
    parse_statement()
        .map_with(|s, e| (s, e.span()))
        .repeated()
        .collect::<Vec<_>>()
        .then_ignore(end())
}
