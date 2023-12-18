use chumsky::error::Rich;

use crate::{parser::Spanned, Expr, State};

#[derive(Clone, Debug)]
pub struct Intrinsic<'src, 'indiv_src> {
    pub name: &'src str,
    pub code_generator: fn(
        &Spanned<Vec<Spanned<Expr<'indiv_src>>>>,
    ) -> Result<(String, Option<String>), Rich<'indiv_src, char>>,
}

const INTRINSICS: [Intrinsic; 1] = [Intrinsic {
    name: "eval",
    code_generator: |_| Ok((String::new(), None)),
}];

pub fn call_intrinsic<'src>(
    name: &str,
    args: &Spanned<Vec<Spanned<Expr<'src>>>>,
    state: &mut State,
) -> Result<(String, Option<String>), Rich<'src, char>> {
    for intrinsic in &INTRINSICS {
        if name == intrinsic.name {
            let v = intrinsic.code_generator;
            return v(args);
        }
    }
    Err(Rich::custom(
        args.1,
        format!("Unable to find intrinsic {name}"),
    ))
}
