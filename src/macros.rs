use chumsky::error::Rich;

use crate::transpiler::transpile_repr;
use crate::State;
use crate::{parser::Spanned, Expr};

pub fn transpile_macro<'src>(
    macro_name: &'src str,
    args: Spanned<&'src Vec<Spanned<Expr<'src>>>>,
    state: &mut State,
) -> Result<(String, Option<String>), Rich<'src, char>> {
    match macro_name {
        "eval" => eval(args, state),
        "raw_name" => raw_name(args, state),
        m => Err(Rich::custom(args.1, format!("Unable to find macro {m}"))),
    }
}

fn eval<'src>(
    args: Spanned<&'src Vec<Spanned<Expr<'src>>>>,
    state: &mut State,
) -> Result<(String, Option<String>), Rich<'src, char>> {
    let mut contents = String::new();
    let mut run_before = String::new();
    for expr in args.0 {
        let (new_conts, new_run_before) = transpile_repr((&expr.0, expr.1), state)?;
        contents.push_str(&new_conts);
        contents.push(' ');
        if let Some(before) = new_run_before {
            run_before.push_str(&before);
        }
    }
    Ok((contents, run_before.into()))
}

fn raw_name<'src>(
    args: Spanned<&'src Vec<Spanned<Expr<'src>>>>,
    state: &mut State,
) -> Result<(String, Option<String>), Rich<'src, char>> {
    if args.0.len() != 1 {
        return Err(Rich::custom(
            args.1,
            format!("Expected 1 argument, found {}", args.0.len()),
        ));
    }
    if let (Expr::Var(variable), span) = &args.0[0] {
        if let Some((sh_variable_name, _type)) = state.get_var(&variable) {
            Ok((format!("\"${sh_variable_name}\""), None))
        } else {
            Err(Rich::custom(
                *span,
                format!("Could not find variable {}", variable),
            ))
        }
    } else {
        Err(Rich::custom(
            args.1,
            format!("Expected string literal found {:?}", args.0[0].0),
        ))
    }
}
