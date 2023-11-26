use chumsky::prelude::Rich;

use crate::{parser::Spanned, State, Statement};

use self::statements::transpile_statement;

mod condition;
mod expressions;
mod statements;

pub fn transpile_from_ast<'state, 'src: 'state>(
    conts: &'src Vec<Spanned<Statement<'src>>>,
    state: &mut State<'state>,
) -> Result<String, Rich<'src, char>> {
    let mut compiled = String::new();
    for expr in conts {
        let (output, run_before) = transpile_statement(expr, state)?;
        if let Some(run) = run_before {
            compiled.push_str(&run);
            compiled.push('\n');
        }
        compiled.push_str(&output);
        compiled.push('\n');
    }
    Ok(compiled)
}
