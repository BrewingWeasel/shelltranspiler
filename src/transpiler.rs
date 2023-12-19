use chumsky::prelude::Rich;

use crate::{parser::Spanned, Function, State, Statement};

use self::statements::transpile_statement;

pub use expressions::{transpile_expr, transpile_repr};

mod condition;
mod expressions;
mod statements;

pub fn transpile_from_ast<'state, 'src: 'state>(
    conts: &'src Vec<Spanned<Statement<'src>>>,
    state: &mut State<'state>,
    main_transpiler: bool,
) -> Result<String, Rich<'src, char>> {
    // eprintln!("{:#?}", conts);
    let mut func_defs = String::new();
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

    if main_transpiler {
        let mut compile_function = |func: &Function| {
            if func.times_called > 0 {
                func_defs.push_str(&func.contents);
                func_defs.push('\n');
            }
        };
        for func in &state.prelude.functions {
            compile_function(func.1);
        }

        for func in &state
            .scopes
            .last()
            .expect("global scope should always exist")
            .functions
        {
            compile_function(func.1);
        }
    }

    Ok(func_defs + &compiled)
}
