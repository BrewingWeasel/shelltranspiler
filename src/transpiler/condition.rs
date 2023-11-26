use crate::{parser::Spanned, Condition, State};
use chumsky::prelude::Rich;

use super::expressions::{transpile_expr, transpile_repr};

pub fn transpile_condition<'src>(
    condition: Spanned<&'src Condition>,
    state: &mut State,
) -> Result<(String, Option<String>), Rich<'src, char>> {
    match condition.0 {
        Condition::Expression(expr) => transpile_repr((&expr.0, condition.1), state),
        Condition::Operator(op, expr1, expr2) => {
            let mut output = String::from("[ ");
            let (new_output, mut run_before) = transpile_expr((&expr1.0, condition.1), state)?;
            output.push_str(&new_output);

            output.push(' ');
            let shell_op = match *op {
                "==" => "=",
                "!=" => "!=",
                ">=" => "-ge",
                ">" => "-gt",
                "<=" => "-le",
                "<" => "-lt",
                _ => unreachable!(),
            };
            output.push_str(shell_op);
            output.push(' ');

            let (second_output, second_run_before) =
                transpile_expr((&expr2.0, condition.1), state)?;
            if let Some(run) = second_run_before {
                run_before = Some(run_before.unwrap_or_default() + &run);
            }

            output.push_str(&second_output);
            output.push_str(" ]");
            Ok((output, run_before))
        }
        Condition::Not(cond) => {
            let mut output = String::from("not ");
            let (new_output, run_before) = transpile_condition((cond, condition.1), state)?;
            output.push_str(&new_output);
            Ok((output, run_before))
        }
        Condition::InParens(cond) => {
            let mut output = String::from("(");
            let (new_output, run_before) = transpile_condition((cond, condition.1), state)?;
            output.push_str(&new_output);
            output.push(')');
            Ok((output, run_before))
        }
        Condition::And(cond1, cond2) => {
            let (mut output, mut run_before) = transpile_condition((cond1, condition.1), state)?;
            output.push_str(" && ");

            let (second_output, second_run_before) =
                transpile_condition((cond2, condition.1), state)?;
            if let Some(run) = second_run_before {
                run_before = Some(run_before.unwrap_or_default() + &run);
            }
            output.push_str(&second_output);
            Ok((output, run_before))
        }
        Condition::Or(cond1, cond2) => {
            let (mut output, mut run_before) = transpile_condition((cond1, condition.1), state)?;
            output.push_str(" || ");
            let (second_output, second_run_before) =
                transpile_condition((cond2, condition.1), state)?;
            if let Some(run) = second_run_before {
                run_before = Some(run_before.unwrap_or_default() + &run);
            }
            output.push_str(&second_output);
            Ok((output, run_before))
        }
    }
}
