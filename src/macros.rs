use chumsky::error::Rich;

use crate::transpiler::{transpile_expr, transpile_repr};
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
        "unsafe_into" => unsafe_into(args, state),
        "format" => format(args, state),
        "print" => print(args, state),
        "stdout" => stdout(args, state),
        "is_successful_exit" => is_successful_exit(args, state),
        m => Err(Rich::custom(args.1, format!("Unable to find macro {m}"))),
    }
}

fn check_valid_args<'src>(args: Spanned<&'src Vec<Spanned<Expr<'src>>>>, intended_arg_num: usize) -> Result<(), Rich<'src, char>> {
    if args.0.len() != intended_arg_num {
        return Err(Rich::custom(
            args.1,
            format!("Expected 1 argument, found {}", args.0.len()),
        ));
    }
    Ok(())
}

fn eval<'src>(
    args: Spanned<&'src Vec<Spanned<Expr<'src>>>>,
    state: &mut State,
) -> Result<(String, Option<String>), Rich<'src, char>> {
    let mut contents = String::new();
    let mut run_before = String::new();
    for expr in args.0 {
        let (new_conts, new_run_before) = transpile_expr((&expr.0, expr.1), state)?;
        contents.push_str(&new_conts);
        contents.push(' ');
        if let Some(before) = new_run_before {
            run_before.push_str(&before);
        }
    }
    Ok((contents, run_before.into()))
}

fn unsafe_into<'src>(
    args: Spanned<&'src Vec<Spanned<Expr<'src>>>>,
    state: &mut State,
) -> Result<(String, Option<String>), Rich<'src, char>> {
    check_valid_args(args, 1)?;
    transpile_expr((&args.0[0].0, args.0[0].1), state)
}

fn stdout<'src>(
    args: Spanned<&'src Vec<Spanned<Expr<'src>>>>,
    state: &mut State,
) -> Result<(String, Option<String>), Rich<'src, char>> {
    check_valid_args(args, 1)?;
    let (output, run_before) = transpile_repr((&args.0[0].0, args.0[0].1), state)?;
    Ok((format!("$({output})"), run_before))
}

fn is_successful_exit<'src>(
    args: Spanned<&'src Vec<Spanned<Expr<'src>>>>,
    state: &mut State,
) -> Result<(String, Option<String>), Rich<'src, char>> {
    check_valid_args(args, 1)?;
    let (output, run_before) = transpile_repr((&args.0[0].0, args.0[0].1), state)?;
    let mut run_before = run_before.unwrap_or_default();
    run_before.push_str(&output);
    let var_name = format!("__tempv_{}", state.temp_vars_used);
    run_before.push_str(&format!(" 
if [ $? = 0 ]; then {var_name}=1; else {var_name}=0; fi
"));
    state.temp_vars_used += 1;
    Ok((format!("\"${var_name}\""), Some(run_before)))
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
        if let Some((sh_variable_name, _type)) = state.get_var(variable) {
            Ok((format!("\"${sh_variable_name}\""), None))
        } else {
            Err(Rich::custom(
                *span,
                format!("Could not find variable {variable}"),
            ))
        }
    } else {
        Err(Rich::custom(
            args.1,
            format!("Expected string literal found {:?}", args.0[0].0),
        ))
    }
}

fn print<'src>(
    args: Spanned<&'src Vec<Spanned<Expr<'src>>>>,
    state: &mut State,
) -> Result<(String, Option<String>), Rich<'src, char>> {
    let (to_print, run_before) = format(args, state)?;
    Ok((String::from("echo -e ") + &to_print, run_before))
}

fn format<'src>(
    args: Spanned<&'src Vec<Spanned<Expr<'src>>>>,
    state: &mut State,
) -> Result<(String, Option<String>), Rich<'src, char>> {
    if let Some(format_str) = args.0.first() {
        if let Expr::Str(s) = &format_str.0 {
            let mut chars = s.chars();

            let mut current_chars = Vec::new();
            let mut current_output = Vec::new();

            let mut run_before = String::new();

            let mut cur_num_put_in = 1;

            while let Some(c) = chars.next() {
                if c == '{' {
                    match chars.next() {
                        Some('}') => {
                            let normal_string = format!("'{}'", current_chars.iter().collect::<String>());
                            current_output.push(normal_string); 
                            if let Some(replacement) = args.0.get(cur_num_put_in) {
                                let (additional_output, extra_run_before) = transpile_expr((&replacement.0, replacement.1), state)?;
                                current_output.push(additional_output);
                                if let Some(before) = extra_run_before {
                                    run_before.push_str(&before);
                                    run_before.push('\n');
                                }
                            }
                            cur_num_put_in += 1;
                            current_chars = Vec::new();
                            continue;
                        },
                        Some('{') => (),
                        v => return Err(Rich::custom(
                format_str.1,
                format!("Expected ending bracket (or two brackets to escape formatting), but found {}", v.map_or_else(|| String::from("end of string"), |c| c.to_string())),
            )),
                    }
                }
                current_chars.push(c);
            }

            let normal_string = format!("'{}'", current_chars.iter().collect::<String>());
            current_output.push(normal_string);
            if cur_num_put_in == args.0.len() {
                Ok((current_output.join(""), run_before.into()))
            } else {
                Err(Rich::custom(
                    args.1,
                    format!("Expected {} arguments, found {}", cur_num_put_in, args.0.len()),
                ))
            }
        } else {
            Err(Rich::custom(
                format_str.1,
                String::from("Expected first argument of the format string to be a string literal"),
            ))
        }
    } else {
        Err(Rich::custom(
            args.1,
            format!("Expected at least 1 argument, found {}", args.0.len()),
        ))
    }
}
