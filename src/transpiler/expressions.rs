use std::collections::HashMap;

use crate::{get_generic_by_path, macros::transpile_macro, parser::Spanned, Expr, State, Type};
use chumsky::{
    container::Seq,
    prelude::Rich,
    span::{SimpleSpan, Span},
};

pub fn transpile_expr<'a>(
    expr: Spanned<&'a Expr>,
    state: &mut State,
) -> Result<(String, Option<String>), Rich<'a, char>> {
    match expr.0 {
        Expr::Num(x) => Ok((format!("{x}"), None)), // HACK: should have quotes but that breaks
        // indexing
        Expr::Str(s) => Ok((format!("'{s}'"), None)),
        Expr::List(elements) => {
            let list_pointer = state.new_list_pointer();
            let (mut output, mut add_before) = (String::new(), String::new());
            for (i, elem) in elements.0.iter().enumerate() {
                let (new_output, new_add_before) = transpile_expr((&elem.0, elem.1), state)?;
                if let Some(before) = new_add_before {
                    add_before.push_str(&before);
                    add_before.push('\n');
                }
                output.push_str(&format!("{list_pointer}_{i}={new_output}\n"));
                // TODO: type checking
            }
            add_before.push_str(&format!("{list_pointer}_len={}\n", elements.0.len()));
            add_before.push_str(&output);
            Ok((format!("\"{list_pointer}\""), Some(add_before)))
        }
        Expr::ListIndex(list, index) => {
            let (output, run_before) = transpile_expr((&index.0, index.1), state)?;
            if let Some((sh_variable_name, _type)) = state.get_var(list) {
                let actual_type = index.0.get_type(state);
                if actual_type != Type::Num {
                    return Err(Rich::custom(
                        index.1,
                        format!("Only int works for indexing list, found {actual_type}"),
                    ));
                }

                Ok((
                    format!(
                        r#""$(eval "echo \"\$$(echo "${sh_variable_name}")_$(echo "{output}")\"")""#
                    ),
                    run_before,
                ))
            } else {
                Err(Rich::custom(expr.1, format!("Could not find list {list}")))
            }
        }
        Expr::Var(s) => {
            if let Some((sh_variable_name, _type)) = state.get_var(s) {
                Ok((format!("\"${sh_variable_name}\""), None))
            } else {
                Err(Rich::custom(expr.1, format!("Could not find variable {s}")))
            }
        }
        Expr::Call(func, args, kwargs) => {
            if let Some(f) = state.get_func(func) {
                if f.return_value.is_none() {
                    return Err(Rich::custom(
                        expr.1,
                        format!("Function {func} does not have a return value. Maybe you meant to call it piped? (<{func})"))
                    );
                }
            }
            // else {
            //     return Err(Rich::custom(
            //         expr.1,
            //         format!("Unable to find function {func}"),
            //     ));
            // }
            let var_name = format!("$__{func}_return_value_{}", state.get_times_called(func));
            Ok((
                var_name,
                Some(call_function(
                    func,
                    (&args.0, args.1),
                    (&kwargs.0, kwargs.1),
                    state,
                )?),
            ))
        }
        Expr::CallPiped(f, args, kwargs) => {
            let mut output = String::from("$(");
            output.push_str(&call_function(
                f,
                (&args.0, args.1),
                (&kwargs.0, kwargs.1),
                state,
            )?);
            output.push(')');
            Ok((output, None))
        }
        Expr::Pipe(first, second) => {
            let mut output = String::from("$(");
            let (new_output, run_before) = transpile_repr((&first.0, first.1), state)?;
            output.push_str(&new_output);
            output.push_str(" | ");

            let (second_output, second_run_before) = transpile_repr((&second.0, second.1), state)?;
            if let Some(run) = second_run_before {
                run_before.unwrap_or_default().push_str(&run);
            }
            output.push_str(&second_output);
            output.push(')');
            Ok((output, None))
        }
        Expr::Operation(op, first, second) => match *op {
            "+" => run_operation(
                first,
                second,
                HashMap::from([(Type::Num, "$(({1} + {2}))"), (Type::Str, "{1}{2}")]),
                state,
            ),
            op if ["-", "*", "/", "%", "**"].contains(&op) => run_operation(
                first,
                second,
                HashMap::from([(Type::Num, format!("$(({{1}} {op} {{2}}))").as_str())]),
                state,
            ),
            _ => unreachable!(),
        },
        Expr::Macro(macro_name, args) => transpile_macro(macro_name, (&args.0, args.1), state),
    }
}

fn run_operation<'a>(
    first: &'a Spanned<Expr<'a>>,
    second: &'a Spanned<Expr<'a>>,
    operations: HashMap<Type, &str>,
    state: &mut State,
) -> Result<(String, Option<String>), Rich<'a, char>> {
    let type1 = first.0.get_type(state);
    let type2 = second.0.get_type(state);
    if type1 != type2 {
        return Err(Rich::custom(
            first.1.union(second.1),
            format!("Types being added do not match (found {type1} and {type2})"),
        ));
    }
    let (first_expr, run_before) = transpile_expr((&first.0, first.1), state)?;
    let mut run_before = run_before.unwrap_or_default();
    let (second_expr, new_run_before) = transpile_expr((&second.0, second.1), state)?;
    if let Some(run) = new_run_before {
        run_before.push_str(&run);
    }
    if let Some(format) = operations.get(&type1) {
        let output = format
            .replace("{1}", &first_expr)
            .replace("{2}", &second_expr);
        Ok((output, Some(run_before)))
    } else {
        Err(Rich::custom(
            first.1.union(second.1),
            format!(
                "Expected type to be one of {:?} but found {}",
                operations.keys(),
                type1
            ),
        ))
    }
}

fn call_function<'a>(
    f: &str,
    args: Spanned<&'a Vec<Spanned<Expr>>>,
    kwargs: Spanned<&'a Vec<(&'a str, Spanned<Expr>)>>,
    state: &mut State,
) -> Result<String, Rich<'a, char>> {
    let (args, args_span) = args;
    let (kwargs, _kwargs_span) = kwargs;
    let mut output = String::new();

    let check_used_generic = |expected_type: &Type,
                              other_type: &Type,
                              generic_types_map: &mut HashMap<String, Type>,
                              span: &SimpleSpan| {
        if let Some((v, path_to_generic)) = expected_type.get_generic_var() {
            let other_type = get_generic_by_path(&path_to_generic, other_type.clone());
            if let Some(previous_type) = generic_types_map.get(v) {
                if !other_type.matches(previous_type) {
                    return Err(Rich::custom(
                        *span,
                        format!(
                            "Expected {other_type} to match the type of previous type of generic variable {v} ({previous_type})"
                        ),
                    ));
                }
            } else {
                generic_types_map.insert(v.to_owned(), other_type);
            }
        }
        Ok(())
    };

    if let Some(func) = state.get_func(f) {
        if args.len() != func.args.len() {
            let span = match args.len() {
                0 => args_span,
                1 => args.first().unwrap().1,
                _ => args.first().unwrap().1.union(args.last().unwrap().1),
            };
            return Err(Rich::custom(
                span,
                format!(
                    "{f} expected {} arguments, but got {}",
                    func.args.len(),
                    args.len()
                ),
            ));
        }
        let mut generic_types_map = HashMap::new();
        for (kwarg_ident, (expr, span)) in kwargs {
            let mut known_kwarg = false;
            for real_kwarg in &func.kwargs {
                if &real_kwarg.ident == kwarg_ident {
                    if let Some(expected_type) = &real_kwarg.kwarg_type {
                        let other_type = &expr.get_type(state);
                        if !expected_type.matches(other_type) {
                            return Err(Rich::custom(
                                *span,
                                format!(
                                    "Expected {other_type} to match the type of {expected_type}"
                                ),
                            ));
                        }
                        check_used_generic(
                            expected_type,
                            other_type,
                            &mut generic_types_map,
                            span,
                        )?;
                    }
                    known_kwarg = true;
                }
            }
            if !known_kwarg {
                return Err(Rich::custom(
                    *span,
                    format!("Kwarg {} not found", &kwarg_ident,),
                ));
            }
        }
        for ((arg, span), (arg_name, possible_arg_type)) in args.iter().zip(func.args.iter()) {
            if let Some(arg_type) = possible_arg_type {
                let attempted_type = &arg.get_type(state);
                if !attempted_type.matches(arg_type) {
                    return Err(Rich::custom(
                        *span,
                        format!("Expected {arg:?} to match the type of {arg_name} ({arg_type})"),
                    ));
                }
                check_used_generic(arg_type, attempted_type, &mut generic_types_map, span)?;
            }
        }
    }
    let mut function_call_output = String::from(f);
    function_call_output.push(' ');
    function_call_output.push_str(&state.get_times_called(f));
    for (kwarg, (expr, span)) in kwargs {
        function_call_output.push_str(&format!(" --{kwarg} "));
        let (normal_expr, run_before) = transpile_expr((expr, *span), state)?;
        if let Some(run) = run_before {
            output.push_str(&run);
            output.push('\n');
        }
        function_call_output.push_str(&normal_expr);
    }
    for (arg, span) in args {
        function_call_output.push(' ');
        let (normal_expr, run_before) = transpile_expr((arg, *span), state)?;
        if let Some(run) = run_before {
            output.push_str(&run);
            output.push('\n');
        }
        function_call_output.push_str(&normal_expr);
    }
    output.push_str(&function_call_output);
    state.call_func(f);
    Ok(output)
}

pub fn transpile_repr<'a>(
    expr: Spanned<&'a Expr>,
    state: &mut State,
) -> Result<(String, Option<String>), Rich<'a, char>> {
    match expr.0 {
        Expr::Call(f, args, kwargs) => {
            call_function(f, (&args.0, args.1), (&kwargs.0, kwargs.1), state).map(|v| (v, None))
        }
        Expr::Pipe(first, second) => {
            let (mut output, mut run_before) = transpile_repr((&first.0, first.1), state)?;
            output.push_str(" | ");

            let (second_output, second_run_before) = transpile_repr((&second.0, second.1), state)?;
            if let Some(run) = second_run_before {
                run_before = Some(run_before.unwrap_or_default() + &run);
            }

            output.push_str(&second_output);
            Ok((output, run_before))
        }
        _ => transpile_expr(expr, state),
    }
}

#[cfg(test)]
mod tests {
    use chumsky::span::SimpleSpan;

    use crate::{transpiler::expressions::transpile_expr, Expr, State, Type};

    fn dummy_span() -> SimpleSpan {
        SimpleSpan::new(0, 0)
    }

    #[test]
    fn test_transpile_str() {
        let mut state = State::new();
        assert_eq!(
            transpile_expr((&Expr::Str(String::from("yes")), dummy_span()), &mut state),
            Ok((String::from("'yes'"), None))
        );
    }

    #[test]
    fn test_transpile_get_return_value() {
        let mut state = State::new();
        state.scopes.last_mut().unwrap().functions.insert(
            "returns_str",
            crate::Function {
                args: Vec::new(),
                kwargs: Vec::new(),
                return_value: Some(crate::Type::Str),
                times_called: 2,
                contents: String::new(),
            },
        );
        assert_eq!(
            transpile_expr(
                (
                    &Expr::Call(
                        "returns_str",
                        (Vec::new(), dummy_span()),
                        (Vec::new(), dummy_span())
                    ),
                    dummy_span()
                ),
                &mut state
            ),
            Ok((
                String::from("$__returns_str_return_value_2"),
                Some(String::from("returns_str 2"))
            ))
        );
    }

    #[test]
    fn test_transpile_get_list_at_index() {
        let mut state = State::new();
        state.scopes.last_mut().unwrap().vars.insert(
            String::from("fruits"),
            (String::from("fruits"), Type::List(Box::new(Type::Str))),
        );
        assert_eq!(
            transpile_expr(
                (
                    &Expr::ListIndex("fruits", Box::new((Expr::Num(3), dummy_span()))),
                    dummy_span()
                ),
                &mut state
            ),
            Ok((
                String::from(r#""$(eval "echo \"\$$(echo "$fruits")_$(echo "3")\"")""#),
                None
            ))
        );
    }
}
