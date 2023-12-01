use std::collections::HashMap;

use crate::{Function, Kwarg, Scope, Type};

pub fn prelude<'a>() -> Scope<'a> {
    let functions = HashMap::from([
        generate_function("is_empty", &[("str", Some(Type::Str))], &[], "test -z $1"),
        generate_function(
            "is_directory",
            &[("path", Some(Type::Str))],
            &[],
            "test -d $1",
        ),
        generate_function("is_file", &[("path", Some(Type::Str))], &[], "test -f $1"),
    ]);
    Scope {
        vars: HashMap::new(),
        functions,
        name: "prelude",
        return_value: None,
    }
}

fn generate_function<'a>(
    name: &'a str,
    args: &'a [(&str, Option<Type>)],
    kwargs: &'a [Kwarg<'a>],
    contents: &str,
) -> (&'a str, Function<'a>) {
    (
        name,
        Function {
            args,
            kwargs,
            return_value: None,
            times_called: 0,
            contents: format!(
                "{name}() {{
local __n_timecalled=$1
shift
{contents}
}}"
            ),
        },
    )
}
