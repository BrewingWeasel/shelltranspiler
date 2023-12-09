use std::collections::HashMap;

use crate::{Function, Kwarg, Scope, Type};

pub fn prelude<'a>() -> Scope<'a> {
    let functions = HashMap::from([
        generate_function(
            "is_empty",
            [("str", Some(Type::Str))].into(),
            &[],
            None,
            "test -z $1",
        ),
        generate_function(
            "is_directory",
            [("path", Some(Type::Str))].into(),
            &[],
            None,
            "test -d $1",
        ),
        push(),
        len(),
        generate_function(
            "is_file",
            [("path", Some(Type::Str))].into(),
            &[],
            None,
            "test -f $1",
        ),
    ]);
    Scope {
        vars: HashMap::new(),
        functions,
        name: "prelude",
        return_value: None,
    }
}

fn push<'a>() -> (&'a str, Function<'a>) {
    generate_function(
        "push",
        vec![
            (
                "list",
                Some(Type::List(Box::new(Type::Generic(String::from("v"))))),
            ),
            ("elem", Some(Type::Generic(String::from("v")))),
        ],
        &[],
        Some(Type::Generic(String::from("v"))),
        r#"eval "$1_$(eval "echo \"\$$(echo "$1")_len\"")=$2"; eval "$1_len=$(( "$(echo "$1")_len" + 1))""#,
    )
}

fn len<'a>() -> (&'a str, Function<'a>) {
    generate_function(
        "len",
        vec![(
            "list",
            Some(Type::List(Box::new(Type::Generic(String::from("v"))))),
        )],
        &[],
        Some(Type::Num),
        r#"eval "__len_return_value_$__n_timecalled=\"\$${1}_len\"""#,
    )
}

fn generate_function<'a>(
    name: &'a str,
    args: Vec<(&'a str, Option<Type>)>,
    kwargs: &'a [Kwarg<'a>],
    return_value: Option<Type>,
    contents: &str,
) -> (&'a str, Function<'a>) {
    (
        name,
        Function {
            args,
            kwargs,
            return_value,
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
