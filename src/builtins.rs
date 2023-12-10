use std::collections::HashMap;

use chumsky::span::SimpleSpan;

use crate::{Expr, Function, Kwarg, Scope, Type};

pub fn prelude<'a>() -> Scope<'a> {
    let functions = HashMap::from([
        generate_function(
            "is_empty",
            [("str", Some(Type::Str))].into(),
            Vec::new(),
            None,
            "test -z $1",
        ),
        generate_function(
            "is_directory",
            [("path", Some(Type::Str))].into(),
            Vec::new(),
            None,
            "test -d $1",
        ),
        push(),
        len(),
        pop(),
        generate_function(
            "is_file",
            [("path", Some(Type::Str))].into(),
            Vec::new(),
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
        Vec::new(),
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
        Vec::new(),
        Some(Type::Num),
        r#"eval "__len_return_value_$__n_timecalled=\"\$${1}_len\"""#,
    )
}

fn pop<'a>() -> (&'a str, Function<'a>) {
    generate_function(
        "pop",
        vec![(
            "list",
            Some(Type::List(Box::new(Type::Generic(String::from("v"))))),
        )],
        vec![Kwarg {
            ident: "index",
            kwarg_type: Some(Type::Num),
            default: (Expr::Num(-1), SimpleSpan::new(0, 0)),
        }],
        Some(Type::Generic(String::from("v"))),
        r#"
if [ $1 == "--index" ]; then 
    shift
    if [ "$1" = "${1#-}" ]; then
        to_remove=$1
    else
        to_remove=$(("$(echo "${2}_len")" - "${1#-}"))
    fi
    shift
else
    to_remove=$(("$(echo "${1}_len")" - 1))
fi
eval "__pop_return_value_$__n_timecalled=\"\$${1}_${to_remove}\""
eval "ending=\"$(("$(echo "${1}_len")" - 1))\""
while [ $to_remove -lt $ending ]; do
new_to_remove=$(($to_remove + 1))
eval "${1}_$to_remove=\"\$${1}_$new_to_remove\""
to_remove="$new_to_remove"
done
eval "${1}_len=$(("${1}_len" - 1))"
eval "unset "${1}_$(echo "\$${1}_len")""  "#,
    ) // TODO: any negative value should work
}

fn generate_function<'a>(
    name: &'a str,
    args: Vec<(&'a str, Option<Type>)>,
    kwargs: Vec<Kwarg<'a>>,
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
