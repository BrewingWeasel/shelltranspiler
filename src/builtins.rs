use std::collections::HashMap;

use crate::{Function, Scope, Type};

pub fn prelude<'a>() -> Scope<'a> {
    let functions = HashMap::from([("is_empty", is_empty())]);
    Scope {
        vars: HashMap::new(),
        functions,
        name: "prelude",
        return_value: None,
    }
}

fn is_empty<'a>() -> Function<'a> {
    Function {
        args: &[("str", Some(Type::Str))],
        kwargs: &[],
        return_value: None,
        times_called: 0,
        contents: String::from(
            "is_empty() {
test -z $2
}",
        ),
    }
}
