use crate::parser::parser;
use ariadne::{sources, Color, Label, Report, ReportKind};
use chumsky::{prelude::Rich, Parser};
use parser::Spanned;
use std::{borrow::Cow, collections::HashMap, fmt::Display, path::PathBuf, process::exit};
use transpiler::transpile_from_ast;

mod macros;
mod parser;
mod transpiler;

#[derive(Debug, Clone)]
struct Function<'src> {
    args: Vec<(&'src str, Option<Type>)>,
    kwargs: Vec<Kwarg<'src>>,
    return_value: Option<Type>,
    times_called: usize,
    contents: String,
}

#[derive(Debug, Clone)]
struct Kwarg<'src> {
    ident: &'src str,
    kwarg_type: Option<Type>,
    default: Spanned<Expr<'src>>,
}

#[derive(Debug, Clone)]
enum Statement<'src> {
    Expression(Spanned<Expr<'src>>),
    Assignment(bool, &'src str, Option<Type>, Spanned<Expr<'src>>),
    LocalAssignment(bool, &'src str, Option<Type>, Spanned<Expr<'src>>),
    Function(
        &'src str,
        Option<Vec<&'src str>>,
        Vec<(&'src str, Option<Type>)>,
        Vec<Kwarg<'src>>,
        Option<Type>,
        Spanned<Vec<Spanned<Statement<'src>>>>,
    ),
    For(
        &'src str,
        Spanned<Expr<'src>>,
        Spanned<Vec<Spanned<Statement<'src>>>>,
    ),
    While(
        Spanned<Condition<'src>>,
        Spanned<Vec<Spanned<Statement<'src>>>>,
    ),
    If(Spanned<IfStatement<'src>>),
    Return(Spanned<Expr<'src>>),
    Import(Spanned<String>),
    Pub(Spanned<Box<Statement<'src>>>),
    Empty,
}

#[derive(Clone, PartialEq, Eq, Hash, Debug)]
enum Type {
    Str,
    Num,
    Any,
    None,
    Generic(String),
    List(Box<Type>),
}

impl Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Type::Str => write!(f, "\x1b[35mstring\x1b[39m"),
            Type::Num => write!(f, "\x1b[35mint\x1b[39m"),
            Type::Any => write!(f, "\x1b[35mundefined\x1b[39m"),
            Type::None => write!(f, "\x1b[35mNone\x1b[39m"),
            Type::Generic(v) => write!(f, "\x1b[35mGeneric variable {}\x1b[39m", v),
            Type::List(v) => write!(f, "\x1b[35m[{}]\x1b[39m", v),
        }
    }
}

impl Type {
    fn matches(&self, other_type: &Self) -> bool {
        match (self, other_type) {
            (_, Self::Any) | (Self::Any, _) => true,
            (Self::List(l1), Self::List(l2)) => l1.matches(l2),
            (Self::List(_), _) | (_, Self::List(_)) => false,
            (Self::Generic(_), _) | (_, Self::Generic(_)) => true,
            (t1, t2) => t1 == t2,
        }
    }

    fn get_generic_var(&self) -> Option<(&str, Vec<PathToValue>)> {
        match self {
            Self::Generic(name) => Some((name, Vec::new())),
            Self::List(t) => t.get_generic_var().map(|(ty, mut path)| {
                path.push(PathToValue::List);
                (ty, path)
            }),
            _ => None,
        }
    }
}

enum PathToValue {
    List,
}

#[derive(Debug, Clone)]
struct IfStatement<'a> {
    cond: Spanned<Condition<'a>>,
    statements: Vec<Spanned<Statement<'a>>>,
    continue_if: Box<Option<ContinueIfStatement<'a>>>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
enum Condition<'src> {
    Expression(Spanned<Expr<'src>>),
    Operator(&'src str, Spanned<Expr<'src>>, Spanned<Expr<'src>>),
    Not(Box<Condition<'src>>),
    InParens(Box<Condition<'src>>),
    And(Box<Condition<'src>>, Box<Condition<'src>>),
    Or(Box<Condition<'src>>, Box<Condition<'src>>),
}

#[derive(Debug, Clone)]
enum ContinueIfStatement<'src> {
    If(Spanned<IfStatement<'src>>),
    Else(Vec<Spanned<Statement<'src>>>),
}

#[derive(Debug, Clone, PartialEq, Eq)]
enum Expr<'src> {
    Num(i64), // TODO: float
    Str(String),
    List(Spanned<Vec<Spanned<Expr<'src>>>>),
    ListIndex(&'src str, Box<Spanned<Expr<'src>>>),
    Var(&'src str),
    Call(
        &'src str,
        Spanned<Vec<Spanned<Expr<'src>>>>,
        Spanned<Vec<(&'src str, Spanned<Expr<'src>>)>>,
    ),
    CallModule(
        &'src str,
        &'src str,
        Spanned<Vec<Spanned<Expr<'src>>>>,
        Spanned<Vec<(&'src str, Spanned<Expr<'src>>)>>,
    ),
    Macro(&'src str, Spanned<Vec<Spanned<Expr<'src>>>>),
    Pipe(Box<Spanned<Expr<'src>>>, Box<Spanned<Expr<'src>>>),
    Operation(
        &'src str,
        Box<Spanned<Expr<'src>>>,
        Box<Spanned<Expr<'src>>>,
    ),
}

impl<'src> Expr<'src> {
    fn get_type(&self, state: &'src State<'src>) -> Type {
        match self {
            Self::Num(_) => Type::Num,
            Self::Str(_) => Type::Str,
            Self::List(v) => Type::List(
                v.0.first()
                    .map_or(Box::new(Type::Any), |v| Box::new(v.0.get_type(state))),
            ),
            Self::ListIndex(name, _index) => state.get_var(name).map_or(Type::Any, |v| {
                if let Type::List(t) = &v.1 {
                    *t.clone()
                } else {
                    Type::Any
                }
            }),
            Self::Var(v) => state.get_var(v).map_or(Type::Any, |var| var.1.clone()),
            Self::Call(func, (args, _), _) => {
                if let Some(fun) = state.get_func(func) {
                    if let Some(value) = get_fun_return_type(fun, args, state) {
                        return value;
                    }
                }
                Type::Any
            }
            Self::CallModule(module, func, (args, _), _) => {
                if let Some(fun) = state.modules.get(module).and_then(|s| s.get_func(func)) {
                    if let Some(value) = get_fun_return_type(fun, args, state) {
                        return value;
                    }
                }
                Type::Any
            }
            Self::Macro(m, _) => match *m {
                "eval" => Type::None,
                "raw_name" => Type::Str,
                "into_str" => Type::Str,
                _ => unreachable!(),
            },
            Self::Pipe(_, expr) | Self::Operation(_, expr, _) => expr.0.get_type(state),
        }
    }
}

fn get_fun_return_type(
    fun: &Function<'_>,
    args: &Vec<(Expr<'_>, chumsky::prelude::SimpleSpan)>,
    state: &State<'_>,
) -> Option<Type> {
    if let Some(return_v) = &fun.return_value {
        if let Type::Generic(generic_v) = return_v {
            return args.iter().zip(fun.args.iter()).find_map(
                |((attempted_arg, _), (_, arg_type))| {
                    if let Some((real_generic_v, path_to_generic)) =
                        arg_type.as_ref().and_then(|v| v.get_generic_var())
                    {
                        if generic_v == real_generic_v {
                            return Some(Some(get_generic_by_path(
                                &path_to_generic,
                                attempted_arg.get_type(state),
                            )));
                        }
                    }
                    None
                },
            )?;
        }
        return Some(return_v.clone());
    }
    None
}

fn get_generic_by_path(path_to_generic: &[PathToValue], mut ty: Type) -> Type {
    for operation in path_to_generic {
        match operation {
            PathToValue::List => {
                if let Type::List(new_ty) = ty {
                    ty = *new_ty;
                } else {
                    panic!("Getting nested generic failed, path to type did not exist");
                }
            }
        }
    }
    ty
}

#[derive(Debug, Clone)]
struct State<'src> {
    name: &'src str,
    scopes: Vec<Scope<'src>>,
    list_num: usize,
    times_ran_for_loop: usize,
    modules: HashMap<&'src str, Box<State<'src>>>,
}

impl<'src> State<'src> {
    fn new() -> Self {
        Self {
            name: "",
            scopes: vec![Scope::new("global", None)],
            list_num: 0,
            times_ran_for_loop: 0,
            modules: HashMap::new(),
        }
    }

    fn new_scope(&mut self, name: &'src str, return_value: Option<Type>) {
        self.scopes.push(Scope::new(name, return_value));
    }

    fn end_scope(&mut self) {
        self.scopes.pop();
    }

    fn get_var(&self, variable_name: &str) -> Option<&(String, Type)> {
        for scope in self.scopes.iter().rev() {
            if let Some(real_var) = scope.vars.get(variable_name) {
                return Some(real_var);
            }
        }
        None
    }

    fn get_func(&self, function: &str) -> Option<&Function> {
        for scope in self.scopes.iter().rev() {
            if let Some(real_var) = scope.functions.get(function) {
                return Some(real_var);
            }
        }
        None
    }

    fn call_func(&mut self, function: &str) {
        for scope in self.scopes.iter_mut().rev() {
            if let Some(real_var) = scope.functions.get_mut(function) {
                real_var.times_called += 1;
                break;
            }
        }
    }

    fn get_times_called(&self, function: &str) -> String {
        self.get_func(function)
            .map_or_else(String::new, move |f| f.times_called.to_string())
    }

    fn new_list_pointer(&mut self) -> String {
        self.list_num += 1;
        format!("__list_{}", self.list_num)
    }

    fn new_for_loop_index(&mut self) -> String {
        self.times_ran_for_loop += 1;
        format!("__index_{}", self.times_ran_for_loop)
    }
}

#[derive(Debug, Clone)]
struct Scope<'src> {
    vars: HashMap<String, (String, Type)>,
    functions: HashMap<&'src str, Function<'src>>,
    name: &'src str,
    return_value: Option<Type>,
}

impl<'src> Scope<'src> {
    fn new(name: &'src str, return_value: Option<Type>) -> Self {
        Self {
            vars: HashMap::new(),
            functions: HashMap::new(),
            name,
            return_value,
        }
    }
}

fn show_err(err: &Rich<'_, char>, filename: String, src: &str) {
    Report::build(ReportKind::Error, filename.clone(), err.span().start)
        .with_message(err.to_string())
        .with_label(
            Label::new((filename.clone(), err.span().into_range()))
                .with_message(err.reason().to_string())
                .with_color(Color::Red),
        )
        .with_labels(err.contexts().map(|(label, span)| {
            Label::new((filename.clone(), span.into_range()))
                .with_message(format!("while parsing this {label}"))
                .with_color(Color::Yellow)
        }))
        .finish()
        .print(sources([(filename, src)]))
        .unwrap();
}

#[must_use]
pub fn transpile_from_file(filename: &PathBuf) -> Option<String> {
    let src = if let Ok(src) = std::fs::read_to_string(filename) {
        format!("{}\n{src}", include_str!("../lib/prelude.shh"))
    } else {
        eprintln!("Unable to read file {}", filename.to_string_lossy());
        exit(1);
    };
    let mut state = State::new();

    let mut srcs = Vec::new();
    let mut mod_asts = Vec::new();

    match parser().parse(&src).into_result() {
        Ok(ast) => {
            let mut output = String::new();
            for statement in &ast {
                if let Statement::Import(module) = &statement.0 {
                    let src = match module.0.as_str() {
                        "term" => Cow::Borrowed(include_str!("../lib/term.shh")),
                        user_module => {
                            if let Ok(src) = std::fs::read_to_string(user_module) {
                                Cow::Owned(src)
                            } else {
                                eprintln!("Unable to read file {}", filename.to_string_lossy());
                                exit(1);
                            }
                        }
                    };
                    srcs.push((module.0.as_str(), src));
                }
            }

            for (mod_name, src) in &srcs {
                let mod_ast = parser()
                    .parse(src)
                    .unwrap()
                    .into_iter()
                    .filter_map(|v| {
                        if let Statement::Pub(statement) = v.0 {
                            Some((*statement.0, statement.1))
                        } else {
                            None
                        }
                    })
                    .collect();
                mod_asts.push((mod_name, mod_ast));
            }

            for (mod_name, mod_ast) in &mod_asts {
                let mut mini_state = State {
                    name: mod_name,
                    scopes: vec![Scope::new("module", None)],
                    list_num: state.list_num,
                    times_ran_for_loop: state.times_ran_for_loop,
                    modules: HashMap::new(),
                };
                let generated = match transpile_from_ast(&mod_ast, &mut mini_state, true) {
                    Ok(main_output) => main_output,
                    Err(err) => {
                        show_err(&err, filename.to_string_lossy().to_string(), &src);
                        return None;
                    }
                };
                state.list_num += mini_state.list_num;
                state.times_ran_for_loop += mini_state.times_ran_for_loop;
                output.push_str(&generated);
                state.modules.insert(mod_name, Box::from(mini_state));
            }

            match transpile_from_ast(&ast, &mut state, true) {
                Ok(main_output) => return Some(output + &main_output),
                Err(err) => {
                    show_err(&err, filename.to_string_lossy().to_string(), &src);
                }
            }
        }
        Err(errors) => {
            for err in errors {
                show_err(&err, filename.to_string_lossy().to_string(), &src);
            }
        }
    };

    None
}
