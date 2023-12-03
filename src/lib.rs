use crate::parser::parser;
use ariadne::{sources, Color, Label, Report, ReportKind};
use builtins::prelude;
use chumsky::{prelude::Rich, Parser};
use parser::Spanned;
use std::{collections::HashMap, path::PathBuf, process::exit};
use transpiler::transpile_from_ast;

mod builtins;
mod parser;
mod transpiler;

#[derive(Debug, Clone)]
struct Function<'src> {
    args: &'src [(&'src str, Option<Type>)],
    kwargs: &'src [Kwarg<'src>],
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
    Empty,
}

#[derive(Debug, Clone, PartialEq)]
enum Type {
    Str,
    Num,
    Any,
    Generic(String),
    List(Box<Type>),
}

impl Type {
    fn matches(&self, other_type: &Type) -> bool {
        match (self, other_type) {
            (_, Self::Any) | (Self::Any, _) => true,
            (Self::List(l1), Self::List(l2)) => l1.matches(l2),
            (Self::List(_), _) | (_, Self::List(_)) => false,
            (Self::Generic(_), _) | (_, Self::Generic(_)) => true,
            (t1, t2) => t1 == t2,
        }
    }

    fn get_generic_var(&self) -> Option<&str> {
        match self {
            Self::Generic(name) => Some(name),
            Self::List(t) => t.get_generic_var(),
            _ => None,
        }
    }
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
    CallPiped(
        &'src str,
        Spanned<Vec<Spanned<Expr<'src>>>>,
        Spanned<Vec<(&'src str, Spanned<Expr<'src>>)>>,
    ),
    Pipe(Box<Spanned<Expr<'src>>>, Box<Spanned<Expr<'src>>>),
}

impl<'src> Expr<'src> {
    fn get_type(&self, state: &'src State) -> Type {
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
            Self::Var(v) => state
                .get_var(v)
                .map(|var| var.1.clone())
                .unwrap_or(Type::Any),
            Self::Call(func, _, _) => state
                .get_func(func)
                .and_then(|func| func.return_value.clone())
                .unwrap_or(Type::Any),
            Self::CallPiped(_, _, _) => Type::Any,
            Self::Pipe(_, expr) => expr.0.get_type(state),
        }
    }
}

#[derive(Debug, Clone)]
struct State<'src> {
    scopes: Vec<Scope<'src>>,
    prelude: Scope<'src>,
    list_num: usize,
    times_ran_for_loop: usize,
}

impl<'src> State<'src> {
    fn new() -> Self {
        Self {
            scopes: vec![Scope::new("global", None)],
            prelude: prelude(),
            list_num: 0,
            times_ran_for_loop: 0,
        }
    }

    fn new_scope(&mut self, name: &'src str, return_value: Option<Type>) {
        self.scopes.push(Scope::new(name, return_value));
    }

    fn end_scope(&mut self) {
        self.scopes.pop();
    }

    fn get_var(&'src self, variable_name: &str) -> Option<&(String, Type)> {
        for scope in self.scopes.iter().rev() {
            if let Some(real_var) = scope.vars.get(variable_name) {
                return Some(real_var);
            }
        }
        self.prelude.vars.get(variable_name)
    }

    fn get_func(&self, function: &str) -> Option<&Function> {
        for scope in self.scopes.iter().rev() {
            if let Some(real_var) = scope.functions.get(function) {
                return Some(real_var);
            }
        }
        self.prelude.functions.get(function)
    }

    fn call_func(&mut self, function: &str) {
        for scope in self.scopes.iter_mut().rev() {
            if let Some(real_var) = scope.functions.get_mut(function) {
                real_var.times_called += 1;
                break;
            }
        }
        if let Some(real_var) = self.prelude.functions.get_mut(function) {
            real_var.times_called += 1;
        }
    }

    fn get_times_called(&self, function: &str) -> String {
        self.get_func(function)
            .map_or_else(String::new, |f| f.times_called.to_string())
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
    let Ok(src) = std::fs::read_to_string(filename) else {
        eprintln!("Unable to read file {}", filename.to_string_lossy());
        exit(1);
    };
    let mut state = State::new();

    match parser().parse(&src).into_result() {
        Ok(ast) => match transpile_from_ast(&ast, &mut state, true) {
            Ok(output) => return Some(output),
            Err(err) => {
                show_err(&err, filename.to_string_lossy().to_string(), &src);
            }
        },
        Err(errors) => {
            for err in errors {
                show_err(&err, filename.to_string_lossy().to_string(), &src);
            }
        }
    };

    None
}
