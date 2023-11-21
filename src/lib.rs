use crate::parser::parser;
use chumsky::Parser;
use std::collections::HashMap;
use transpiler::transpile_from_ast;

mod parser;
mod transpiler;

#[derive(Debug, Clone)]
enum Statement<'a> {
    Expression(Expr),
    Assignment(String, Option<Type>, Expr),
    LocalAssignment(String, Option<Type>, Expr),
    Function(
        String,
        Vec<(String, Option<Type>)>,
        Option<Type>,
        Vec<Statement<'a>>,
    ),
    If(IfStatement<'a>),
    Return(Expr),
    Empty,
}

#[derive(Debug, Clone, PartialEq, Copy)]
enum Type {
    Str,
    Num,
    Any,
}

#[derive(Debug, Clone)]
struct IfStatement<'a> {
    cond: Condition<'a>,
    statements: Vec<Statement<'a>>,
    continue_if: Box<Option<ContinueIfStatement<'a>>>,
}

#[derive(Debug, Clone)]
enum Condition<'a> {
    Expression(Expr),
    Operator(&'a str, Expr, Expr),
    Not(Box<Condition<'a>>),
    InParens(Box<Condition<'a>>),
    And(Box<Condition<'a>>, Box<Condition<'a>>),
    Or(Box<Condition<'a>>, Box<Condition<'a>>),
}

#[derive(Debug, Clone)]
enum ContinueIfStatement<'a> {
    If(IfStatement<'a>),
    Else(Vec<Statement<'a>>),
}

#[derive(Debug, Clone)]
enum Expr {
    Num(f64),
    Str(String),
    Var(String),
    Call(String, Vec<Expr>),
    CallPiped(String, Vec<Expr>),
    Pipe(Box<Expr>, Box<Expr>),
}

impl Expr {
    fn get_type(&self, state: &State) -> Type {
        match self {
            Self::Num(_) => Type::Num,
            Self::Str(_) => Type::Str,
            Self::Var(v) => state
                .get_var(v)
                .map(|var| var.1)
                .flatten()
                .unwrap_or(Type::Any),
            Self::Call(func, _) => state
                .get_func(func)
                .map(|func| func.1)
                .flatten()
                .unwrap_or(Type::Any),
            _ => unimplemented!(),
        }
    }
}

#[derive(Debug, Clone)]
struct State {
    scopes: Vec<Scope>,
}

impl State {
    fn new() -> Self {
        Self {
            scopes: vec![Scope::new("global".to_owned())],
        }
    }

    fn new_scope(&mut self, name: &str) {
        self.scopes.push(Scope::new(name.to_owned()))
    }

    fn end_scope(&mut self) {
        self.scopes.pop();
    }

    fn get_var(&self, variable_name: &str) -> Option<&(String, Option<Type>)> {
        for scope in self.scopes.iter().rev() {
            if let Some(real_var) = scope.vars.get(variable_name) {
                return Some(real_var);
            }
        }
        None
    }
    fn get_func(&self, function: &str) -> Option<&(Vec<(String, Option<Type>)>, Option<Type>)> {
        for scope in self.scopes.iter().rev() {
            if let Some(real_var) = scope.functions.get(function) {
                return Some(real_var);
            }
        }
        None
    }
}

#[derive(Debug, Clone)]
struct Scope {
    vars: HashMap<String, (String, Option<Type>)>,
    functions: HashMap<String, (Vec<(String, Option<Type>)>, Option<Type>)>,
    name: String,
}

impl Scope {
    fn new(name: String) -> Self {
        Self {
            vars: HashMap::new(),
            functions: HashMap::new(),
            name,
        }
    }
}

pub fn transpile_from_string(input: &str) -> Result<String, Vec<String>> {
    let mut state = State::new();
    match parser().parse(input) {
        Ok(ast) => transpile_from_ast(&ast, &mut state)
            .map_err(|e| vec![format!("Evaluation error: {}", e)]),
        Err(parse_errs) => Err(parse_errs
            .into_iter()
            .map(|e| format!("Parse error: {} {:?}", e, e.span()))
            .collect()),
    }
}
