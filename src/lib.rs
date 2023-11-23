use crate::parser::parser;
use chumsky::{prelude::Rich, span::SimpleSpan, Parser};
use parser::Spanned;
use std::collections::HashMap;
use transpiler::transpile_from_ast;

mod parser;
mod transpiler;

type Function<'src> = (&'src [(&'src str, Option<Type>)], Option<Type>);

#[derive(Debug, Clone)]
enum Statement<'src> {
    Expression(Spanned<Expr<'src>>),
    Assignment(&'src str, Option<Type>, Spanned<Expr<'src>>),
    LocalAssignment(&'src str, Option<Type>, Spanned<Expr<'src>>),
    Function(
        &'src str,
        Vec<(&'src str, Option<Type>)>,
        Option<Type>,
        Vec<Spanned<Statement<'src>>>,
    ),
    If(Spanned<IfStatement<'src>>),
    Return(Spanned<Expr<'src>>),
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
    cond: Spanned<Condition<'a>>,
    statements: Vec<Spanned<Statement<'a>>>,
    continue_if: Box<Option<ContinueIfStatement<'a>>>,
}

#[derive(Debug, Clone)]
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

#[derive(Debug, Clone)]
enum Expr<'src> {
    Num(f64),
    Str(String),
    Var(&'src str),
    Call(&'src str, Vec<Spanned<Expr<'src>>>),
    CallPiped(&'src str, Vec<Spanned<Expr<'src>>>),
    Pipe(Box<Spanned<Expr<'src>>>, Box<Spanned<Expr<'src>>>),
}

impl<'src> Expr<'src> {
    fn get_type(&self, state: &State) -> Type {
        match self {
            Self::Num(_) => Type::Num,
            Self::Str(_) => Type::Str,
            Self::Var(v) => state.get_var(v).and_then(|var| var.1).unwrap_or(Type::Any),
            Self::Call(func, _) => state
                .get_func(func)
                .and_then(|func| func.1)
                .unwrap_or(Type::Any),
            Self::CallPiped(_, _) => Type::Any,
            Self::Pipe(_, expr) => expr.0.get_type(state),
        }
    }
}

#[derive(Debug, Clone)]
struct State<'src> {
    scopes: Vec<Scope<'src>>,
}

impl<'src> State<'src> {
    fn new() -> Self {
        Self {
            scopes: vec![Scope::new("global")],
        }
    }

    fn new_scope(&mut self, name: &'src str) {
        self.scopes.push(Scope::new(name))
    }

    fn end_scope(&mut self) {
        self.scopes.pop();
    }

    fn get_var(&'src self, variable_name: &str) -> Option<&(String, Option<Type>)> {
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
}

#[derive(Debug, Clone)]
struct Scope<'src> {
    vars: HashMap<String, (String, Option<Type>)>,
    functions: HashMap<&'src str, Function<'src>>,
    name: &'src str,
}

impl<'src> Scope<'src> {
    fn new(name: &'src str) -> Self {
        Self {
            vars: HashMap::new(),
            functions: HashMap::new(),
            name,
        }
    }
}

pub fn transpile_from_string(input: &str) -> Result<String, Vec<Rich<char>>> {
    let mut state = State::new();
    match parser().parse(input).into_result() {
        Ok(ast) => transpile_from_ast(&ast, &mut state)
            .map_err(|e| vec![Rich::custom(SimpleSpan::new(0, 4), e)]),
        Err(parse_errs) => Err(parse_errs),
    }
}
