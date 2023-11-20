use crate::parser::parser;
use chumsky::Parser;
use std::collections::HashMap;
use transpiler::transpile_from_ast;

mod parser;
mod transpiler;

#[derive(Debug, Clone)]
enum Statement {
    Expression(Expr),
    Assignment(String, Expr),
    LocalAssignment(String, Expr),
    Function(String, Vec<Vec<String>>, Vec<Statement>),
    If(IfStatement),
    Empty,
}

#[derive(Debug, Clone)]
struct IfStatement {
    cond: Condition,
    statements: Vec<Statement>,
    continue_if: Box<Option<ContinueIfStatement>>,
}

#[derive(Debug, Clone)]
enum Condition {
    Expression(Expr),
    Equal(Expr, Expr),
    Not(Box<Condition>),
    And(Box<Condition>, Box<Condition>),
    Or(Box<Condition>, Box<Condition>),
}

#[derive(Debug, Clone)]
enum ContinueIfStatement {
    If(IfStatement),
    Else(Vec<Statement>),
}

#[derive(Debug, Clone)]
enum Expr {
    Num(f64),
    Str(String),
    Var(String),
    Call(String, Vec<Expr>),
    Pipe(Box<Expr>, Box<Expr>),
}

#[derive(Debug, Clone)]
struct State {
    scopes: Vec<Scope>,
}

impl State {
    fn new() -> Self {
        Self {
            scopes: vec![Scope::new()],
        }
    }

    fn new_scope(&mut self) {
        self.scopes.push(Scope::new())
    }

    fn end_scope(&mut self) {
        self.scopes.pop();
    }

    fn get_var(&self, variable_name: &str) -> Option<&String> {
        for scope in self.scopes.iter().rev() {
            if let Some(real_var) = scope.vars.get(variable_name) {
                return Some(real_var);
            }
        }
        None
    }
}

#[derive(Debug, Clone)]
struct Scope {
    vars: HashMap<String, String>,
}

impl Scope {
    fn new() -> Self {
        Self {
            vars: HashMap::new(),
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
            .map(|e| format!("Parse error: {}", e))
            .collect()),
    }
}
