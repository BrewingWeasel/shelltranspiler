use crate::parser::parser;
use chumsky::Parser;
use std::collections::HashMap;

mod parser;

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

fn transpile_expr(expr: &Expr, state: &mut State) -> Result<String, String> {
    match expr {
        Expr::Num(x) => Ok(format!("'{x}'")),
        Expr::Str(s) => Ok(format!("'{s}'")),
        Expr::Var(s) => {
            if let Some(sh_variable_name) = state.get_var(s) {
                Ok(format!("${sh_variable_name}"))
            } else {
                Err(format!("Could not find variable {s}"))
            }
        }
        Expr::Call(f, args) => {
            let mut output = String::from("$(");
            output.push_str(f);
            for arg in args {
                output.push(' ');
                output.push_str(&transpile_expr(arg, state)?);
            }
            output.push(')');
            Ok(output)
        }
        Expr::Pipe(first, second) => {
            let mut output = String::from("$(");
            output.push_str(&transpile_repr(first, state)?);
            output.push_str(" | ");
            output.push_str(&transpile_repr(second, state)?);
            output.push(')');
            Ok(output)
        }
    }
}

fn transpile_repr(expr: &Expr, state: &mut State) -> Result<String, String> {
    match expr {
        Expr::Call(f, args) => {
            let mut output = String::from(f);
            for arg in args {
                output.push(' ');
                output.push_str(&transpile_expr(arg, state)?);
            }
            Ok(output)
        }
        Expr::Pipe(first, second) => {
            let mut output = transpile_repr(first, state)?;
            output.push_str(" | ");
            output.push_str(&transpile_repr(second, state)?);
            Ok(output)
        }
        other => transpile_expr(other, state),
    }
}

fn transpile(statement: &Statement, state: &mut State) -> Result<String, String> {
    match statement {
        Statement::Expression(expr) => transpile_repr(expr, state),
        Statement::Assignment(ident, value) => {
            let mut output = String::from(ident);
            output.push('=');
            output.push_str(&transpile_expr(value, state)?);
            state
                .scopes
                .first_mut()
                .unwrap()
                .vars
                .insert(ident.to_owned(), ident.to_owned());
            Ok(output)
        }
        Statement::LocalAssignment(ident, value) => {
            let mut output = String::from("local ");
            output.push_str(ident);
            output.push('=');
            output.push_str(&transpile_expr(value, state)?);
            state
                .scopes
                .last_mut()
                .unwrap()
                .vars
                .insert(ident.to_owned(), ident.to_owned());
            Ok(output)
        }
        Statement::Function(ident, args, conts) => {
            state.new_scope();
            for (i, arg) in args.first().unwrap().iter().enumerate() {
                state
                    .scopes
                    .last_mut()
                    .unwrap()
                    .vars
                    .insert(arg.to_owned(), (i + 1).to_string());
            }
            let mut output = String::from(ident);
            output.push_str("() {\n");
            output.push_str(&transpile_from_ast(conts, state)?);
            output.push('}');
            state.end_scope();
            Ok(output)
        }
        Statement::If(if_statement) => transpile_if(if_statement, state, true),
        Statement::Empty => Ok(String::new()),
    }
}

fn transpile_condition(condition: &Condition, state: &mut State) -> Result<String, String> {
    match condition {
        Condition::Expression(expr) => transpile_repr(expr, state),
        Condition::Equal(expr1, expr2) => {
            let mut output = String::from("[ ");
            output.push_str(&transpile_repr(expr1, state)?);
            output.push_str(" = ");
            output.push_str(&transpile_repr(expr2, state)?);
            output.push_str(" ]");
            Ok(output)
        }
        Condition::Not(cond) => {
            let mut output = String::from("not ");
            output.push_str(&transpile_condition(cond, state)?);
            Ok(output)
        }
        Condition::And(cond1, cond2) => {
            let mut output = transpile_condition(cond1, state)?;
            output.push_str(" && ");
            output.push_str(&transpile_condition(cond2, state)?);
            Ok(output)
        }
    }
}

fn transpile_if(
    if_statement: &IfStatement,
    state: &mut State,
    ends_if: bool,
) -> Result<String, String> {
    let mut output = String::from("if ");
    output.push_str(&transpile_condition(&if_statement.cond, state)?);
    output.push_str("; then\n");
    output.push_str(&transpile_from_ast(&if_statement.statements, state)?);
    if let Some(to_continue) = if_statement.continue_if.as_ref() {
        match to_continue {
            ContinueIfStatement::Else(statements) => {
                output.push_str("else \n");
                output.push_str(&transpile_from_ast(statements, state)?);
            }
            ContinueIfStatement::If(if_statement) => {
                output.push_str("el");
                output.push_str(&transpile_if(if_statement, state, false)?)
            }
        }
    }
    if ends_if {
        output.push_str("fi");
    }
    Ok(output)
}

fn transpile_from_ast(conts: &Vec<Statement>, state: &mut State) -> Result<String, String> {
    eprintln!("{:#?}", conts);
    let mut compiled = String::new();
    for expr in conts {
        let output = transpile(expr, state)?;
        compiled.push_str(&output);
        compiled.push('\n');
    }
    Ok(compiled)
}

fn transpile_from_string(input: &str) -> Result<String, Vec<String>> {
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

fn main() {
    let src = std::fs::read_to_string(std::env::args().nth(1).unwrap()).unwrap();

    match transpile_from_string(&src) {
        Ok(output) => print!("{output}"),
        Err(errors) => errors
            .into_iter()
            .for_each(|e| eprintln!("Parse error: {}", e)),
    }
}
