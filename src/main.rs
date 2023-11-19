use std::collections::HashMap;

use chumsky::prelude::*;

#[derive(Debug)]
enum Statement {
    Expression(Expr),
    Assignment(String, Expr),
    Function(String, Vec<Vec<String>>, Box<Vec<Statement>>),
}

#[derive(Debug)]
enum Expr {
    Num(f64),
    Str(String),
    Var(String),
    Call(String, Vec<Expr>),
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
        return None;
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

fn parser() -> impl Parser<char, Vec<Statement>, Error = Simple<char>> {
    let ident = text::ident().padded();

    let statement = recursive(|statement| {
        let expr = recursive(|expr| {
            let int = text::int(10)
                .map(|s: String| Expr::Num(s.parse().unwrap()))
                .padded();

            let strvalue = filter::<_, _, Simple<char>>(|c: &char| *c != '"')
                .repeated()
                .map(|s: Vec<char>| Expr::Str(s.into_iter().collect()))
                .delimited_by(just('"'), just('"'))
                .padded();

            let var = ident.clone().map(|s| Expr::Var(s));

            let call = ident
                .then(
                    expr.clone()
                        .separated_by(just(','))
                        .allow_trailing()
                        .delimited_by(just('('), just(')')),
                )
                .map(|(f, args)| Expr::Call(f, args));

            let atom = int
                .or(expr.delimited_by(just('('), just(')')))
                .or(strvalue)
                .or(call)
                .or(var);
            atom
        });

        let assignment = ident
            .then_ignore(just('='))
            .then(expr.clone())
            .map(|(id, val)| Statement::Assignment(id, val));

        let function = text::keyword("fun")
            .ignore_then(ident)
            .then_ignore(just('('))
            .then(ident.repeated().separated_by(just(',')).allow_trailing())
            .then_ignore(just(')'))
            .padded()
            .then_ignore(just('{'))
            .then(statement.repeated())
            .then_ignore(just('}'))
            .map(|((id, args), body)| Statement::Function(id, args, Box::new(body)));

        assignment
            .or(function)
            .or(expr.map(|e| Statement::Expression(e)))
            .then_ignore(text::newline().or_not())
            .padded()
    });

    statement.repeated().then_ignore(end())
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
    }
}

fn transpile(statement: &Statement, state: &mut State) -> Result<String, String> {
    match statement {
        Statement::Expression(expr) => match expr {
            Expr::Call(f, args) => {
                let mut output = String::from(f);
                for arg in args {
                    output.push(' ');
                    output.push_str(&transpile_expr(arg, state)?);
                }
                Ok(output)
            }
            other => transpile_expr(other, state),
        },
        Statement::Assignment(ident, value) => {
            let mut output = String::from(ident);
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
            for (i, arg) in args.first().unwrap().into_iter().enumerate() {
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
    }
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
