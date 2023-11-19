use chumsky::prelude::*;

#[derive(Debug)]
enum Statement {
    Expression(Expr),
    Assignment(String, Expr),
}

#[derive(Debug)]
enum Expr {
    Num(f64),
    Str(String),
    Var(String),
    Call(String, Vec<Expr>),
}

fn parser() -> impl Parser<char, Vec<Statement>, Error = Simple<char>> {
    let ident = text::ident().padded();
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

    let statement = assignment.or(expr.map(|e| Statement::Expression(e)));
    let statements = statement
        .then_ignore(text::newline().or_not())
        .padded()
        .repeated();

    statements.then_ignore(end())
}

fn transpile_expr(expr: &Expr) -> Result<String, String> {
    match expr {
        Expr::Num(x) => Ok(format!("'{x}'")),
        Expr::Str(s) => Ok(format!("'{s}'")),
        Expr::Var(s) => Ok(format!("${s}")),
        Expr::Call(f, args) => {
            let mut output = String::from("$(");
            output.push_str(f);
            for arg in args {
                output.push(' ');
                output.push_str(&transpile_expr(arg)?);
            }
            output.push(')');
            Ok(output)
        }
    }
}

fn transpile(statement: &Statement) -> Result<String, String> {
    match statement {
        Statement::Expression(expr) => match expr {
            Expr::Call(f, args) => {
                let mut output = String::from(f);
                for arg in args {
                    output.push(' ');
                    output.push_str(&transpile_expr(arg)?);
                }
                Ok(output)
            }
            other => transpile_expr(other),
        },
        Statement::Assignment(ident, value) => {
            let mut output = String::from(ident);
            output.push('=');
            output.push_str(&transpile_expr(value)?);
            Ok(output)
        }
    }
}

fn transpile_from_ast(conts: &Vec<Statement>) -> Result<String, String> {
    let mut compiled = String::new();
    for expr in conts {
        let output = transpile(expr)?;
        compiled.push_str(&output);
        compiled.push('\n');
    }
    Ok(compiled)
}

fn transpile_from_string(input: &str) -> Result<String, Vec<String>> {
    match parser().parse(input) {
        Ok(ast) => transpile_from_ast(&ast).map_err(|e| vec![format!("Evaluation error: {}", e)]),
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
            .for_each(|e| println!("Parse error: {}", e)),
    }
}
