use shelltranspiler::transpile_from_string;

fn main() {
    let src = std::fs::read_to_string(std::env::args().nth(1).unwrap()).unwrap();

    match transpile_from_string(&src) {
        Ok(output) => print!("{output}"),
        Err(errors) => errors
            .into_iter()
            .for_each(|e| eprintln!("Parse error: {}", e)),
    }
}
