use shelltranspiler::transpile_from_file;

fn main() {
    let filename = std::env::args().nth(1).unwrap();
    if let Some(output) = transpile_from_file(filename) {
        print!("{output}")
    }
}
