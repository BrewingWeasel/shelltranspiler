use std::{
    fs,
    io::Write,
    path::PathBuf,
    process::{Command, Stdio},
};

use clap::{Parser, Subcommand};
use shelltranspiler::transpile_from_file;

#[derive(Parser)]
#[command(author, version, about, long_about = None)]
struct Args {
    #[command(subcommand)]
    command: Commands,
} // TODO: show progress etc

#[derive(Subcommand)]
enum Commands {
    /// Runs the file directly
    Run {
        /// File to run
        file: PathBuf,
    },
    /// Builds the file
    Build {
        /// File to build
        file: PathBuf,

        /// Output file
        #[arg(short, long)]
        out_file: Option<PathBuf>,

        /// Print output to stdout
        #[arg(short, long, default_value_t = false)]
        print_output: bool,
    },
}

fn main() {
    let args = Args::parse();
    match args.command {
        Commands::Run { file } => {
            if let Some(output) = transpile_from_file(&file) {
                let mut child = Command::new("sh")
                    .stdin(Stdio::piped())
                    .spawn()
                    .expect("Sh command not found");
                let mut stdin = child.stdin.take().expect("Failed to open stdin");
                std::thread::spawn(move || {
                    stdin
                        .write_all(output.as_bytes())
                        .expect("Failed to write to stdin");
                });
                child.wait().expect("Failed while waiting for child");
            }
        }
        Commands::Build {
            file,
            out_file,
            print_output,
        } => {
            if let Some(output) = transpile_from_file(&file) {
                if print_output {
                    print!("{output}");
                }
                fs::write(
                    out_file.unwrap_or(PathBuf::from(file.with_extension("sh"))),
                    output,
                )
                .expect("Unable to write to the file");
            }
        }
    }
}
