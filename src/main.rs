use std::{env, fs, process::ExitCode};

mod scanner;
mod token;

fn main() -> ExitCode {
    let args: Vec<String> = env::args().collect();
    if args.len() < 3 {
        eprintln!("Usage: {} tokenize <filename>", args[0]);
        return ExitCode::from(128);
    }

    let command = &args[1];
    let filename = &args[2];

    match command.as_str() {
        "tokenize" => {
            let file_contents = fs::read_to_string(filename).unwrap_or_else(|_| {
                eprintln!("Failed to read file {}", filename);
                String::new()
            });

            let mut scanner = scanner::Scanner::new(&file_contents);
            let tokens = scanner.scan_tokens();
            for token in tokens {
                println!("{}", token);
            }

            if scanner.had_error() {
                return ExitCode::from(65);
            }
        }
        _ => {
            eprintln!("Unknown command: {}", command);
        }
    }

    ExitCode::from(0)
}
