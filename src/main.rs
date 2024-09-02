use std::{env, fs, process::ExitCode};

mod evaluator;
mod expr;
mod parser;
mod scanner;
mod token;

fn main() -> ExitCode {
    let args: Vec<String> = env::args().collect();
    if args.len() < 3 {
        eprintln!(
            "Usage: {} [tokenize | parse | evaluate] <filename>",
            args[0]
        );
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
        "parse" => {
            let file_contents = fs::read_to_string(filename).unwrap_or_else(|_| {
                eprintln!("Failed to read file {}", filename);
                String::new()
            });

            let mut scanner = scanner::Scanner::new(&file_contents);
            let tokens = scanner.scan_tokens();
            if scanner.had_error() {
                return ExitCode::from(65);
            }

            let mut parser = parser::Parser::new(tokens);
            match parser.parse_ast() {
                Ok(ast) => {
                    ast.print();
                    println!();
                }
                Err(e) => {
                    eprintln!("{e}");
                    return ExitCode::from(65);
                }
            }
        }
        "evaluate" => {
            let file_contents = fs::read_to_string(filename).unwrap_or_else(|_| {
                eprintln!("Failed to read file {}", filename);
                String::new()
            });

            let mut scanner = scanner::Scanner::new(&file_contents);
            let tokens = scanner.scan_tokens();
            if scanner.had_error() {
                return ExitCode::from(65);
            }

            let mut parser = parser::Parser::new(tokens);
            let ast = parser.parse_ast();
            if let Err(e) = ast {
                eprintln!("{e}");
                return ExitCode::from(65);
            }
            let ast = ast.unwrap();

            let result = evaluator::eval(&ast);
            if let Err(e) = result {
                eprintln!("{e}");
                return ExitCode::from(70);
            }
            let result = result.unwrap();
            println!("{result}");
        }
        _ => {
            eprintln!("Unknown command: {}", command);
        }
    }

    ExitCode::from(0)
}
