use std::env;
use std::fs;
use std::io::{self, Write};

mod scanner;
mod token;

use crate::scanner::Scanner;

#[derive(Debug)]
struct RunTime {
    had_error: bool,
}

impl RunTime {
    fn new() -> RunTime {
        RunTime { had_error: false }
    }

    fn run(&mut self, code: &String) {
        let mut scanner = Scanner::new(code);

        let (tokens, errs) = scanner.scan_tokens();
        for err in errs {
            let (line, msg) = err;
            self.error(line, &msg)
        }
        for token in tokens {
            println!("{}", token);
        }
        // parse the tokens
        // type infer the parse
        // do environment
    }

    fn error(&mut self, line: usize, message: &str) {
        self.report(line, "", message);
        self.had_error = true;
    }

    fn report(&self, line: usize, location: &str, message: &str) {
        eprintln!("[line {}] Error{}: {}", line, location, message);
    }

    pub fn run_file(&mut self, file_path: &String) {
        let code: String =
            fs::read_to_string(file_path).expect("Should have been able to read the file");
        self.run(&code);
    }

    pub fn run_prompt(&mut self) {
        let mut code = String::new();
        loop {
            print!("> ");
            io::stdout().flush().unwrap();
            io::stdin()
                .read_line(&mut code)
                .expect("Failed to read line");
            if code.is_empty() {
                break;
            };
            self.run(&code);
            code.clear();
            self.had_error = false;
        }
    }
}

fn main() {
    let args: Vec<String> = env::args().collect();
    let mut rt = RunTime::new();
    match args.len() {
        3.. => println!("Usage: lox-rs [script]"),
        2 => rt.run_file(&args[1]),
        _ => rt.run_prompt(),
    }

    if rt.had_error {
        std::process::exit(65)
    }
}
