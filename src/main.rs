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

pub(crate) fn error(line: usize, message: &str) {
    report(line, "", message);
}

fn report(line: usize, location: &str, message: &str) {
    eprintln!("[line {}] Error{}: {}", line, location, message);
}

impl RunTime {
    fn new() -> RunTime {
        RunTime { had_error: false }
    }

    fn run(&mut self, code: &String) {
        let mut scanner = Scanner::new(code);
        let tokens = scanner.scan_tokens();
        for token in tokens {
            println!("{}", token);
        }
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
