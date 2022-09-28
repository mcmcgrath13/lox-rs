use std::env;
use std::fs;
use std::io::{self, Write};

mod ast;
mod parser;
mod scanner;
mod token;

pub trait PrettyPrinting {
    fn print(&self) -> String;
}

pub trait Reportable {
    fn report(&self);
}

use crate::parser::Parser;
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
        // scanning phase
        let mut scanner = Scanner::new(code);
        let (tokens, scan_errs) = scanner.scan_tokens();
        for err in scan_errs {
            self.error(err)
        }

        // parsing phase
        let mut parser = Parser::new(tokens);
        let (ast, parse_errs) = parser.parse();
        for err in parse_errs {
            self.error(err)
        }

        // short circuit at this point if we've had errors
        if self.had_error {
            println!("we had an error!");
            return;
        }

        println!("{}", ast.expect("No errors, but no ast either").print());

        // type infer the parse
        // do environment
        // ...
    }

    fn error(&mut self, err: impl Reportable) {
        err.report();
        self.had_error = true;
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
