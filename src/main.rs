use std::env;
use std::fs;
use std::io::{self, Write};

mod ast;
mod environment;
mod interpreter;
mod parser;
mod scanner;
mod token;

pub trait PrettyPrinting {
    fn print(&self) -> String;
}

pub trait Reportable {
    fn report(&self);
}

use crate::interpreter::Interpreter;
use crate::parser::Parser;
use crate::scanner::Scanner;

#[derive(Debug)]
struct RunTime {
    had_error: bool,
    had_runtime_error: bool,
    interpreter: Interpreter,
}

impl RunTime {
    fn new() -> RunTime {
        RunTime {
            had_error: false,
            had_runtime_error: false,
            interpreter: Interpreter::new(),
        }
    }

    fn run(&mut self, code: &String) {
        // scanning phase
        let mut scanner = Scanner::new(code);
        let (tokens, scan_errs) = scanner.scan_tokens();
        for err in scan_errs {
            self.error(err)
        }
        for token in tokens {
            println!("{}", token)
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

        for stmt in &ast {
            println!("{}", stmt.print());
        }
        if let Err(err) = self.interpreter.interpret(ast) {
            self.runtime_error(err);
        }

        // type infer the parse
        // do environment
        // ...
    }

    fn error(&mut self, err: impl Reportable) {
        err.report();
        self.had_error = true;
    }

    fn runtime_error(&mut self, err: impl Reportable) {
        err.report();
        self.had_runtime_error = true;
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
            self.had_runtime_error = false;
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

    if rt.had_runtime_error {
        std::process::exit(70)
    }
    if rt.had_error {
        std::process::exit(65)
    }
}
