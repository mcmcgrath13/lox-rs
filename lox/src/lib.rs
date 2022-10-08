use std::fs;
use std::io::{self, Write};

use colored::Colorize;

use crate::interpreter::Interpreter;
use crate::parser::Parser;
use crate::resolver::Resolver;
use crate::scanner::Scanner;

mod ast;
mod environment;
mod interpreter;
mod parser;
mod resolver;
mod scanner;
mod token;
mod types;

pub trait PrettyPrinting {
    fn print(&self) -> String;
}

impl<T> PrettyPrinting for Vec<T>
where
    T: PrettyPrinting,
{
    fn print(&self) -> String {
        self.iter()
            .map(PrettyPrinting::print)
            .collect::<Vec<_>>()
            .join(" ")
    }
}

pub trait Reportable {
    fn report(&self) -> String;
}

#[derive(Debug)]
pub struct RunTime {
    had_error: bool,
    had_runtime_error: bool,
    interpreter: Interpreter,
}

impl RunTime {
    pub fn new() -> RunTime {
        RunTime {
            had_error: false,
            had_runtime_error: false,
            interpreter: Interpreter::new(),
        }
    }

    fn run_debug(&mut self, code: &String) {
        // scanning phase
        let mut scanner = Scanner::new(code);
        let (tokens, scan_errs) = scanner.scan_tokens();

        println!("{}", "\nScanned tokens:".bold().cyan());
        for token in tokens {
            println!("{}", token)
        }
        for err in scan_errs {
            eprintln!("{}", self.error(&err))
        }

        // parsing phase
        let mut parser = Parser::new(tokens);
        let (ast, parse_errs) = parser.parse();
        for err in parse_errs {
            eprintln!("{}", self.error(&err))
        }

        // short circuit at this point if we've had errors
        if self.had_error {
            eprintln!("{}", "\nOH NO! we had an error!".bold().red());
            return;
        }

        println!("{}", "\nParsed AST:".bold().yellow());
        for stmt in &ast {
            println!("{}", stmt.print());
        }

        let mut resolver = Resolver::new();
        let (locals, resolve_errs) = resolver.resolve(&ast);
        for err in resolve_errs {
            eprintln!("{}", self.error(&err))
        }

        // short circuit at this point if we've had errors
        if self.had_error {
            eprintln!("{}", "\nOH NO! we had an error!".bold().red());
            return;
        }

        println!("{}", "\nResult:".bold().green());
        match self.interpreter.interpret(ast, locals) {
            Ok(v) => println!("{}", v),
            Err(err) => eprintln!("{}", self.runtime_error(err)),
        };
    }

    pub fn run(&mut self, code: &String) -> Result<String, String> {
        // scanning phase
        let mut scanner = Scanner::new(code);
        let (tokens, scan_errs) = scanner.scan_tokens();

        // parsing phase
        let mut parser = Parser::new(tokens);
        let (ast, parse_errs) = parser.parse();
        if !scan_errs.is_empty() || !parse_errs.is_empty() {
            let errs = self.error_all(scan_errs) + "\n" + &self.error_all(parse_errs);
            return Err(errs.trim().to_string());
        }

        let mut resolver = Resolver::new();
        let (locals, resolve_errs) = resolver.resolve(&ast);
        if !resolve_errs.is_empty() {
            return Err(self.error_all(resolve_errs));
        }

        match self.interpreter.interpret(ast, locals) {
            Ok(s) => Ok(s),
            Err(err) => Err(self.runtime_error(err)),
        }
    }

    fn error_all(&mut self, errs: Vec<impl Reportable>) -> String {
        errs.iter()
            .map(|e| self.error(e))
            .collect::<Vec<String>>()
            .join("\n")
    }

    fn error(&mut self, err: &impl Reportable) -> String {
        self.had_error = true;
        err.report()
    }

    fn runtime_error(&mut self, err: impl Reportable) -> String {
        self.had_runtime_error = true;
        err.report()
    }

    pub fn run_file(&mut self, file_path: &String) {
        let code: String =
            fs::read_to_string(file_path).expect("Should have been able to read the file");
        self.run_debug(&code);
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
            match self.run(&code) {
                Ok(s) => println!("{}", s),
                Err(s) => eprintln!("{}", s),
            };
            code.clear();
            self.had_error = false;
            self.had_runtime_error = false;
        }
    }

    pub fn exit_code(&mut self) -> i32 {
        if self.had_runtime_error {
            70
        } else if self.had_error {
            65
        } else {
            0
        }
    }
}

impl Default for RunTime {
    fn default() -> Self {
        Self::new()
    }
}
