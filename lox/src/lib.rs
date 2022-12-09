use std::fs;
use std::io::{self, Write};
use std::rc::Rc;

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

pub trait Printer: std::fmt::Debug {
    fn out(&self, val: String);
    fn err(&self, val: String);
}

#[derive(Debug)]
struct SysPrinter {}
impl Printer for SysPrinter {
    fn out(&self, val: String) {
        println!("{}", val)
    }

    fn err(&self, val: String) {
        eprintln!("{}", val)
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
    printer: Rc<dyn Printer>,
}

impl RunTime {
    pub fn new(printer: impl Printer + 'static) -> RunTime {
        let p = Rc::new(printer);
        RunTime {
            had_error: false,
            had_runtime_error: false,
            interpreter: Interpreter::new(p.clone()),
            printer: p,
        }
    }

    fn run_debug(&mut self, code: &String) {
        // scanning phase
        let mut scanner = Scanner::new(code);
        let (tokens, scan_errs) = scanner.scan_tokens();

        self.printer
            .out(format!("{}", "\nScanned tokens:".bold().cyan()));
        for token in tokens {
            self.printer.out(format!("{}", token))
        }
        for err in scan_errs {
            self.error(&err)
        }

        // parsing phase
        let mut parser = Parser::new(tokens);
        let (ast, parse_errs) = parser.parse();
        for err in parse_errs {
            self.error(&err)
        }

        // short circuit at this point if we've had errors
        if self.had_error {
            self.printer
                .err(format!("{}", "\nOH NO! we had an error!".bold().red()));
            return;
        }

        self.printer
            .out(format!("{}", "\nParsed AST:".bold().yellow()));
        for stmt in &ast {
            self.printer.out(stmt.print());
        }

        let mut resolver = Resolver::new();
        let (locals, resolve_errs) = resolver.resolve(&ast);
        for err in resolve_errs {
            self.error(&err)
        }

        // short circuit at this point if we've had errors
        if self.had_error {
            self.printer
                .err(format!("{}", "\nOH NO! we had an error!".bold().red()));
            return;
        }

        self.printer.out(format!("{}", "\nResult:".bold().green()));
        match self.interpreter.interpret(ast, locals) {
            Ok(v) => self.printer.out(v),
            Err(err) => self.runtime_error(err),
        };
    }

    pub fn run(&mut self, code: &String) {
        // scanning phase
        let mut scanner = Scanner::new(code);
        let (tokens, scan_errs) = scanner.scan_tokens();

        // parsing phase
        let mut parser = Parser::new(tokens);
        let (ast, parse_errs) = parser.parse();
        if !scan_errs.is_empty() || !parse_errs.is_empty() {
            self.error_all(scan_errs);
            self.error_all(parse_errs);
            return;
        }

        let mut resolver = Resolver::new();
        let (locals, resolve_errs) = resolver.resolve(&ast);
        if !resolve_errs.is_empty() {
            return;
        }

        match self.interpreter.interpret(ast, locals) {
            Ok(s) => self.printer.out(s),
            Err(err) => self.runtime_error(err),
        };
    }

    fn error_all(&mut self, errs: Vec<impl Reportable>) {
        for err in errs.iter() {
            self.error(err);
        }
    }

    fn error(&mut self, err: &impl Reportable) {
        self.had_error = true;
        self.printer.err(err.report())
    }

    fn runtime_error(&mut self, err: impl Reportable) {
        self.had_runtime_error = true;
        self.printer.err(err.report())
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
            self.run(&code);
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
        Self::new(SysPrinter {})
    }
}
