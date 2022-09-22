use std::env;
use std::fs;
use std::io::{self, Write};

mod token;
mod scanner;

use crate::scanner::Scanner;

#[derive(Debug)]
struct RunTime {
    had_error: bool
}

impl RunTime {
    fn new() -> RunTime {
        RunTime {
            had_error: false,
        }
    }

    fn run(&self, code: &String) {
        let mut scanner = Scanner::new(code);
        scanner.scan_tokens();
        for token in scanner.tokens {
            println!("{}", token);
        }
    }

    // TODO: use Result instead of passing this function around
    fn error(&mut self, line: u64, message: &String) {
        self.report(line, &String::new(), message);
        self.had_error = true;
    }

    fn report(&self, line: u64, location: &String, message: &String) {
        eprintln!("[line {}] Error{}: {}", line, location, message);
    }

    pub fn run_file(&self, file_path: &String) {
        let code: String = fs::read_to_string(file_path).expect("Should have been able to read the file");
        self.run(&code);
    }

    pub fn run_prompt(&mut self) {
        let mut code = String::new();
        loop {
            print!("> ");
            io::stdout().flush().unwrap();
            io::stdin().read_line(&mut code).expect("Failed to read line");
            if code.is_empty() {break};
            self.run(&code);
            code.clear();
            self.had_error = false;
        }
    }
}


fn main() {
    let args: Vec<String> = env::args().collect();
    let mut rt = RunTime::new();
    if args.len() > 2 {
        println!("Usage: lox-rs [script]");
    } else if args.len() == 2 {
        rt.run_file(&args[1]);
    } else {
        rt.run_prompt();
    }

    if rt.had_error {
        std::process::exit(65)
    }
}
