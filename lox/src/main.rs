use std::env;

use lox::RunTime;

fn main() {
    let args: Vec<String> = env::args().collect();
    let mut rt = RunTime::default();
    match args.len() {
        3.. => println!("Usage: lox-rs [script]"),
        2 => rt.run_file(&args[1]),
        _ => rt.run_prompt(),
    }

    std::process::exit(rt.exit_code())
}
