use std::env;
use std::process::exit;

use crate::runner::{Runner, RunnerError};
use crate::runner::RunnerError::ScannerError;

mod expr;
mod interpreter;
mod parser;
mod runner;
mod scanner;
mod token;
mod value;
mod stmt;

fn main() {
    let args: Vec<String> = env::args().collect();
    let mut runner = Runner::new();

    if args.len() > 2 {
        println!("Usage: {} [file]", &args[0]);
        exit(64);
    } else if args.len() == 2 {
        match runner.run_file(&args[1]) {
            Ok(_) => exit(0),
            Err(RunnerError::ScannerError(_)) => exit(65), // TODO: correct exit codes
            Err(RunnerError::ParserError(_)) => exit(66),
            Err(RunnerError::RuntimeError(_)) => exit(67)
        }
    } else {
        runner.run_prompt();
    }
}
