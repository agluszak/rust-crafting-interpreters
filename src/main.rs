use std::env;
use std::process::exit;

use crate::runner::{Runner, RunnerError};

mod expr;
mod interpreter;
mod parser;
mod runner;
mod scanner;
mod stmt;
mod token;
mod value;

fn main() {
    let args: Vec<String> = env::args().collect();
    let mut runner = Runner::new();

    if args.len() > 2 {
        println!("Usage: {} [file]", &args[0]);
        exit(64);
    } else if args.len() == 2 {
        match runner.run_file(&args[1]) {
            Ok(_) => exit(0),
            Err(err) => {
                println!("{:?}", err);
                exit(1);
            }
        }
    } else {
        runner.run_prompt();
    }
}
