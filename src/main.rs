use std::env;
use std::process::exit;

use crate::interpreter::Interpreter;

mod expr;
mod interpreter;
mod parser;
mod scanner;
mod tokens;

fn main() {
    let args: Vec<String> = env::args().collect();
    let _interpreter = Interpreter::new();

    if args.len() > 2 {
        println!("Usage: {} [file]", &args[0]);
        exit(64);
    } else if args.len() == 2 {
        match Interpreter::run_file(&args[1]) {
            Ok(_) => {}
            Err(_) => exit(65),
        }
    } else {
        Interpreter::run_prompt();
    }
}
