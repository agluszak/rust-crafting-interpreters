use std::env;
use std::process::exit;

use crate::runner::{run_file, run_prompt};

mod expr;
mod interpreter;
mod parser;
mod runner;
mod scanner;
mod token;
mod value;

fn main() {
    let args: Vec<String> = env::args().collect();

    if args.len() > 2 {
        println!("Usage: {} [file]", &args[0]);
        exit(64);
    } else if args.len() == 2 {
        run_file(&args[1])
        // TODO: Err(_) => exit(65),
    } else {
        run_prompt();
    }
}
