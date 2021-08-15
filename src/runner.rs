use std::fs::read_to_string;
use std::io;
use std::io::BufRead;

use crate::interpreter::evaluate;
use crate::parser::parse;
use crate::scanner::{scan_tokens, ScannerError};

pub fn run(source: String) -> () {
    let mut had_error = false;
    let mut had_runtime_error = false;

    let (tokens, errors) = scan_tokens(source);

    had_error = !errors.is_empty();

    let ast = parse(tokens.iter()).unwrap();

    println!("{:#?}", *ast);

    println!("{:#?}", evaluate(*ast));
}

pub fn run_file(path: &str) {
    let contents = read_to_string(path).expect("Could not read file");
    self::run(contents);
}

pub fn run_prompt() {
    let stdin = io::stdin();

    for line in stdin.lock().lines() {
        print!("> ");
        let line = line.expect("Could not read line");
        self::run(line);
    }
}
