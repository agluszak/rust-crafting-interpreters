use std::fs::read_to_string;
use std::io;
use std::io::BufRead;

use crate::interpreter::Interpreter;
use crate::parser::parse;
use crate::scanner::{Scanner, ScannerError};

struct Runner {

}

pub fn run(source: String) -> () {
    let mut had_error = false;
    let mut had_runtime_error = false;

    let mut scanner = Scanner::new();

    scanner.append(source);

    let (tokens, errors) = scanner.scan_tokens();

    had_error = !errors.is_empty();

    let ast = parse(tokens).unwrap();

    println!("{:#?}", ast);

    let mut interpreter = Interpreter::new();

    println!("{:#?}", interpreter.interpret(ast));
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
