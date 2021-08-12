use std::fs::read_to_string;
use std::io;
use std::io::BufRead;

use crate::scanner::{scan_tokens, ScannerError};

pub(crate) struct Interpreter {
    had_error: bool,
}

pub struct RunError;

impl Interpreter {
    pub fn new() -> Self {
        Interpreter { had_error: false }
    }

    pub fn run(source: String) -> Result<(), Vec<ScannerError>> {
        let (tokens, errors) = scan_tokens(source);

        for token in tokens {
            println!("{:?}", token);
        }

        if errors.is_empty() {
            Ok(())
        } else {
            Err(errors)
        }
    }

    pub fn run_file(path: &str) -> Result<(), Vec<ScannerError>> {
        let contents = read_to_string(path).expect("Could not read file");
        Self::run(contents)
    }

    pub fn run_prompt() {
        let stdin = io::stdin();

        for line in stdin.lock().lines() {
            print!("> ");
            let line = line.expect("Could not read line");
            Self::run(line).unwrap_or(());
        }
    }
}
