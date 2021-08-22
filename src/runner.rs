use std::fs::read_to_string;
use std::io;
use std::io::BufRead;

use crate::interpreter::{Interpreter, RuntimeError};
use crate::parser::{Parser, ParserError};
use crate::scanner::{Scanner, ScannerError};

#[derive(Debug)]
pub enum RunnerError {
    ScannerError(Vec<ScannerError>),
    ParserError(ParserError),
    RuntimeError(RuntimeError),
}

type Result<T> = std::result::Result<T, RunnerError>;

pub struct Runner {
    scanner: Scanner,
    parser: Parser,
    interpreter: Interpreter,
}

impl Runner {
    pub fn new() -> Self {
        Self {
            scanner: Scanner::new(),
            parser: Parser::new(),
            interpreter: Interpreter::new(),
        }
    }

    pub fn run(&mut self, source: String) -> Result<()> {
        self.scanner.append(source);
        let tokens = self.scanner.scan_tokens().map_err(&RunnerError::ScannerError)?;

        self.parser.append(tokens);
        let ast = self.parser.program().map_err(&RunnerError::ParserError)?;
        // println!("{:#?}", ast);

        self.interpreter.interpret(ast).map_err(&RunnerError::RuntimeError)
    }

    pub fn run_file(&mut self, path: &str) -> Result<()> {
        let contents = read_to_string(path).expect("Could not read file");
        self.run(contents)
    }

    pub fn run_prompt(&mut self) {
        let stdin = io::stdin();
        print!("> ");

        for line in stdin.lock().lines() {
            let line = line.expect("Could not read line") + "\n";
            self.run(line).unwrap_or_else(|err| println!("{:?}", err));
            print!("> ")
        }
    }
}