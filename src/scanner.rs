use std::string::String;

use phf::phf_map;

use crate::scanner::ScannerError::{BadCharacter, UnclosedString};
use crate::tokens::TokenType::*;
use crate::tokens::{Token, TokenType};

static KEYWORDS: phf::Map<&'static str, TokenType> = phf_map! {
    "and" => And,
    "class" => Class,
    "else" => Else,
    "false" => False,
    "for" => For,
    "fun" => Fun,
    "if" => If,
    "nil" => Nil,
    "or" => Or,
    "print" => Print,
    "return" => Return,
    "super" => Super,
    "this" => This,
    "true" => True,
    "var" => Var,
    "while" => While,
};

#[derive(Debug, PartialEq)]
pub enum ScannerError {
    UnclosedString,
    BadCharacter { line: usize, start: usize },
}

struct Scanner {
    tokens: Vec<Token>,
    errors: Vec<ScannerError>,
    source: String,
    start: usize,
    current: usize,
    line: usize,
    column: usize,
}

impl Scanner {
    fn new(source: String) -> Self {
        Scanner {
            tokens: Vec::new(),
            errors: Vec::new(),
            source,
            start: 0,
            current: 0,
            line: 1,
            column: 1,
        }
    }

    fn is_at_end(&self) -> bool {
        self.current >= self.source.chars().count()
    }

    fn scan_token(&mut self) {
        let c = self.advance();
        match c {
            '(' => self.add_token(LeftParen),
            ')' => self.add_token(RightParen),
            '{' => self.add_token(LeftBrace),
            '}' => self.add_token(RightBrace),
            ',' => self.add_token(Comma),
            '.' => self.add_token(Dot),
            '-' => self.add_token(Minus),
            '+' => self.add_token(Plus),
            ';' => self.add_token(Semicolon),
            '*' => self.add_token(Star),
            '!' => {
                let token_type = if self.next_char_matches('=') {
                    BangEqual
                } else {
                    Bang
                };
                self.add_token(token_type);
            }
            '=' => {
                let token_type = if self.next_char_matches('=') {
                    EqualEqual
                } else {
                    Equal
                };
                self.add_token(token_type);
            }
            '<' => {
                let token_type = if self.next_char_matches('=') {
                    LessEqual
                } else {
                    Less
                };
                self.add_token(token_type);
            }
            '>' => {
                let token_type = if self.next_char_matches('=') {
                    GreaterEqual
                } else {
                    Greater
                };
                self.add_token(token_type);
            }
            '/' => {
                if self.next_char_matches('/') {
                    while self.peek() != '\n' && !self.is_at_end() {
                        self.advance();
                    }
                } else {
                    self.add_token(Slash);
                }
            }
            // Ignore whitespace
            ' ' | '\r' | '\t' => {}
            // Newline
            '\n' => self.advance_line(),
            // String
            '"' => self.string(),
            // Default
            c => {
                if c.is_ascii_digit() {
                    self.number()
                } else if c.is_ascii_alphabetic() || c == '_' {
                    self.identifier()
                } else {
                    self.add_error(BadCharacter {
                        line: self.line,
                        start: self.start,
                    })
                }
            }
        }
    }

    fn advance_line(&mut self) {
        self.line += 1;
        self.column = 1;
    }

    fn can_be_identifier(c: char) -> bool {
        c.is_ascii_alphanumeric() || c == '_'
    }

    fn identifier(&mut self) {
        while Self::can_be_identifier(self.peek()) {
            self.advance();
        }

        let text = self.source_substring(self.start, self.current);
        let token_type = KEYWORDS.get(&text);
        if let Some(token_type) = token_type {
            self.add_token(token_type.clone());
        } else {
            self.add_token(Identifier(text));
        }
    }

    fn number(&mut self) {
        while self.peek().is_ascii_digit() {
            self.advance();
        }
        // Look for a fractional part.
        if self.peek() == '.' && self.peek_next().is_ascii_digit() {
            // Consume the "."
            self.advance();

            while self.peek().is_ascii_digit() {
                self.advance();
            }
        }

        let number = self
            .source_substring(self.start, self.current)
            .parse()
            .unwrap();

        self.add_token(Number(number));
    }

    fn string(&mut self) {
        while !self.is_at_end() && self.peek() != '"' {
            if self.peek() == '\n' {
                self.advance_line();
            }
            self.advance();
        }

        if self.is_at_end() {
            self.add_error(UnclosedString);
            return;
        }

        // Consume the closing quote
        self.advance();

        let literal = self.source_substring(self.start + 1, self.current - 1);
        self.add_token(String(literal));
    }

    fn peek(&self) -> char {
        if self.is_at_end() {
            return '\0';
        }
        self.source_char(self.current)
    }

    fn peek_next(&self) -> char {
        if self.current + 1 >= self.source.chars().count() {
            return '\0';
        }
        self.source_char(self.current + 1)
    }

    fn source_char(&self, nth: usize) -> char {
        self.source.chars().nth(nth).unwrap()
    }

    fn source_substring(&self, start: usize, end: usize) -> String {
        self.source.chars().skip(start).take(end - start).collect()
    }

    fn next_char_matches(&mut self, expected: char) -> bool {
        if self.is_at_end() {
            return false;
        }
        if self.source_char(self.current) != expected {
            return false;
        }
        self.current += 1;
        true
    }

    fn add_token(&mut self, token_type: TokenType) {
        let lexeme = self.source_substring(self.start, self.current);
        let lexeme_length = lexeme.chars().count();
        let token = Token {
            token_type,
            lexeme,
            line: self.line,
            column: self.column - lexeme_length,
        };
        self.tokens.push(token)
    }

    fn add_error(&mut self, error_type: ScannerError) {
        self.errors.push(error_type)
    }

    fn advance(&mut self) -> char {
        let char = self.source_char(self.current);
        self.current += 1;
        self.column += 1;
        char
    }
}

pub fn scan_tokens(source: String) -> (Vec<Token>, Vec<ScannerError>) {
    let mut scanner = Scanner::new(source);

    while !scanner.is_at_end() {
        // We are at the beginning of the next lexeme.
        scanner.start = scanner.current;
        scanner.scan_token()
    }

    let eof: Token = Token {
        token_type: EOF,
        lexeme: "".to_string(),
        line: scanner.line,
        column: 1,
    };
    scanner.tokens.push(eof);
    (scanner.tokens, scanner.errors)
}

#[cfg(test)]
mod tests {
    use crate::scanner::scan_tokens;
    use crate::scanner::ScannerError::UnclosedString;
    use crate::tokens::Token;
    use crate::tokens::TokenType::*;

    #[test]
    fn simple_test() {
        static SOURCE: &str = r#"var _how_are_you = "good";
// this is a comment
var       num = 2 + 3.14;
if (true or false) { print _how_are_you; }
"#;

        let (tokens, errors) = scan_tokens(SOURCE.to_string());
        assert!(errors.is_empty());
        let mut tokens_iter = tokens.iter();
        assert_eq!(
            Some(&Token {
                token_type: Var,
                lexeme: "var".to_string(),
                line: 1,
                column: 1
            }),
            tokens_iter.next()
        );
        assert_eq!(
            Some(&Token {
                token_type: Identifier("_how_are_you".to_string()),
                lexeme: "_how_are_you".to_string(),
                line: 1,
                column: 5
            }),
            tokens_iter.next()
        );
        assert_eq!(
            Some(&Token {
                token_type: Equal,
                lexeme: "=".to_string(),
                line: 1,
                column: 18
            }),
            tokens_iter.next()
        );
        assert_eq!(
            Some(&Token {
                token_type: String("good".to_string()),
                lexeme: "\"good\"".to_string(),
                line: 1,
                column: 20
            }),
            tokens_iter.next()
        );
        assert_eq!(
            Some(&Token {
                token_type: Semicolon,
                lexeme: ";".to_string(),
                line: 1,
                column: 26
            }),
            tokens_iter.next()
        );
        assert_eq!(
            Some(&Token {
                token_type: Var,
                lexeme: "var".to_string(),
                line: 3,
                column: 1
            }),
            tokens_iter.next()
        );
        assert_eq!(
            Some(&Token {
                token_type: Identifier("num".to_string()),
                lexeme: "num".to_string(),
                line: 3,
                column: 11
            }),
            tokens_iter.next()
        );
        assert_eq!(
            Some(&Token {
                token_type: Equal,
                lexeme: "=".to_string(),
                line: 3,
                column: 15
            }),
            tokens_iter.next()
        );
        assert_eq!(
            Some(&Token {
                token_type: Number(2.0),
                lexeme: "2".to_string(),
                line: 3,
                column: 17
            }),
            tokens_iter.next()
        );
        assert_eq!(
            Some(&Token {
                token_type: Plus,
                lexeme: "+".to_string(),
                line: 3,
                column: 19
            }),
            tokens_iter.next()
        );
        assert_eq!(
            Some(&Token {
                token_type: Number(3.14),
                lexeme: "3.14".to_string(),
                line: 3,
                column: 21
            }),
            tokens_iter.next()
        );
        assert_eq!(
            Some(&Token {
                token_type: Semicolon,
                lexeme: ";".to_string(),
                line: 3,
                column: 25
            }),
            tokens_iter.next()
        );
        assert_eq!(
            Some(&Token {
                token_type: If,
                lexeme: "if".to_string(),
                line: 4,
                column: 1
            }),
            tokens_iter.next()
        );
        assert_eq!(
            Some(&Token {
                token_type: LeftParen,
                lexeme: "(".to_string(),
                line: 4,
                column: 4
            }),
            tokens_iter.next()
        );
        assert_eq!(
            Some(&Token {
                token_type: True,
                lexeme: "true".to_string(),
                line: 4,
                column: 5
            }),
            tokens_iter.next()
        );
        assert_eq!(
            Some(&Token {
                token_type: Or,
                lexeme: "or".to_string(),
                line: 4,
                column: 10
            }),
            tokens_iter.next()
        );
        assert_eq!(
            Some(&Token {
                token_type: False,
                lexeme: "false".to_string(),
                line: 4,
                column: 13
            }),
            tokens_iter.next()
        );
        assert_eq!(
            Some(&Token {
                token_type: RightParen,
                lexeme: ")".to_string(),
                line: 4,
                column: 18
            }),
            tokens_iter.next()
        );
        assert_eq!(
            Some(&Token {
                token_type: LeftBrace,
                lexeme: "{".to_string(),
                line: 4,
                column: 20
            }),
            tokens_iter.next()
        );
        assert_eq!(
            Some(&Token {
                token_type: Print,
                lexeme: "print".to_string(),
                line: 4,
                column: 22
            }),
            tokens_iter.next()
        );
        assert_eq!(
            Some(&Token {
                token_type: Identifier("_how_are_you".to_string()),
                lexeme: "_how_are_you".to_string(),
                line: 4,
                column: 28
            }),
            tokens_iter.next()
        );
        assert_eq!(
            Some(&Token {
                token_type: Semicolon,
                lexeme: ";".to_string(),
                line: 4,
                column: 40
            }),
            tokens_iter.next()
        );
        assert_eq!(
            Some(&Token {
                token_type: RightBrace,
                lexeme: "}".to_string(),
                line: 4,
                column: 42
            }),
            tokens_iter.next()
        );
        assert_eq!(
            Some(&Token {
                token_type: EOF,
                lexeme: "".to_string(),
                line: 5,
                column: 1
            }),
            tokens_iter.next()
        );
        assert_eq!(None, tokens_iter.next());
    }

    #[test]
    fn unclosed_string() {
        let (tokens, errors) = scan_tokens("\"".to_string());
        assert_eq!(
            tokens,
            vec![Token {
                token_type: EOF,
                lexeme: "".to_string(),
                line: 1,
                column: 1
            }]
        );
        assert_eq!(errors, vec![UnclosedString]);
    }
}
