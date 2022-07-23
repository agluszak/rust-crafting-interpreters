use std::string::String;

use phf::phf_map;

use crate::scanner::ScannerError::{BadCharacter, NumberParsing, UnclosedString};
use crate::token::TokenType::*;
use crate::token::{Location, Token, TokenType};

use self::hidden::CharReader;

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

// TODO: impl Error
// TODO: split into type and location
#[derive(Debug, PartialEq)]
pub enum ScannerError {
    UnclosedString,
    BadCharacter(Location),
    NumberParsing(Location),
}

mod hidden {
    use std::collections::VecDeque;

    use crate::token::Location;

    // based on https://github.com/toyboot4e/loxrs/blob/master/loxrs_treewalk/src/lexer/scanner.rs
    pub struct CharReader {
        src: VecDeque<char>,
        location: Location,
        lexeme: String,
    }

    impl Iterator for CharReader {
        type Item = char;
        fn next(&mut self) -> Option<char> {
            let next = self.src.pop_front();
            if let Some(c) = next {
                self.lexeme.push(c);
                match c {
                    '\n' => {
                        self.location.increment_line();
                        self.location.reset_column();
                    }
                    _ => {
                        self.location.increment_column();
                    }
                };
            }
            next
        }
    }

    impl CharReader {
        pub fn new() -> Self {
            CharReader {
                src: VecDeque::new(),
                location: Location::initial(),
                lexeme: String::new(),
            }
        }

        pub fn append(&mut self, src: String) {
            self.src.extend(src.chars())
        }

        pub fn location(&self) -> Location {
            self.location
        }

        pub fn lexeme(&self) -> &str {
            &self.lexeme
        }

        pub fn peek(&mut self) -> Option<&char> {
            self.src.front()
        }

        pub fn clear_lexeme(&mut self) {
            self.lexeme.clear();
        }

        /// Returns true if the expected char was consumed
        pub fn consume_char(&mut self, expected: char) -> bool {
            if Some(&expected) == self.peek() {
                self.next();
                true
            } else {
                false
            }
        }

        /// Advances while the peek matches `predicate`; peeks char by char
        pub fn advance_while<P>(&mut self, predicate: P) -> bool
        where
            P: Fn(char) -> bool,
        {
            while let Some(&c) = self.peek() {
                if !predicate(c) {
                    return true;
                }
                self.next();
            }
            false
        }
    }
}

type Result<T> = std::result::Result<T, ScannerError>;

pub struct Scanner {
    char_reader: CharReader,
}

impl Scanner {
    pub fn new() -> Self {
        Self {
            char_reader: CharReader::new(),
        }
    }

    pub fn append(&mut self, source: String) {
        self.char_reader.append(source);
    }

    pub fn scan_tokens(&mut self) -> std::result::Result<Vec<Token>, Vec<ScannerError>> {
        let mut tokens = Vec::new();
        let mut errors = Vec::new();
        loop {
            match self.scan_token() {
                Ok(Some(token)) => tokens.push(token),
                Err(error) => errors.push(error),
                Ok(None) => break,
            }
        }

        if !errors.is_empty() {
            Err(errors)
        } else {
            Ok(tokens)
        }
    }

    fn scan_token(&mut self) -> Result<Option<Token>> {
        loop {
            self.char_reader.clear_lexeme();
            let start_location = self.char_reader.location();

            let c = if let Some(c) = self.char_reader.next() {
                c
            } else {
                return Ok(None);
            };

            let token_type = match c {
                '(' => LeftParen,
                ')' => RightParen,
                '{' => LeftBrace,
                '}' => RightBrace,
                ',' => Comma,
                '.' => Dot,
                '-' => Minus,
                '+' => Plus,
                ';' => Semicolon,
                '*' => Star,
                '!' => self.compare_next_char('=', BangEqual, Bang),
                '=' => self.compare_next_char('=', EqualEqual, Equal),
                '<' => self.compare_next_char('=', LessEqual, Less),
                '>' => self.compare_next_char('=', GreaterEqual, Greater),
                '/' => match self.handle_slash() {
                    None => continue,
                    Some(token_type) => token_type,
                },
                // Ignore whitespace
                ' ' | '\r' | '\t' | '\n' => continue,
                // String
                '"' => self.string()?,
                // Default
                c if c.is_ascii_digit() => self.number()?,
                c if c.is_ascii_alphabetic() || c == '_' => self.identifier(),
                _ => return Err(BadCharacter(self.char_reader.location())),
            };

            return Ok(Some(Token {
                token_type,
                lexeme: self.char_reader.lexeme().to_string(),
                location: start_location,
            }));
        }
    }

    fn compare_next_char(
        &mut self,
        expected: char,
        if_true: TokenType,
        if_false: TokenType,
    ) -> TokenType {
        if self.char_reader.consume_char(expected) {
            if_true
        } else {
            if_false
        }
    }

    fn can_be_identifier(c: char) -> bool {
        c.is_ascii_alphanumeric() || c == '_'
    }

    // TODO: range comments
    fn handle_slash(&mut self) -> Option<TokenType> {
        if self.char_reader.consume_char('/') {
            self.char_reader.advance_while(|c| c != '\n');
            None
        } else {
            Some(Slash)
        }
    }

    fn identifier(&mut self) -> TokenType {
        self.char_reader.advance_while(Self::can_be_identifier);

        let text = self.char_reader.lexeme();
        KEYWORDS
            .get(text)
            .cloned()
            .unwrap_or_else(|| Identifier(text.to_string()))
    }

    fn number(&mut self) -> Result<TokenType> {
        self.char_reader.advance_while(|c| c.is_ascii_digit());
        if self.char_reader.consume_char('.') {
            self.char_reader.advance_while(|c| c.is_ascii_digit());
        }

        self.char_reader
            .lexeme()
            .parse()
            .map_err(|_| NumberParsing(self.char_reader.location()))
            .map(Number)
    }

    fn string(&mut self) -> Result<TokenType> {
        loop {
            match self.char_reader.next() {
                None => return Err(UnclosedString),
                Some('"') => {
                    return Ok(String(
                        self.char_reader
                            .lexeme()
                            .chars()
                            .skip(1)
                            .take(self.char_reader.lexeme().len() - 2)
                            .collect(),
                    ));
                }
                _ => continue,
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::scanner::ScannerError::{BadCharacter, UnclosedString};
    use crate::scanner::{Scanner, ScannerError};
    use crate::token::TokenType::*;
    use crate::token::{Location, Token};
    use std::fmt::Debug;

    fn compare_vecs<T: Debug + PartialEq>(expected: Vec<T>, actual: Vec<T>) {
        assert_eq!(expected.len(), actual.len());
        for (expected, actual) in expected.iter().zip(&actual) {
            assert_eq!(expected, actual);
        }
        assert_eq!(expected, actual);
    }

    fn scanner_test(source: &str, expected_result: Result<Vec<Token>, Vec<ScannerError>>) {
        let mut scanner = Scanner::new();
        scanner.append(source.to_string());
        let result = scanner.scan_tokens();

        match expected_result {
            Ok(expected) => {
                assert!(result.is_ok());
                let actual = result.unwrap();
                compare_vecs(expected, actual);
            }
            Err(expected) => {
                assert!(result.is_err());
                let actual = result.unwrap_err();
                compare_vecs(expected, actual);
            }
        }
    }

    #[test]
    fn simple_test() {
        static SOURCE: &str = r#"var _how_are_you = "good";
// this is a comment
var       num = 2 + 3.1;
if (true or false) { print _how_are_you; }
"#;

        let expected_tokens = vec![
            Token::new(Var, "var".to_string(), Location::new(1, 1)),
            Token::new(
                Identifier("_how_are_you".to_string()),
                "_how_are_you".to_string(),
                Location::new(1, 5),
            ),
            Token::new(Equal, "=".to_string(), Location::new(1, 18)),
            Token::new(
                String("good".to_string()),
                "\"good\"".to_string(),
                Location::new(1, 20),
            ),
            Token::new(Semicolon, ";".to_string(), Location::new(1, 26)),
            Token::new(Var, "var".to_string(), Location::new(3, 1)),
            Token::new(
                Identifier("num".to_string()),
                "num".to_string(),
                Location::new(3, 11),
            ),
            Token::new(Equal, "=".to_string(), Location::new(3, 15)),
            Token::new(Number(2.0), "2".to_string(), Location::new(3, 17)),
            Token::new(Plus, "+".to_string(), Location::new(3, 19)),
            Token::new(Number(3.1), "3.1".to_string(), Location::new(3, 21)),
            Token::new(Semicolon, ";".to_string(), Location::new(3, 24)),
            Token::new(If, "if".to_string(), Location::new(4, 1)),
            Token::new(LeftParen, "(".to_string(), Location::new(4, 4)),
            Token::new(True, "true".to_string(), Location::new(4, 5)),
            Token::new(Or, "or".to_string(), Location::new(4, 10)),
            Token::new(False, "false".to_string(), Location::new(4, 13)),
            Token::new(RightParen, ")".to_string(), Location::new(4, 18)),
            Token::new(LeftBrace, "{".to_string(), Location::new(4, 20)),
            Token::new(Print, "print".to_string(), Location::new(4, 22)),
            Token::new(
                Identifier("_how_are_you".to_string()),
                "_how_are_you".to_string(),
                Location::new(4, 28),
            ),
            Token::new(Semicolon, ";".to_string(), Location::new(4, 40)),
            Token::new(RightBrace, "}".to_string(), Location::new(4, 42)),
        ];

        scanner_test(SOURCE, Ok(expected_tokens))
    }

    #[test]
    fn unclosed_string() {
        scanner_test("\"", Err(vec![UnclosedString]))
    }

    #[test]
    fn nonsense_chars() {
        // let expected_tokens = vec![
        //     Token::new(Identifier("qwerty".to_string()), "qwerty".to_string(), Location::new(1, 4)),
        //     Token::new(If, "if".to_string(), Location::new(1, 11)),
        //     Token::new(Star, "*".to_string(), Location::new(1, 15)),
        //     Token::new(True, "true".to_string(), Location::new(1, 17)),
        // ];

        let expected_errors = vec![
            BadCharacter(Location::new(1, 2)),
            BadCharacter(Location::new(1, 3)),
            BadCharacter(Location::new(1, 4)),
            BadCharacter(Location::new(1, 15)),
        ];

        scanner_test("$#@qwerty if $* true", Err(expected_errors))
    }

    #[test]
    fn divide() {
        static SOURCE: &str = r#"2 / 3 // comment
/ / not_a_comment
"#;

        let expected_tokens = vec![
            Token::new(Number(2.0), "2".to_string(), Location::new(1, 1)),
            Token::new(Slash, "/".to_string(), Location::new(1, 3)),
            Token::new(Number(3.0), "3".to_string(), Location::new(1, 5)),
            Token::new(Slash, "/".to_string(), Location::new(2, 1)),
            Token::new(Slash, "/".to_string(), Location::new(2, 3)),
            Token::new(
                Identifier("not_a_comment".to_string()),
                "not_a_comment".to_string(),
                Location::new(2, 5),
            ),
        ];

        scanner_test(SOURCE, Ok(expected_tokens))
    }

    #[test]
    fn number_without_point() {
        let expected_tokens = vec![
            Token::new(Number(5.0), "5.".to_string(), Location::new(1, 1)),
            Token::new(Number(2.0), "2".to_string(), Location::new(1, 4)),
            Token::new(Number(3.1), "3.1".to_string(), Location::new(1, 6)),
            Token::new(Number(1.0), "1".to_string(), Location::new(1, 10)),
        ];

        scanner_test("5. 2 3.1 1", Ok(expected_tokens))
    }

    #[test]
    fn number_with_two_points() {
        let expected_tokens = vec![
            Token::new(Number(3.0), "3.".to_string(), Location::new(1, 1)),
            Token::new(Dot, ".".to_string(), Location::new(1, 3)),
            Token::new(Number(14.0), "14".to_string(), Location::new(1, 4)),
        ];

        scanner_test("3..14", Ok(expected_tokens))
    }

    #[test]
    fn fibonacci() {
        static SOURCE: &str = r#"var a = 0;
var temp;

for (var b = 1; a < 10000; b = temp + b) {
  print a;
  temp = a;
  a = b;
}
"#;

        let expected_tokens = vec![
            Token::new(Var, "var".to_string(), Location::new(1, 1)),
            Token::new(
                Identifier("a".to_string()),
                "a".to_string(),
                Location::new(1, 5),
            ),
            Token::new(Equal, "=".to_string(), Location::new(1, 7)),
            Token::new(Number(0.0), "0".to_string(), Location::new(1, 9)),
            Token::new(Semicolon, ";".to_string(), Location::new(1, 10)),
            Token::new(Var, "var".to_string(), Location::new(2, 1)),
            Token::new(
                Identifier("temp".to_string()),
                "temp".to_string(),
                Location::new(2, 5),
            ),
            Token::new(Semicolon, ";".to_string(), Location::new(2, 9)),
            Token::new(For, "for".to_string(), Location::new(4, 1)),
            Token::new(LeftParen, "(".to_string(), Location::new(4, 5)),
            Token::new(Var, "var".to_string(), Location::new(4, 6)),
            Token::new(
                Identifier("b".to_string()),
                "b".to_string(),
                Location::new(4, 10),
            ),
            Token::new(Equal, "=".to_string(), Location::new(4, 12)),
            Token::new(Number(1.0), "1".to_string(), Location::new(4, 14)),
            Token::new(Semicolon, ";".to_string(), Location::new(4, 15)),
            Token::new(
                Identifier("a".to_string()),
                "a".to_string(),
                Location::new(4, 17),
            ),
            Token::new(Less, "<".to_string(), Location::new(4, 19)),
            Token::new(Number(10000.0), "10000".to_string(), Location::new(4, 21)),
            Token::new(Semicolon, ";".to_string(), Location::new(4, 26)),
            Token::new(
                Identifier("b".to_string()),
                "b".to_string(),
                Location::new(4, 28),
            ),
            Token::new(Equal, "=".to_string(), Location::new(4, 30)),
            Token::new(
                Identifier("temp".to_string()),
                "temp".to_string(),
                Location::new(4, 32),
            ),
            Token::new(Plus, "+".to_string(), Location::new(4, 37)),
            Token::new(
                Identifier("b".to_string()),
                "b".to_string(),
                Location::new(4, 39),
            ),
            Token::new(RightParen, ")".to_string(), Location::new(4, 40)),
            Token::new(LeftBrace, "{".to_string(), Location::new(4, 42)),
            Token::new(Print, "print".to_string(), Location::new(5, 3)),
            Token::new(
                Identifier("a".to_string()),
                "a".to_string(),
                Location::new(5, 9),
            ),
            Token::new(Semicolon, ";".to_string(), Location::new(5, 10)),
            Token::new(
                Identifier("temp".to_string()),
                "temp".to_string(),
                Location::new(6, 3),
            ),
            Token::new(Equal, "=".to_string(), Location::new(6, 8)),
            Token::new(
                Identifier("a".to_string()),
                "a".to_string(),
                Location::new(6, 10),
            ),
            Token::new(Semicolon, ";".to_string(), Location::new(6, 11)),
            Token::new(
                Identifier("a".to_string()),
                "a".to_string(),
                Location::new(7, 3),
            ),
            Token::new(Equal, "=".to_string(), Location::new(7, 5)),
            Token::new(
                Identifier("b".to_string()),
                "b".to_string(),
                Location::new(7, 7),
            ),
            Token::new(Semicolon, ";".to_string(), Location::new(7, 8)),
            Token::new(RightBrace, "}".to_string(), Location::new(8, 1)),
        ];

        scanner_test(SOURCE, Ok(expected_tokens))
    }
}
