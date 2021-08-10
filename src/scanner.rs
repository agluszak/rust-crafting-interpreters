use std::str::Chars;
use std::string::String;

use phf::phf_map;

use crate::scanner::ScannerError::{BadCharacter, NumberParsing, UnclosedString};
use crate::tokens::TokenType::*;
use crate::tokens::{Location, Token, TokenType};

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
    BadCharacter(Location),
    NumberParsing(Location),
}

mod hidden {
    use crate::tokens::Location;
    use itertools::{multipeek, MultiPeek};
    use std::str::Chars;

    // based on https://github.com/toyboot4e/loxrs/blob/master/loxrs_treewalk/src/lexer/scanner.rs
    pub struct CharReader<I>
    where
        I: Iterator<Item = char>,
    {
        src: MultiPeek<I>,
        location: Location,
        lexeme: String,
    }

    impl<'a> CharReader<Chars<'a>> {
        pub fn new(src: &'a str) -> Self {
            CharReader {
                src: multipeek(src.chars()),
                location: Location::initial(),
                lexeme: String::new(),
            }
        }
    }

    impl<I> Iterator for CharReader<I>
    where
        I: Iterator<Item = char>,
    {
        type Item = char;
        fn next(&mut self) -> Option<char> {
            let next = self.src.next();
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

    impl<I> CharReader<I>
    where
        I: Iterator<Item = char>,
    {
        pub fn location(&self) -> Location {
            self.location
        }

        pub fn lexeme(&self) -> &str {
            &self.lexeme
        }

        pub fn peek(&mut self) -> Option<&char> {
            self.src.reset_peek();
            self.src.peek()
        }

        pub fn peek_next(&mut self) -> Option<&char> {
            self.src.reset_peek();
            self.src.peek();
            self.src.peek()
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
            return false;
        }
    }
}

type Result<T> = std::result::Result<T, ScannerError>;

use self::hidden::CharReader;

struct Scanner<'a> {
    char_reader: CharReader<Chars<'a>>,
}

impl<'a> Scanner<'a> {
    fn new(src: &'a str) -> Self {
        Self {
            char_reader: CharReader::new(src),
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

    // TODO range comments
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
            .unwrap_or(Identifier(text.to_string()))
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
                    ))
                }
                _ => continue,
            }
        }
    }
}

pub fn scan_tokens(source: String) -> (Vec<Token>, Vec<ScannerError>) {
    let mut scanner = Scanner::new(&*source);
    let mut tokens = Vec::new();
    let mut errors = Vec::new();
    loop {
        match scanner.scan_token() {
            Ok(Some(token)) => tokens.push(token),
            Err(error) => errors.push(error),
            Ok(None) => break,
        }
    }

    (tokens, errors)
}

#[cfg(test)]
mod tests {
    use crate::scanner::scan_tokens;
    use crate::scanner::ScannerError::{BadCharacter, UnclosedString};
    use crate::tokens::TokenType::*;
    use crate::tokens::{Location, Token};
    use itertools::zip;

    #[test]
    fn simple_test() {
        static SOURCE: &str = r#"var _how_are_you = "good";
// this is a comment
var       num = 2 + 3.14;
if (true or false) { print _how_are_you; }
"#;

        let (tokens, errors) = scan_tokens(SOURCE.to_string());
        assert!(errors.is_empty());

        let expected_tokens = vec![
            Token {
                token_type: Var,
                lexeme: "var".to_string(),
                location: Location::new(1, 1),
            },
            Token {
                token_type: Identifier("_how_are_you".to_string()),
                lexeme: "_how_are_you".to_string(),
                location: Location::new(1, 5),
            },
            Token {
                token_type: Equal,
                lexeme: "=".to_string(),
                location: Location::new(1, 18),
            },
            Token {
                token_type: String("good".to_string()),
                lexeme: "\"good\"".to_string(),
                location: Location::new(1, 20),
            },
            Token {
                token_type: Semicolon,
                lexeme: ";".to_string(),
                location: Location::new(1, 26),
            },
            Token {
                token_type: Var,
                lexeme: "var".to_string(),
                location: Location::new(3, 1),
            },
            Token {
                token_type: Identifier("num".to_string()),
                lexeme: "num".to_string(),
                location: Location::new(3, 11),
            },
            Token {
                token_type: Equal,
                lexeme: "=".to_string(),
                location: Location::new(3, 15),
            },
            Token {
                token_type: Number(2.0),
                lexeme: "2".to_string(),
                location: Location::new(3, 17),
            },
            Token {
                token_type: Plus,
                lexeme: "+".to_string(),
                location: Location::new(3, 19),
            },
            Token {
                token_type: Number(3.14),
                lexeme: "3.14".to_string(),
                location: Location::new(3, 21),
            },
            Token {
                token_type: Semicolon,
                lexeme: ";".to_string(),
                location: Location::new(3, 25),
            },
            Token {
                token_type: If,
                lexeme: "if".to_string(),
                location: Location::new(4, 1),
            },
            Token {
                token_type: LeftParen,
                lexeme: "(".to_string(),
                location: Location::new(4, 4),
            },
            Token {
                token_type: True,
                lexeme: "true".to_string(),
                location: Location::new(4, 5),
            },
            Token {
                token_type: Or,
                lexeme: "or".to_string(),
                location: Location::new(4, 10),
            },
            Token {
                token_type: False,
                lexeme: "false".to_string(),
                location: Location::new(4, 13),
            },
            Token {
                token_type: RightParen,
                lexeme: ")".to_string(),
                location: Location::new(4, 18),
            },
            Token {
                token_type: LeftBrace,
                lexeme: "{".to_string(),
                location: Location::new(4, 20),
            },
            Token {
                token_type: Print,
                lexeme: "print".to_string(),
                location: Location::new(4, 22),
            },
            Token {
                token_type: Identifier("_how_are_you".to_string()),
                lexeme: "_how_are_you".to_string(),
                location: Location::new(4, 28),
            },
            Token {
                token_type: Semicolon,
                lexeme: ";".to_string(),
                location: Location::new(4, 40),
            },
            Token {
                token_type: RightBrace,
                lexeme: "}".to_string(),
                location: Location::new(4, 42),
            },
        ];

        for (expected, actual) in zip(expected_tokens, tokens) {
            assert_eq!(expected, actual);
        }
    }

    #[test]
    fn unclosed_string() {
        let (tokens, errors) = scan_tokens("\"".to_string());
        assert!(tokens.is_empty());
        assert_eq!(errors, vec![UnclosedString]);
    }

    #[test]
    fn nonsense_chars() {
        let (tokens, errors) = scan_tokens("$#@qwerty if $* true".to_string());
        let expected_tokens = vec![
            Token {
                token_type: Identifier("qwerty".to_string()),
                lexeme: "qwerty".to_string(),
                location: Location::new(1, 4),
            },
            Token {
                token_type: If,
                lexeme: "if".to_string(),
                location: Location::new(1, 11),
            },
            Token {
                token_type: Star,
                lexeme: "*".to_string(),
                location: Location::new(1, 15),
            },
            Token {
                token_type: True,
                lexeme: "true".to_string(),
                location: Location::new(1, 17),
            },
        ];

        let expected_errors = vec![
            BadCharacter(Location::new(1, 2)),
            BadCharacter(Location::new(1, 3)),
            BadCharacter(Location::new(1, 4)),
            BadCharacter(Location::new(1, 15)),
        ];

        for (expected, actual) in zip(expected_tokens, tokens) {
            assert_eq!(expected, actual);
        }

        for (expected, actual) in zip(expected_errors, errors) {
            assert_eq!(expected, actual);
        }
    }

    #[test]
    fn divide() {
        static SOURCE: &str = r#"2 / 3 // comment
/ / not_a_comment
"#;
        let (tokens, errors) = scan_tokens(SOURCE.to_string());
        assert!(errors.is_empty());

        let expected_tokens = vec![
            Token {
                token_type: Number(2.0),
                lexeme: "2".to_string(),
                location: Location::new(1, 1),
            },
            Token {
                token_type: Slash,
                lexeme: "/".to_string(),
                location: Location::new(1, 3),
            },
            Token {
                token_type: Number(3.0),
                lexeme: "3".to_string(),
                location: Location::new(1, 5),
            },
            Token {
                token_type: Slash,
                lexeme: "/".to_string(),
                location: Location::new(2, 1),
            },
            Token {
                token_type: Slash,
                lexeme: "/".to_string(),
                location: Location::new(2, 3),
            },
            Token {
                token_type: Identifier("not_a_comment".to_string()),
                lexeme: "not_a_comment".to_string(),
                location: Location::new(2, 5),
            },
        ];

        for (expected, actual) in zip(expected_tokens, tokens) {
            assert_eq!(expected, actual);
        }
    }

    #[test]
    fn number_without_point() {
        let (tokens, errors) = scan_tokens("5. 2 3.1 1".to_string());
        assert!(errors.is_empty());

        let expected_tokens = vec![
            Token {
                token_type: Number(5.0),
                lexeme: "5.".to_string(),
                location: Location::new(1, 1),
            },
            Token {
                token_type: Number(2.0),
                lexeme: "2".to_string(),
                location: Location::new(1, 4),
            },
            Token {
                token_type: Number(3.1),
                lexeme: "3.1".to_string(),
                location: Location::new(1, 6),
            },
            Token {
                token_type: Number(1.0),
                lexeme: "1".to_string(),
                location: Location::new(1, 10),
            },
        ];

        for (expected, actual) in zip(expected_tokens, tokens) {
            assert_eq!(expected, actual);
        }
    }

    #[test]
    fn number_with_two_points() {
        let (tokens, errors) = scan_tokens("3..14".to_string());
        assert!(errors.is_empty());

        let expected_tokens = vec![
            Token {
                token_type: Number(3.0),
                lexeme: "3.".to_string(),
                location: Location::new(1, 1),
            },
            Token {
                token_type: Dot,
                lexeme: ".".to_string(),
                location: Location::new(1, 3),
            },
            Token {
                token_type: Number(14.0),
                lexeme: "14".to_string(),
                location: Location::new(1, 4),
            },
        ];

        for (expected, actual) in zip(expected_tokens, tokens) {
            assert_eq!(expected, actual);
        }
    }
}
