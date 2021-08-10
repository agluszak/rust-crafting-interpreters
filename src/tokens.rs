#[derive(Debug, Clone, PartialEq)]
pub enum TokenType {
    // Single-character tokens.
    LeftParen,
    RightParen,
    LeftBrace,
    RightBrace,
    Comma,
    Dot,
    Minus,
    Plus,
    Semicolon,
    Slash,
    Star,

    // One or two character tokens.
    Bang,
    BangEqual,
    Equal,
    EqualEqual,
    Greater,
    GreaterEqual,
    Less,
    LessEqual,

    // Literals.
    Identifier(String),
    String(String),
    Number(f64),

    // Keywords.
    And,
    Class,
    Else,
    False,
    Fun,
    For,
    If,
    Nil,
    Or,
    Print,
    Return,
    Super,
    This,
    True,
    Var,
    While,
}

#[derive(Debug, PartialEq, Clone)]
pub struct Token {
    pub token_type: TokenType,
    pub lexeme: String,
    pub location: Location,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Location {
    line: usize,
    column: usize,
}

impl Location {
    pub fn initial() -> Self {
        Self::new(1, 1)
    }

    pub fn new(line: usize, column: usize) -> Self {
        Self {
            line: line,
            column: column,
        }
    }

    pub fn line(&self) -> usize {
        self.line
    }

    pub fn column(&self) -> usize {
        self.column
    }

    pub fn increment_line(&mut self) {
        self.line += 1;
    }

    pub fn increment_column(&mut self) {
        self.column += 1;
    }

    pub fn reset_column(&mut self) {
        self.column = 1;
    }
}
