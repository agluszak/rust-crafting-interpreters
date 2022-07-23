use crate::expr::{BinaryOperator, Literal, UnaryOperator};

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

impl TokenType {
    pub fn is_same_variant(&self, other: &Self) -> bool {
        use std::mem::discriminant;
        discriminant(self) == discriminant(other)
    }

    pub fn to_binary_operator(&self) -> BinaryOperator {
        use BinaryOperator::*;
        match &self {
            TokenType::BangEqual => NotEqual,
            TokenType::EqualEqual => Equal,
            TokenType::Greater => Greater,
            TokenType::GreaterEqual => GreaterEqual,
            TokenType::Less => Less,
            TokenType::LessEqual => LessEqual,
            TokenType::Minus => Subtract,
            TokenType::Plus => Add,
            TokenType::Slash => Divide,
            TokenType::Star => Multiply,
            _ => unreachable!(),
        }
    }

    pub fn to_unary_operator(&self) -> UnaryOperator {
        use UnaryOperator::*;
        match &self {
            TokenType::Bang => BooleanNegate,
            TokenType::Minus => NumericNegate,
            _ => unreachable!(),
        }
    }

    pub fn to_literal(&self) -> Literal {
        use Literal::*;
        match &self {
            TokenType::String(s) => String(s.clone()), // TODO: get rid of this clone?
            TokenType::Number(n) => Number(*n),
            TokenType::True => Boolean(true),
            TokenType::False => Boolean(false),
            _ => unreachable!(),
        }
    }

    pub fn to_variable(&self) -> String {
        match &self {
            TokenType::Identifier(name) => name.clone(),
            _ => unreachable!(),
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct Token {
    pub token_type: TokenType,
    pub lexeme: String,
    pub location: Location,
}

impl Token {
    pub fn new(token_type: TokenType, lexeme: String, location: Location) -> Self {
        Self {
            token_type,
            lexeme,
            location,
        }
    }

    pub fn end_location(&self) -> Location {
        Location::new(
            self.location.line(),
            self.location.column() + self.lexeme.chars().count(),
        )
    }
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
        Self { line, column }
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
