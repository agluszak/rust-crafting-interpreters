use std::iter::Peekable;
use std::slice::Iter;

use crate::expr::BinaryOperator::*;
use crate::expr::Expr::{Binary, Grouping, Unary};
use crate::expr::Literal::{Boolean, Number, String};
use crate::expr::UnaryOperator::{BooleanNegate, NumericNegate};
use crate::expr::{BinaryOperator, Expr, Literal, UnaryOperator};
use crate::parser::ParserError::{MissingExpectedToken, UnexpectedEOF};
use crate::token::{Token, TokenType};

#[derive(Debug, PartialEq)]
pub enum ParserError {
    UnexpectedEOF,
    MissingExpectedToken(TokenType), // TODO: report location
}

type Result<T> = std::result::Result<T, ParserError>;

// TODO: separate parser logic (i.e. grammar) from the scanner logic
pub struct Parser<'a, I>
where
    I: Iterator<Item = &'a Token>,
{
    src: Peekable<I>,
}

impl<'a> Parser<'a, Iter<'a, Token>> {
    pub fn new(src: Iter<'a, Token>) -> Self {
        Self {
            src: src.peekable(),
        }
    }
}

impl<'a, I> Parser<'a, I>
where
    I: Iterator<Item = &'a Token>,
{
    fn next(&mut self) -> Option<&Token> {
        self.src.next()
    }

    pub fn peek(&mut self) -> Option<&&Token> {
        self.src.peek()
    }

    pub fn consume_token(&mut self, expected: TokenType) -> Option<&Token> {
        self.consume_one_of(&[expected])
    }

    fn consume_token_expect(&mut self, expected: TokenType) -> Result<&Token> {
        self.consume_token(expected.clone())
            .ok_or(MissingExpectedToken(expected))
    }

    pub fn consume_one_of(&mut self, expected_types: &[TokenType]) -> Option<&Token> {
        if let Some(actual) = self.peek() {
            for expected in expected_types {
                if actual.token_type.is_same_variant(expected) {
                    return self.next();
                }
            }
        }

        None
    }

    /// Advances while the peek matches `predicate`
    pub fn advance_while<P>(&mut self, predicate: P) -> bool
    where
        P: Fn(&Token) -> bool,
    {
        while let Some(c) = self.peek() {
            if !predicate(c) {
                return true;
            }
            self.next();
        }
        return false;
    }

    fn token_type_to_binary_operator(token_type: &TokenType) -> BinaryOperator {
        match token_type {
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

    fn token_type_to_unary_operator(token_type: &TokenType) -> UnaryOperator {
        match token_type {
            TokenType::Bang => BooleanNegate,
            TokenType::Minus => NumericNegate,
            _ => unreachable!(),
        }
    }

    fn token_type_to_literal(token_type: &TokenType) -> Literal {
        match token_type {
            TokenType::String(s) => String(s.clone()), // TODO get rid of this clone?
            TokenType::Number(n) => Number(*n),
            TokenType::True => Boolean(true),
            TokenType::False => Boolean(false),
            _ => unreachable!(),
        }
    }

    fn parse_binary_operation<ParseFunction>(
        &mut self,
        token_types: &[TokenType],
        higher_precedence_parser: ParseFunction,
    ) -> Result<Box<Expr>>
    where
        ParseFunction: Fn(&mut Self) -> Result<Box<Expr>>,
    {
        let mut expr = higher_precedence_parser(self)?;
        while let Some(operator_token) = self.consume_one_of(token_types) {
            let token_type = &operator_token.token_type.clone();
            let right = higher_precedence_parser(self)?;
            let operator = Self::token_type_to_binary_operator(token_type);
            expr = Box::new(Binary(expr, operator, right))
        }

        Ok(expr)
    }

    fn parse_unary_operation<ParseFunction>(
        &mut self,
        token_types: &[TokenType],
        higher_precedence_parser: ParseFunction,
    ) -> Result<Box<Expr>>
    where
        ParseFunction: Fn(&mut Self) -> Result<Box<Expr>>,
    {
        if let Some(operator_token) = self.consume_one_of(token_types) {
            let token_type = &operator_token.token_type.clone();
            let right = higher_precedence_parser(self)?;
            let operator = Self::token_type_to_unary_operator(token_type);
            return Ok(Box::new(Unary(operator, right)));
        }

        higher_precedence_parser(self)
    }

    fn expression(&mut self) -> Result<Box<Expr>> {
        self.equality()
    }

    fn equality(&mut self) -> Result<Box<Expr>> {
        self.parse_binary_operation(
            &[TokenType::BangEqual, TokenType::EqualEqual],
            &Self::comparison,
        )
    }

    fn comparison(&mut self) -> Result<Box<Expr>> {
        self.parse_binary_operation(
            &[
                TokenType::Greater,
                TokenType::GreaterEqual,
                TokenType::Less,
                TokenType::LessEqual,
            ],
            &Self::term,
        )
    }

    fn term(&mut self) -> Result<Box<Expr>> {
        self.parse_binary_operation(&[TokenType::Minus, TokenType::Plus], &Self::factor)
    }

    fn factor(&mut self) -> Result<Box<Expr>> {
        self.parse_binary_operation(&[TokenType::Star, TokenType::Slash], &Self::unary)
    }

    fn unary(&mut self) -> Result<Box<Expr>> {
        self.parse_unary_operation(&[TokenType::Minus, TokenType::Bang], &Self::primary)
    }

    fn primary(&mut self) -> Result<Box<Expr>> {
        if let Some(operator_token) = self.consume_one_of(&[
            TokenType::False,
            TokenType::True,
            TokenType::Number(0.0),
            TokenType::String("".to_string()),
        ]) {
            let literal = Self::token_type_to_literal(&operator_token.token_type);
            return Ok(Box::new(Expr::Literal(literal)));
        }

        if let Some(_) = self.consume_token(TokenType::LeftParen) {
            let expr = self.expression()?;
            self.consume_token_expect(TokenType::RightParen)?;
            return Ok(Box::new(Grouping(expr)));
        }

        return Err(UnexpectedEOF);
    }
}

pub fn parse(tokens: Iter<Token>) -> Result<Box<Expr>> {
    let mut parser = Parser::new(tokens);
    parser.expression()
}

#[cfg(test)]
mod tests {
    use std::slice::Iter;

    use crate::expr::BinaryOperator::*;
    use crate::expr::Expr::{Binary, Grouping, Unary};
    use crate::expr::Literal::{Number, String};
    use crate::expr::UnaryOperator::NumericNegate;
    use crate::expr::{Expr, Literal, UnaryOperator};
    use crate::parser::Parser;
    use crate::token::TokenType::*;
    use crate::token::{Location, Token, TokenType};

    // !true || false
    // > Unary(
    // BooleanNegate,
    // Literal(
    // Boolean(
    // true,
    // ),
    // ),
    // )

    #[test]
    fn single_expression() {
        let tokens = [
            Token {
                token_type: TokenType::Number(1.0),
                lexeme: "1".to_string(),
                location: Location::new(1, 1),
            },
            Token {
                token_type: Plus,
                lexeme: "+".to_string(),
                location: Location::new(1, 3),
            },
            Token {
                token_type: TokenType::Number(2.0),
                lexeme: "2".to_string(),
                location: Location::new(1, 5),
            },
            Token {
                token_type: Star,
                lexeme: "*".to_string(),
                location: Location::new(1, 7),
            },
            Token {
                token_type: LeftParen,
                lexeme: "(".to_string(),
                location: Location::new(1, 9),
            },
            Token {
                token_type: Minus,
                lexeme: "-".to_string(),
                location: Location::new(1, 10),
            },
            Token {
                token_type: TokenType::Number(3.0),
                lexeme: "3".to_string(),
                location: Location::new(1, 11),
            },
            Token {
                token_type: RightParen,
                lexeme: ")".to_string(),
                location: Location::new(1, 12),
            },
        ];

        let expected_expr = Ok(Box::from(Binary(
            Box::from(Expr::Literal(Number(1.0))),
            Add,
            Box::from(Binary(
                Box::from(Expr::Literal(Number(2.0))),
                Multiply,
                Box::from(Grouping(Box::from(Unary(
                    NumericNegate,
                    Box::from(Expr::Literal(Number(3.0))),
                )))),
            )),
        )));

        assert_eq!(Parser::<Iter<Token>>::parse(tokens.iter()), expected_expr);
    }
}
