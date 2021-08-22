use std::collections::VecDeque;

use crate::expr::{BinaryOperator, Expr, Literal, UnaryOperator};
use crate::expr::BinaryOperator::*;
use crate::expr::Expr::{Binary, Grouping, Unary};
use crate::expr::Literal::{Boolean, Number};
use crate::expr::UnaryOperator::{BooleanNegate, NumericNegate};
use crate::parser::ParserError::{MissingExpectedToken, UnexpectedEOF};
use crate::stmt::Stmt;
use crate::token::{Token, TokenType};
use crate::token::TokenType::{Identifier, LeftBrace, Print, RightBrace, Semicolon, Var};

#[derive(Debug, PartialEq)]
pub enum ParserError {
    UnexpectedEOF,
    MissingExpectedToken(TokenType),
    // TODO: report location
    InvalidAssignmentTarget,
}

type Result<T> = std::result::Result<T, ParserError>;

// TODO: separate parser logic (i.e. grammar) from the scanning logic
pub struct Parser
{
    src: VecDeque<Token>,
}

impl Parser {
    pub fn new() -> Self {
        Self {
            src: VecDeque::new(),
        }
    }

    pub fn append(&mut self, tokens: Vec<Token>) {
        self.src.extend(tokens)
    }

    // Iterator-like methods

    fn next(&mut self) -> Option<Token> {
        self.src.pop_front()
    }

    pub fn peek(&mut self) -> Option<&Token> {
        self.src.front()
    }

    pub fn consume_token(&mut self, expected: TokenType) -> Option<Token> {
        self.consume_one_of(&[expected])
    }

    fn consume_token_expect(&mut self, expected: TokenType) -> Result<Token> {
        self.consume_token(expected.clone())
            .ok_or(MissingExpectedToken(expected))
    }

    pub fn consume_one_of(&mut self, expected_types: &[TokenType]) -> Option<Token> {
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

    // TODO: Try to get rid of this
    fn synchronize(&mut self) {
        self.advance_while(|token| !token.token_type.is_same_variant(&Semicolon));
        self.consume_token(Semicolon);
    }

    // Helper methods

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
            TokenType::String(s) => Literal::String(s.clone()), // TODO: get rid of this clone?
            TokenType::Number(n) => Number(*n),
            TokenType::True => Boolean(true),
            TokenType::False => Boolean(false),
            _ => unreachable!(),
        }
    }

    fn token_type_to_variable(token_type: &TokenType) -> String {
        match token_type {
            TokenType::Identifier(name) => name.clone(),
            _ => unreachable!()
        }
    }

    fn parse_binary_operation<ParseFunction>(
        &mut self,
        token_types: &[TokenType],
        higher_precedence_parser: ParseFunction,
    ) -> Result<Expr>
        where
            ParseFunction: Fn(&mut Self) -> Result<Expr>,
    {
        let mut expr = higher_precedence_parser(self)?;
        while let Some(operator_token) = self.consume_one_of(token_types) {
            let token_type = &operator_token.token_type.clone();
            let right = higher_precedence_parser(self)?;
            let operator = Self::token_type_to_binary_operator(token_type);
            expr = Binary(Box::new(expr), operator, Box::new(right))
        }

        Ok(expr)
    }

    fn parse_unary_operation<ParseFunction>(
        &mut self,
        token_types: &[TokenType],
        higher_precedence_parser: ParseFunction,
    ) -> Result<Expr>
        where
            ParseFunction: Fn(&mut Self) -> Result<Expr>,
    {
        if let Some(operator_token) = self.consume_one_of(token_types) {
            let token_type = &operator_token.token_type.clone();
            let right = higher_precedence_parser(self)?;
            let operator = Self::token_type_to_unary_operator(token_type);
            return Ok(Unary(operator, Box::new(right)));
        }

        higher_precedence_parser(self)
    }

    // Expression parsing methods

    fn expression(&mut self) -> Result<Expr> {
        self.equality()
    }

    fn assignment(&mut self) -> Result<Expr> {
        let expr = self.equality()?;

        if let Some(equals) = self.consume_token(TokenType::Equal) {
            let value = self.assignment()?;

            if let Expr::Variable(variable) = expr {
                return Ok(Expr::Assign(variable, Box::new(value)));
            }

            Err(ParserError::InvalidAssignmentTarget)
        } else {
            Ok(expr)
        }
    }

    fn equality(&mut self) -> Result<Expr> {
        self.parse_binary_operation(
            &[TokenType::BangEqual, TokenType::EqualEqual],
            &Self::comparison,
        )
    }

    fn comparison(&mut self) -> Result<Expr> {
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

    fn term(&mut self) -> Result<Expr> {
        self.parse_binary_operation(&[TokenType::Minus, TokenType::Plus], &Self::factor)
    }

    fn factor(&mut self) -> Result<Expr> {
        self.parse_binary_operation(&[TokenType::Star, TokenType::Slash], &Self::unary)
    }

    fn unary(&mut self) -> Result<Expr> {
        self.parse_unary_operation(&[TokenType::Minus, TokenType::Bang], &Self::primary)
    }

    fn primary(&mut self) -> Result<Expr> {
        if let Some(operator_token) = self.consume_one_of(&[
            TokenType::False,
            TokenType::True,
            TokenType::Number(0.0),
            TokenType::String("".to_string()),
        ]) {
            let literal = Self::token_type_to_literal(&operator_token.token_type);
            return Ok(Expr::Literal(literal));
        }

        if let Some(_) = self.consume_token(TokenType::LeftParen) {
            let expr = self.expression()?;
            self.consume_token_expect(TokenType::RightParen)?;
            return Ok(Grouping(Box::new(expr)));
        }

        if let Some(identifier) = self.consume_token(TokenType::Identifier("".to_string())) {
            return Ok(Expr::Variable(Self::token_type_to_variable(&identifier.token_type)));
        }

        return Err(UnexpectedEOF);
    }

    // Statement parsing methods

    pub fn program(&mut self) -> Result<Vec<Stmt>> {
        let mut stmts = Vec::new();
        loop {
            match self.declaration()? {
                Some(stmt) => stmts.push(stmt),
                None => break,
            }
        }
        Ok(stmts)
    }

    // TODO: Clean up logic
    fn declaration(&mut self) -> Result<Option<Stmt>> {
        match if self.consume_token(Var).is_some() {
            self.var_declaration()
        } else {
            self.statement()
        } {
            Ok(maybe_stmt) => Ok(maybe_stmt),
            Err(err) => {
                self.synchronize();
                Err(err)
            }
        }
    }

    fn var_declaration(&mut self) -> Result<Option<Stmt>> {
        let name = Self::token_type_to_variable(&self.consume_token_expect(Identifier("".to_string()))?.token_type);
        let mut initializer = None;
        if self.consume_token(TokenType::Equal).is_some() {
            initializer = Some(self.expression()?);
        }
        self.consume_token_expect(Semicolon)?;
        Ok(Some(Stmt::Var(name.clone(), initializer)))
    }

    fn statement(&mut self) -> Result<Option<Stmt>> {
        if self.peek().is_none() {
            Ok(None)
        } else if self.consume_token(Print).is_some() {
            Ok(Some(self.print_statement()?))
        } else if self.consume_token(LeftBrace).is_some() {
            Ok(Some(Stmt::Block(self.block()?)))
        } else {
            Ok(Some(self.expression_statement()?))
        }
    }

    fn print_statement(&mut self) -> Result<Stmt> {
        let expression = self.expression()?;
        self.consume_token_expect(Semicolon)?;
        Ok(Stmt::Print(expression))
    }

    fn block(&mut self) -> Result<Vec<Stmt>> {
        let mut statements = Vec::new();

        while self.peek().filter(|token| token.token_type != RightBrace).is_some() {
            statements.push(self.declaration()?.ok_or(ParserError::UnexpectedEOF)?)
        }

        self.consume_token_expect(RightBrace)?;
        Ok(statements)
    }

    fn expression_statement(&mut self) -> Result<Stmt> {
        let expression = self.expression()?;
        self.consume_token_expect(Semicolon)?;
        Ok(Stmt::Expression(expression))
    }
}

pub fn parse(tokens: Vec<Token>) -> Result<Vec<Stmt>> {
    let mut parser = Parser::new();
    parser.append(tokens);
    parser.program()
}

#[cfg(test)]
mod tests {
    use crate::expr::Expr;
    use crate::expr::BinaryOperator::*;
    use crate::expr::Expr::{Binary, Grouping, Unary};
    use crate::expr::Literal::Number;
    use crate::expr::UnaryOperator::NumericNegate;
    use crate::parser::{Parser, ParserError};
    use crate::stmt::Stmt;
    use crate::token::{Location, Token, TokenType};
    use crate::token::TokenType::*;

// !true || false
    // > Unary(
    // BooleanNegate,
    // Literal(
    // Boolean(
    // true,
    // ),
    // ),
    // )

    fn parser_test(tokens: Vec<Token>, expected_result: Result<Vec<Stmt>, ParserError>) {
        let mut parser = Parser::new();
        parser.append(tokens);
        let result = parser.program();

        assert_eq!(result, expected_result);
    }

    #[test]
    fn single_expression() {
        let tokens = vec![
            Token::new(TokenType::Number(1.0), "1".to_string(), Location::new(1, 1)),
            Token::new(Plus, "+".to_string(), Location::new(1, 3)),
            Token::new(TokenType::Number(2.0), "2".to_string(), Location::new(1, 5)),
            Token::new(Star, "*".to_string(), Location::new(1, 7)),
            Token::new(LeftParen, "(".to_string(), Location::new(1, 9)),
            Token::new(Minus, "-".to_string(), Location::new(1, 10)),
            Token::new(TokenType::Number(3.0), "3".to_string(), Location::new(1, 11)),
            Token::new(RightParen, ")".to_string(), Location::new(1, 12)),
            Token::new(Semicolon, ";".to_string(), Location::new(1, 13)),
        ];

        let expected_expr = Binary(
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
        );

        let expected_result = Ok(vec![Stmt::Expression(expected_expr)]);

        parser_test(tokens, expected_result)
    }
}
