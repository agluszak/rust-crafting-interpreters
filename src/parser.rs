use std::collections::VecDeque;
use std::error::Error;

use crate::expr::{BinaryOperator, Expr, ExprType, Literal, UnaryOperator};
use crate::stmt::Stmt;
use crate::token::{Location, Token, TokenType};

#[derive(Debug, PartialEq)]
pub enum ParserErrorType {
    UnexpectedEOF,
    MissingExpectedToken(TokenType),
    InvalidAssignmentTarget,
}

// TODO: impl Error for ParserError
// TODO: return multiple errors
#[derive(Debug, PartialEq)]
pub struct ParserError {
    error_type: ParserErrorType,
    location: Location,
}

impl ParserError {
    fn new(error_type: ParserErrorType, location: Location) -> Self {
        Self {
            error_type,
            location,
        }
    }
}

type Result<T> = std::result::Result<T, ParserError>;

// TODO: separate parser logic (i.e. grammar) from the scanning logic
pub struct Parser {
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
        // TODO: proper location
        self.consume_token(expected.clone())
            .ok_or(ParserError::new(ParserErrorType::MissingExpectedToken(expected), Location::initial()))
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
        use TokenType::Semicolon;
        self.advance_while(|token| !token.token_type.is_same_variant(&Semicolon));
        self.consume_token(Semicolon);
    }

    // Helper methods

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
            let location = operator_token.location;
            let token_type = &operator_token.token_type.clone();
            let right = higher_precedence_parser(self)?;
            let operator = token_type.to_binary_operator();
            expr = Expr::new(ExprType::Binary(Box::new(expr), operator, Box::new(right)), location);
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
            let operator = token_type.to_unary_operator();
            return Ok(Expr::new(ExprType::Unary(operator, Box::new(right)), operator_token.location));
        }

        higher_precedence_parser(self)
    }

    // Expression parsing methods

    fn expression(&mut self) -> Result<Expr> {
        self.assignment()
    }

    fn assignment(&mut self) -> Result<Expr> {
        let expr = self.or()?;

        if let Some(equals) = self.consume_token(TokenType::Equal) {
            let value = self.assignment()?;

            if let ExprType::Variable(variable) = expr.expr_type {
                return Ok(Expr::new(ExprType::Assign(variable, Box::new(value)), equals.location));
            }

            Err(ParserError::new(ParserErrorType::InvalidAssignmentTarget, equals.location))
        } else {
            Ok(expr)
        }
    }

    fn or(&mut self) -> Result<Expr> {
        self.parse_binary_operation(
            &[TokenType::Or],
            &Self::and,
        )
    }

    fn and(&mut self) -> Result<Expr> {
        self.parse_binary_operation(
            &[TokenType::And],
            &Self::equality,
        )
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
        if let Some(token) = self.consume_one_of(&[
            TokenType::False,
            TokenType::True,
            TokenType::Number(0.0),
            TokenType::String("".to_string()),
        ]) {
            let literal = token.token_type.to_literal();
            return Ok(Expr::new(ExprType::Literal(literal), token.location));
        }

        if let Some(paren) = self.consume_token(TokenType::LeftParen) {
            let expr = self.expression()?;
            self.consume_token_expect(TokenType::RightParen)?;
            return Ok(Expr::new(ExprType::Grouping(Box::new(expr)), paren.location));
        }

        if let Some(identifier) = self.consume_token(TokenType::Identifier("".to_string())) {
            return Ok(Expr::new(ExprType::Variable(identifier.token_type.to_variable()), identifier.location));
        }

        // TODO: proper location
        return Err(ParserError::new(ParserErrorType::UnexpectedEOF, Location::initial()));
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
        match if self.consume_token(TokenType::Var).is_some() {
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
        let name = self.consume_token_expect(TokenType::Identifier("".to_string()))?.token_type.to_variable();
        let mut initializer = None;
        if self.consume_token(TokenType::Equal).is_some() {
            initializer = Some(self.expression()?);
        }
        self.consume_token_expect(TokenType::Semicolon)?;
        Ok(Some(Stmt::Var(name, initializer)))
    }

    fn statement(&mut self) -> Result<Option<Stmt>> {
        if self.peek().is_none() {
            Ok(None)
        } else if self.consume_token(TokenType::If).is_some() {
            Ok(Some(self.if_statement()?))
        } else if self.consume_token(TokenType::Print).is_some() {
            Ok(Some(self.print_statement()?))
        } else if self.consume_token(TokenType::While).is_some() {
            Ok(Some(self.while_statement()?))
        } else if self.consume_token(TokenType::For).is_some() {
            Ok(Some(self.for_statement()?))
        } else if self.consume_token(TokenType::LeftBrace).is_some() {
            Ok(Some(Stmt::Block(self.block()?)))
        } else {
            Ok(Some(self.expression_statement()?))
        }
    }

    fn if_statement(&mut self) -> Result<Stmt> {
        self.consume_token_expect(TokenType::LeftParen)?;
        let condition = self.expression()?;
        self.consume_token_expect(TokenType::RightParen)?;

        // TODO: proper location
        let then_branch = self.statement()?.ok_or(ParserError::new(ParserErrorType::UnexpectedEOF, Location::initial()))?;
        let else_branch = if self.consume_token(TokenType::Else).is_some() {
            Some(Box::new(self.statement()?.ok_or(ParserError::new(ParserErrorType::UnexpectedEOF, Location::initial()))?))
        } else {
            None
        };

        Ok(Stmt::If(condition, Box::new(then_branch), else_branch))
    }

    fn print_statement(&mut self) -> Result<Stmt> {
        let expression = self.expression()?;
        self.consume_token_expect(TokenType::Semicolon)?;
        Ok(Stmt::Print(expression))
    }

    fn while_statement(&mut self) -> Result<Stmt> {
        self.consume_token_expect(TokenType::LeftParen)?;
        let condition = self.expression()?;
        self.consume_token_expect(TokenType::RightParen)?;

        // TODO: proper location
        let stmt = self.statement()?.ok_or(ParserError::new(ParserErrorType::UnexpectedEOF, Location::initial()))?;

        Ok(Stmt::While(condition, Box::new(stmt)))
    }

    fn for_statement(&mut self) -> Result<Stmt> {
        self.consume_token_expect(TokenType::LeftParen)?;

        let initializer = if self.consume_token(TokenType::Semicolon).is_some() {
            None
        } else if self.consume_token(TokenType::Var).is_some() {
            Some(self.var_declaration()?).flatten()
        } else {
            Some(self.expression_statement()?)
        };

        let condition = if self.consume_token(TokenType::Semicolon).is_some() {
            None
        } else {
            Some(self.expression()?)
        };

        if condition.is_some() {
            self.consume_token_expect(TokenType::Semicolon)?;
        }

        let increment = if self.consume_token(TokenType::RightParen).is_some() {
            None
        } else {
            Some(self.expression()?)
        };

        if increment.is_some() {
            self.consume_token_expect(TokenType::RightParen)?;
        }

        let mut body = self.statement()?.ok_or(ParserError::new(ParserErrorType::UnexpectedEOF, Location::initial()))?;

        if let Some(increment) = increment {
            body = Stmt::Block(vec![body, Stmt::Expression(increment)]);
        }

        let condition = if let Some(condition) = condition {
            condition
        } else {
            // TODO: does it really matter that there's no real position in this case?
            Expr::new(ExprType::Literal(Literal::Boolean(true)), Location::initial())
        };

        body = Stmt::While(condition, Box::new(body));

        if let Some(initializer) = initializer {
            body = Stmt::Block(vec![initializer, body]);
        }

        Ok(body)
    }

    fn block(&mut self) -> Result<Vec<Stmt>> {
        let mut statements = Vec::new();

        while let Some(token) = self.peek().filter(|token| token.token_type != TokenType::RightBrace) {
            let location = token.location;
            let stmt = self.declaration()?.ok_or(ParserError::new(ParserErrorType::UnexpectedEOF, location))?;
            statements.push(stmt)
        }

        self.consume_token_expect(TokenType::RightBrace)?;
        Ok(statements)
    }

    fn expression_statement(&mut self) -> Result<Stmt> {
        let expression = self.expression()?;
        self.consume_token_expect(TokenType::Semicolon)?;
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
    use crate::expr::{Expr, ExprType, UnaryOperator, Literal};
    use crate::expr::BinaryOperator::*;
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

    // 2 = 3;

    // var x = var;

    // = 3;





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

        let expected_expr = Expr::new(ExprType::Binary(
            Box::from(Expr::new(ExprType::Literal(Literal::Number(1.0)), Location::new(1, 1))),
            Add,
            Box::from(Expr::new(ExprType::Binary(
                Box::from(Expr::new(ExprType::Literal(Literal::Number(2.0)), Location::new(1, 5))),
                Multiply,
                Box::from(Expr::new(ExprType::Grouping(Box::from(Expr::new(ExprType::Unary(
                    UnaryOperator::NumericNegate,
                    Box::from(Expr::new(ExprType::Literal(Literal::Number(3.0)), Location::new(1, 11)))), Location::new(1, 10)))), Location::new(1, 9)))), Location::new(1, 7)))), Location::new(1, 3));

        let expected_result = Ok(vec![Stmt::Expression(expected_expr)]);

        parser_test(tokens, expected_result)
    }
}
