use std::collections::VecDeque;

use crate::expr::{Expr, ExprType, Literal};
use crate::stmt::Stmt;
use crate::token::{Location, Token, TokenType};

#[derive(Debug, PartialEq)]
pub enum ParserErrorType {
    UnexpectedEOF,
    MissingExpectedToken(TokenType),
    InvalidAssignmentTarget,
    TooManyArguments,
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
    location: Location,
    end_location: Location,
}

impl Parser {
    pub fn new() -> Self {
        Self {
            src: VecDeque::new(),
            location: Location::initial(),
            end_location: Location::initial(),
        }
    }

    pub fn append(&mut self, tokens: Vec<Token>) {
        self.src.extend(tokens)
    }

    // Iterator-like methods

    fn next(&mut self) -> Option<Token> {
        let token = self.src.pop_front();
        if let Some(token) = &token {
            self.location = token.location;
            self.end_location = token.end_location();
        }
        token
    }

    pub fn peek(&mut self) -> Option<&Token> {
        self.src.front()
    }

    pub fn consume_token(&mut self, expected: TokenType) -> Option<Token> {
        self.consume_one_of(&[expected])
    }

    fn consume_token_expect(&mut self, expected: TokenType) -> Result<Token> {
        let expected_location = self.end_location;
        let expected_location = self
            .peek()
            .map_or(expected_location, |token| token.location);

        self.consume_token(expected.clone()).ok_or_else(|| {
            ParserError::new(
                ParserErrorType::MissingExpectedToken(expected),
                expected_location,
            )
        })
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
        false
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
            expr = Expr::new(
                ExprType::Binary(Box::new(expr), operator, Box::new(right)),
                location,
            );
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
            let token_type = &operator_token.token_type;
            let right = higher_precedence_parser(self)?;
            let operator = token_type.to_unary_operator();
            return Ok(Expr::new(
                ExprType::Unary(operator, Box::new(right)),
                operator_token.location,
            ));
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
                return Ok(Expr::new(
                    ExprType::Assign(variable, Box::new(value)),
                    equals.location,
                ));
            }

            Err(ParserError::new(
                ParserErrorType::InvalidAssignmentTarget,
                equals.location,
            ))
        } else {
            Ok(expr)
        }
    }

    fn or(&mut self) -> Result<Expr> {
        self.parse_binary_operation(&[TokenType::Or], &Self::and)
    }

    fn and(&mut self) -> Result<Expr> {
        self.parse_binary_operation(&[TokenType::And], &Self::equality)
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
        self.parse_unary_operation(&[TokenType::Minus, TokenType::Bang], &Self::call)
    }

    fn call(&mut self) -> Result<Expr> {
        let mut expr = self.primary()?;

        while self.consume_token(TokenType::LeftParen).is_some() {
            expr = self.finish_call(expr)?;
        }

        Ok(expr)
    }

    fn finish_call(&mut self, callee: Expr) -> Result<Expr> {
        let mut arguments = Vec::new();
        let right_paren = if let Some(right_paren) = self.consume_token(TokenType::RightParen) {
            right_paren
        } else {
            loop {
                // TODO: introduce constant
                if arguments.len() >= 255 {
                    return Err(ParserError::new(
                        ParserErrorType::TooManyArguments,
                        self.end_location,
                    ));
                }
                arguments.push(self.expression()?);
                if self.consume_token(TokenType::Comma).is_none() {
                    break;
                }
            }
            self.consume_token_expect(TokenType::RightParen)?
        };
        let location = callee.location;
        Ok(Expr::new(
            ExprType::Call(Box::new(callee), arguments),
            location,
        ))
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
            return Ok(Expr::new(
                ExprType::Grouping(Box::new(expr)),
                paren.location,
            ));
        }

        if let Some(identifier) = self.consume_token(TokenType::Identifier("".to_string())) {
            return Ok(Expr::new(
                ExprType::Variable(identifier.token_type.to_variable()),
                identifier.location,
            ));
        }

        Err(ParserError::new(
            ParserErrorType::UnexpectedEOF,
            self.end_location,
        ))
    }

    // Statement parsing methods

    pub fn program(&mut self) -> Result<Vec<Stmt>> {
        let mut stmts = Vec::new();
        while let Some(stmt) = self.declaration()? {
            stmts.push(stmt);
        }
        Ok(stmts)
    }

    // TODO: Clean up logic
    fn declaration(&mut self) -> Result<Option<Stmt>> {
        let stmt =  if self.consume_token(TokenType::Fun).is_some() {
            self.function()
        } else if self.consume_token(TokenType::Var).is_some() {
            self.var_declaration()
        } else {
            self.statement()
        };

        match stmt {
            Ok(maybe_stmt) => Ok(maybe_stmt),
            Err(err) => {
                self.synchronize();
                Err(err)
            }
        }
    }

    fn function(&mut self) -> Result<Option<Stmt>> {
        let name = self.consume_token_expect(TokenType::Identifier("".to_string()))?;
        self.consume_token_expect(TokenType::LeftParen)?;
        let mut parameters = Vec::new();
        if self.consume_token(TokenType::RightParen).is_none() {
            loop {
                // TODO: introduce constant
                if parameters.len() >= 255 {
                    return Err(ParserError::new(
                        ParserErrorType::TooManyArguments,
                        self.end_location,
                    ));
                }
                let name = self
                    .consume_token_expect(TokenType::Identifier("".to_string()))?
                    .token_type
                    .to_variable();
                parameters.push(name);
                if self.consume_token(TokenType::Comma).is_none() {
                    break;
                }
            }
        }
        self.consume_token_expect(TokenType::RightParen)?;
        self.consume_token_expect(TokenType::LeftBrace)?;
        let block = self.block()?;
        Ok(Some(
            Stmt::Function(
                name.token_type.to_variable(),
                parameters,
                block,
            ),
        ))

    }

    fn var_declaration(&mut self) -> Result<Option<Stmt>> {
        let name = self
            .consume_token_expect(TokenType::Identifier("".to_string()))?
            .token_type
            .to_variable();
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
        } else if self.consume_token(TokenType::Return).is_some() {
            Ok(Some(self.return_statement()?))
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
        let right_paren = self.consume_token_expect(TokenType::RightParen)?;

        let then_branch = self.statement()?.ok_or_else(|| {
            ParserError::new(ParserErrorType::UnexpectedEOF, right_paren.location)
        })?;
        let else_branch = if let Some(else_token) = self.consume_token(TokenType::Else) {
            Some(Box::new(self.statement()?.ok_or_else(|| {
                ParserError::new(ParserErrorType::UnexpectedEOF, else_token.location)
            })?))
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

    fn return_statement(&mut self) -> Result<Stmt> {
        let value = if self.consume_token(TokenType::Semicolon).is_some() {
            None
        } else {
            let value = self.expression()?;
            self.consume_token_expect(TokenType::Semicolon)?;
            Some(value)
        };

        Ok(Stmt::Return(value))
    }

    fn while_statement(&mut self) -> Result<Stmt> {
        self.consume_token_expect(TokenType::LeftParen)?;
        let condition = self.expression()?;
        let right_paren = self.consume_token_expect(TokenType::RightParen)?;

        let stmt = self.statement()?.ok_or_else(|| {
            ParserError::new(ParserErrorType::UnexpectedEOF, right_paren.location)
        })?;

        Ok(Stmt::While(condition, Box::new(stmt)))
    }

    // Syntactic sugar for a 'while' statement
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

        let mut body = self
            .statement()?
            .ok_or_else(|| ParserError::new(ParserErrorType::UnexpectedEOF, Location::initial()))?;

        if let Some(increment) = increment {
            body = Stmt::Block(vec![body, Stmt::Expression(increment)]);
        }

        let condition = if let Some(condition) = condition {
            condition
        } else {
            // TODO: does it really matter that there's no real position in this case?
            Expr::new(
                ExprType::Literal(Literal::Boolean(true)),
                Location::initial(),
            )
        };

        body = Stmt::While(condition, Box::new(body));

        if let Some(initializer) = initializer {
            body = Stmt::Block(vec![initializer, body]);
        }

        Ok(body)
    }

    fn block(&mut self) -> Result<Vec<Stmt>> {
        let mut statements = Vec::new();

        while let Some(token) = self
            .peek()
            .filter(|token| token.token_type != TokenType::RightBrace)
        {
            let location = token.location;
            let stmt = self
                .declaration()?
                .ok_or_else(|| ParserError::new(ParserErrorType::UnexpectedEOF, location))?;
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
    use crate::expr::BinaryOperator::*;
    use crate::expr::{Expr, ExprType, Literal, UnaryOperator};
    use crate::parser::{Parser, ParserError};
    use crate::stmt::Stmt;
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
            Token::new(
                TokenType::Number(3.0),
                "3".to_string(),
                Location::new(1, 11),
            ),
            Token::new(RightParen, ")".to_string(), Location::new(1, 12)),
            Token::new(Semicolon, ";".to_string(), Location::new(1, 13)),
        ];

        let expected_expr = Expr::new(
            ExprType::Binary(
                Box::from(Expr::new(
                    ExprType::Literal(Literal::Number(1.0)),
                    Location::new(1, 1),
                )),
                Add,
                Box::from(Expr::new(
                    ExprType::Binary(
                        Box::from(Expr::new(
                            ExprType::Literal(Literal::Number(2.0)),
                            Location::new(1, 5),
                        )),
                        Multiply,
                        Box::from(Expr::new(
                            ExprType::Grouping(Box::from(Expr::new(
                                ExprType::Unary(
                                    UnaryOperator::NumericNegate,
                                    Box::from(Expr::new(
                                        ExprType::Literal(Literal::Number(3.0)),
                                        Location::new(1, 11),
                                    )),
                                ),
                                Location::new(1, 10),
                            ))),
                            Location::new(1, 9),
                        )),
                    ),
                    Location::new(1, 7),
                )),
            ),
            Location::new(1, 3),
        );

        let expected_result = Ok(vec![Stmt::Expression(expected_expr)]);

        parser_test(tokens, expected_result)
    }

    #[test]
    fn function_call_no_args() {
        let tokens = vec![
            Token::new(Identifier("add".to_string()), "add".to_string(), Location::new(1, 1)),
            Token::new(LeftParen, "(".to_string(), Location::new(1, 5)),
            Token::new(RightParen, ")".to_string(), Location::new(1, 6)),
            Token::new(Semicolon, ";".to_string(), Location::new(1, 7)),
        ];

        let expected_expr = Expr::new(
            ExprType::Call(
                Box::from(Expr::new(
                    ExprType::Variable("add".to_string()),
                    Location::new(1, 1),
                )),
                vec![],
            ),
            Location::new(1, 1),
        );

        let expected_result = Ok(vec![Stmt::Expression(expected_expr)]);

        parser_test(tokens, expected_result)
    }
}
