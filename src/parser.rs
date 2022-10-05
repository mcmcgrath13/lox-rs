use crate::ast::{Expr, Stmt};
use crate::token::{Token, TokenType};
use crate::Reportable;

pub struct Parser<'code> {
    tokens: &'code Vec<Token>,
    current: usize,
}

pub struct ParseError {
    line: usize,
    lexeme: String,
    message: String,
}

impl ParseError {
    pub fn from_token(token: &Token, message: impl AsRef<str>) -> Self {
        Self {
            line: token.line,
            lexeme: token.lexeme.to_string(),
            message: message.as_ref().to_string(),
        }
    }
}

impl Reportable for ParseError {
    fn report(&self) {
        eprintln!(
            "[line {} at {}] Error (Parser): {}",
            self.line, self.lexeme, self.message
        );
    }
}

impl<'code> Parser<'code> {
    pub fn new(tokens: &'code Vec<Token>) -> Parser<'code> {
        Parser { tokens, current: 0 }
    }

    pub fn parse(&mut self) -> (Vec<Stmt>, Vec<ParseError>) {
        let mut errs = Vec::new();
        let mut statements = Vec::new();

        loop {
            match self.declaration() {
                Ok(Some(s)) => statements.push(s),
                Ok(None) => break,
                Err(e) => {
                    self.synchronize();
                    errs.push(e)
                }
            }
        }

        (statements, errs)
    }

    // Recursive descent methods:
    // declaration -> statement -> expression -> assignment -> logical or ->  ...
    // ... logical and -> equality -> comparison -> term -> factor -> ...
    // ... unary -> call -> primary

    fn declaration(&mut self) -> Result<Option<Stmt>, ParseError> {
        if self.is_at_end() {
            return Ok(None);
        };

        if self.match_next(&[TokenType::Fun]).is_some() {
            return Ok(Some(self.function_declaration("function")?));
        }

        if self.match_next(&[TokenType::Var]).is_some() {
            return Ok(Some(self.var_declaration()?));
        }

        Ok(Some(self.statement()?))
    }

    fn statement(&mut self) -> Result<Stmt, ParseError> {
        if self.match_next(&[TokenType::For]).is_some() {
            return self.for_statement();
        }

        if self.match_next(&[TokenType::If]).is_some() {
            return self.if_statement();
        }

        if self.match_next(&[TokenType::LeftBrace]).is_some() {
            return Ok(Stmt::Block {
                statements: self.block()?,
            });
        }

        if self.match_next(&[TokenType::Print]).is_some() {
            return self.print_statement();
        }

        if let Some(t) = self.match_next(&[TokenType::Return]) {
            return self.return_statement(t);
        }

        if self.match_next(&[TokenType::While]).is_some() {
            return self.while_statement();
        }

        self.expression_statement()
    }

    fn expression(&mut self) -> Result<Expr, ParseError> {
        self.assignment()
    }

    fn assignment(&mut self) -> Result<Expr, ParseError> {
        let expr = self.logical_or()?;

        if self.match_next(&[TokenType::Equal]).is_some() {
            let value = self.assignment()?;

            if let Expr::Variable { name } = expr {
                return Ok(Expr::Assign {
                    name,
                    value: Box::new(value),
                });
            }

            return Err(ParseError::from_token(
                self.previous(),
                "Invalid assignment target",
            ));
        }

        Ok(expr)
    }

    fn logical_or(&mut self) -> Result<Expr, ParseError> {
        let mut expr = self.logical_and()?;

        while let Some(op) = self.match_next(&[TokenType::Or]) {
            let right = Box::new(self.logical_and()?);
            expr = Expr::Logical {
                left: Box::new(expr),
                op,
                right,
            };
        }

        Ok(expr)
    }

    fn logical_and(&mut self) -> Result<Expr, ParseError> {
        let mut expr = self.equality()?;

        while let Some(op) = self.match_next(&[TokenType::And]) {
            let right = Box::new(self.equality()?);
            expr = Expr::Logical {
                left: Box::new(expr),
                op,
                right,
            };
        }

        Ok(expr)
    }

    fn equality(&mut self) -> Result<Expr, ParseError> {
        let mut expr = self.comparison()?;

        while let Some(op) = self.match_next(&[TokenType::EqualEqual, TokenType::BangEqual]) {
            let right = Box::new(self.comparison()?);
            expr = Expr::Binary {
                left: Box::new(expr),
                op,
                right,
            };
        }

        Ok(expr)
    }

    fn comparison(&mut self) -> Result<Expr, ParseError> {
        let mut expr = self.term()?;

        while let Some(op) = self.match_next(&[
            TokenType::Greater,
            TokenType::GreaterEqual,
            TokenType::Less,
            TokenType::LessEqual,
        ]) {
            let right = Box::new(self.term()?);
            expr = Expr::Binary {
                left: Box::new(expr),
                op,
                right,
            };
        }

        Ok(expr)
    }

    fn term(&mut self) -> Result<Expr, ParseError> {
        let mut expr = self.factor()?;

        while let Some(op) = self.match_next(&[TokenType::Minus, TokenType::Plus]) {
            let right = Box::new(self.factor()?);
            expr = Expr::Binary {
                left: Box::new(expr),
                op,
                right,
            };
        }

        Ok(expr)
    }

    fn factor(&mut self) -> Result<Expr, ParseError> {
        let mut expr = self.unary()?;

        while let Some(op) = self.match_next(&[TokenType::Slash, TokenType::Star]) {
            let right = Box::new(self.unary()?);
            expr = Expr::Binary {
                left: Box::new(expr),
                op,
                right,
            };
        }

        Ok(expr)
    }

    fn unary(&mut self) -> Result<Expr, ParseError> {
        match self.match_next(&[TokenType::Bang, TokenType::Minus]) {
            Some(op) => {
                let right = Box::new(self.unary()?);
                Ok(Expr::Unary { op, right })
            }
            None => self.call(),
        }
    }

    fn call(&mut self) -> Result<Expr, ParseError> {
        let mut expr = self.primary()?;

        while let Some(t) = self.match_next(&[TokenType::LeftParen]) {
            expr = self.finish_call(expr, t)?;
        }

        Ok(expr)
    }

    fn primary(&mut self) -> Result<Expr, ParseError> {
        if let Some(value) = self.match_next(&[TokenType::False, TokenType::True, TokenType::Nil]) {
            return Ok(Expr::Literal { value });
        }

        // special handling for number and string due to the enum value
        if !self.is_at_end() {
            match self.peek().t {
                TokenType::Number(_) | TokenType::String(_) => {
                    return Ok(Expr::Literal {
                        value: self.advance().clone(),
                    })
                }
                _ => {}
            }
        }

        if self.match_next(&[TokenType::LeftParen]).is_some() {
            let expression = Box::new(self.expression()?);
            self.consume(TokenType::RightParen, "no matching right paren")?;
            return Ok(Expr::Grouping { expression });
        };

        if let Some(name) = self.match_next(&[TokenType::Identifier]) {
            return Ok(Expr::Variable { name });
        }

        Err(ParseError::from_token(
            self.peek(),
            "Unterminated expression",
        ))
    }

    // statement parsing helpers
    fn function_declaration(
        &mut self,
        kind: impl AsRef<str> + std::fmt::Display,
    ) -> Result<Stmt, ParseError> {
        let name = self.consume(TokenType::Identifier, format!("expect {} name", kind))?;

        self.consume(
            TokenType::LeftParen,
            format!("expect '(' after {} name", kind),
        )?;

        let mut parameters = Vec::new();
        // accumulate the paramenters if there are any
        if !self.check(&TokenType::RightParen) {
            loop {
                if parameters.len() > 255 {
                    return Err(ParseError::from_token(
                        &name,
                        format!("too many parameters for {}, max is 255", kind),
                    ));
                }
                parameters.push(self.consume(TokenType::Identifier, "expect parameter name")?);
                if self.match_next(&[TokenType::Comma]).is_none() {
                    break;
                }
            }
        }

        self.consume(TokenType::RightParen, "expect ')' after parameters")?;
        self.consume(
            TokenType::LeftBrace,
            format!("expect '{{' before {} body", kind),
        )?;

        let body = self.block()?;

        Ok(Stmt::Function {
            name,
            parameters,
            body,
        })
    }

    fn return_statement(&mut self, keyword: Token) -> Result<Stmt, ParseError> {
        let mut value = None;
        if !self.check(&TokenType::Semicolon) {
            value = Some(self.expression()?);
        }

        self.consume(TokenType::Semicolon, "expect ';' after return statement")?;

        Ok(Stmt::Return { keyword, value })
    }

    fn var_declaration(&mut self) -> Result<Stmt, ParseError> {
        let name = self.consume(TokenType::Identifier, "expect variable name")?;

        let mut initializer = None;
        if self.match_next(&[TokenType::Equal]).is_some() {
            initializer = Some(self.expression()?);
        }

        self.consume(
            TokenType::Semicolon,
            "expect ';' after variable declaration",
        )?;

        Ok(Stmt::Var { name, initializer })
    }

    fn print_statement(&mut self) -> Result<Stmt, ParseError> {
        let expression = self.expression()?;
        self.consume(TokenType::Semicolon, "expect ';' after value")?;
        Ok(Stmt::Print { expression })
    }

    fn expression_statement(&mut self) -> Result<Stmt, ParseError> {
        let expression = self.expression()?;
        self.consume(TokenType::Semicolon, "expect ';' after value")?;
        Ok(Stmt::Expression { expression })
    }

    fn block(&mut self) -> Result<Vec<Stmt>, ParseError> {
        let mut statements = Vec::new();

        while !self.check(&TokenType::RightBrace) {
            match self.declaration()? {
                None => break,
                Some(s) => statements.push(s),
            }
        }

        self.consume(TokenType::RightBrace, "Expect '}' after block")?;

        Ok(statements)
    }

    fn if_statement(&mut self) -> Result<Stmt, ParseError> {
        self.consume(TokenType::LeftParen, "expect '(' after if")?;
        let condition = self.expression()?;
        self.consume(TokenType::RightParen, "expect ')' after if condition")?;
        let then_branch = Box::new(self.statement()?);
        let else_branch = match self.match_next(&[TokenType::Else]) {
            Some(_) => Some(Box::new(self.statement()?)),
            None => None,
        };

        Ok(Stmt::If {
            condition,
            then_branch,
            else_branch,
        })
    }

    fn while_statement(&mut self) -> Result<Stmt, ParseError> {
        self.consume(TokenType::LeftParen, "expect '(' after while")?;
        let condition = self.expression()?;
        self.consume(TokenType::RightParen, "expect ')' after while condition")?;
        let body = Box::new(self.statement()?);

        Ok(Stmt::While { condition, body })
    }

    fn for_statement(&mut self) -> Result<Stmt, ParseError> {
        // collect the initializer, condition, increment, and body
        self.consume(TokenType::LeftParen, "expect '(' after for")?;

        let initializer = if self.match_next(&[TokenType::Semicolon]).is_some() {
            None
        } else if self.match_next(&[TokenType::Var]).is_some() {
            Some(self.var_declaration()?)
        } else {
            Some(self.expression_statement()?)
        };

        // if there's no condition, we insert true, to create a token which could still
        // point to the correct place in the source code on error, we need to get the
        // line where the condition would be
        let mut line = 0;
        let condition = if !self.check(&TokenType::Semicolon) {
            Some(self.expression()?)
        } else {
            line = self.peek().line;
            None
        };
        self.consume(TokenType::Semicolon, "expect ';' after loop condition")?;

        let increment = if !self.check(&TokenType::RightParen) {
            Some(self.expression()?)
        } else {
            None
        };
        self.consume(TokenType::RightParen, "expect ')' after while condition")?;

        let mut body = self.statement()?;

        // desugar into while loop from the inside out
        if let Some(inc) = increment {
            body = Stmt::Block {
                statements: vec![body, Stmt::Expression { expression: inc }],
            }
        }

        body = match condition {
            Some(c) => Stmt::While {
                condition: c,
                body: Box::new(body),
            },
            None => Stmt::While {
                condition: Expr::Literal {
                    value: Token::new(TokenType::True, "true", line, 0),
                },
                body: Box::new(body),
            },
        };

        if let Some(init) = initializer {
            body = Stmt::Block {
                statements: vec![init, body],
            }
        }

        Ok(body)
    }

    fn finish_call(&mut self, callee: Expr, token: Token) -> Result<Expr, ParseError> {
        let mut arguments = Vec::new();

        // accumulate the arguments if there are any
        if !self.check(&TokenType::RightParen) {
            loop {
                if arguments.len() > 255 {
                    return Err(ParseError::from_token(
                        &token,
                        "too many arguments to call, max is 255",
                    ));
                }
                arguments.push(self.expression()?);
                if self.match_next(&[TokenType::Comma]).is_none() {
                    break;
                }
            }
        }

        let paren = self.consume(
            TokenType::RightParen,
            "expect ')' after function call arguments",
        )?;

        Ok(Expr::Call {
            callee: Box::new(callee),
            paren,
            arguments,
        })
    }

    // Helper methods for traversing the tokens

    fn is_at_end(&self) -> bool {
        self.peek().t == TokenType::Eof
    }

    fn check(&self, t: &TokenType) -> bool {
        if self.is_at_end() {
            return false;
        }

        self.peek().t == *t
    }

    fn peek(&self) -> &Token {
        &self.tokens[self.current]
    }

    fn previous(&self) -> &Token {
        &self.tokens[self.current - 1]
    }

    fn advance(&mut self) -> &Token {
        if !self.is_at_end() {
            self.current += 1
        }

        self.previous()
    }

    fn match_next(&mut self, types: &[TokenType]) -> Option<Token> {
        for t in types {
            if self.check(t) {
                return Some(self.advance().clone());
            }
        }

        None
    }

    fn consume(&mut self, t: TokenType, message: impl AsRef<str>) -> Result<Token, ParseError> {
        match self.match_next(&[t]) {
            Some(t) => Ok(t),
            None => Err(ParseError::from_token(self.peek(), message)),
        }
    }

    // Error handling

    fn synchronize(&mut self) {
        self.advance();

        while !self.is_at_end() {
            if self.previous().t == TokenType::Semicolon {
                return;
            }

            match self.peek().t {
                TokenType::Class
                | TokenType::Fun
                | TokenType::Var
                | TokenType::For
                | TokenType::If
                | TokenType::While
                | TokenType::Print
                | TokenType::Return => return,
                _ => self.advance(),
            };
        }
    }
}
