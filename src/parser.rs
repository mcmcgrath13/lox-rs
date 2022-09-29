use crate::ast::Expr;
use crate::token::{Token, TokenType};
use crate::Reportable;

pub struct Parser<'code> {
    tokens: &'code Vec<Token<'code>>,
    current: usize,
}

pub struct ParseError {
    line: usize,
    lexeme: String,
    message: String,
}

impl ParseError {
    pub fn from_token(token: &Token<'_>, message: impl AsRef<str>) -> Self {
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
    pub fn new(tokens: &'code Vec<Token<'code>>) -> Parser<'code> {
        Parser { tokens, current: 0 }
    }

    pub fn parse(&mut self) -> (Option<Expr<'code>>, Vec<ParseError>) {
        let mut errs = Vec::new();
        match self.expression() {
            Ok(e) => (Some(e), errs),
            Err(e) => {
                errs.push(e);
                (None, errs)
            }
        }
    }

    // Recursive descent methods:
    // expression -> equality -> comparison -> term -> factor -> unary -> primary

    fn expression(&mut self) -> Result<Expr<'code>, ParseError> {
        self.equality()
    }

    fn equality(&mut self) -> Result<Expr<'code>, ParseError> {
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

    fn comparison(&mut self) -> Result<Expr<'code>, ParseError> {
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

    fn term(&mut self) -> Result<Expr<'code>, ParseError> {
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

    fn factor(&mut self) -> Result<Expr<'code>, ParseError> {
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

    fn unary(&mut self) -> Result<Expr<'code>, ParseError> {
        match self.match_next(&[TokenType::Bang, TokenType::Minus]) {
            Some(op) => {
                let right = Box::new(self.unary()?);
                Ok(Expr::Unary { op, right })
            }
            None => self.primary(),
        }
    }

    fn primary(&mut self) -> Result<Expr<'code>, ParseError> {
        if let Some(value) = self.match_next(&[TokenType::False, TokenType::True, TokenType::Nil]) {
            return Ok(Expr::Literal { value });
        }

        // special handling for number and string due to the enum value
        if !self.is_at_end() {
            match self.peek().t {
                TokenType::Number(_) | TokenType::String(_) => {
                    return Ok(Expr::Literal {
                        value: self.advance(),
                    })
                }
                _ => {}
            }
        }

        if let Some(left_paren) = self.match_next(&[TokenType::LeftParen]) {
            let expression = Box::new(self.expression()?);
            match self.match_next(&[TokenType::RightParen]) {
                Some(_) => return Ok(Expr::Grouping { expression }),
                None => {
                    return Err(ParseError::from_token(
                        &left_paren,
                        "no matching right paren",
                    ))
                }
            };
        };

        Err(ParseError::from_token(
            &self.peek(),
            "Unterminated expression",
        ))
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

    fn peek(&self) -> Token<'code> {
        self.tokens[self.current]
    }

    fn previous(&self) -> Token<'code> {
        self.tokens[self.current - 1]
    }

    fn advance(&mut self) -> Token<'code> {
        if !self.is_at_end() {
            self.current += 1
        }

        self.previous()
    }

    fn match_next(&mut self, types: &[TokenType]) -> Option<Token<'code>> {
        for t in types {
            if self.check(t) {
                return Some(self.advance());
            }
        }

        None
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
