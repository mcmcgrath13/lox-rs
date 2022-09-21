use std::str::FromStr;
use std::collections::HashMap;

use crate::token::{Token, TokenType};
use crate::RunTime;

pub struct Scanner<'a> {
    source: String,
    tokens: Vec<Token>,
    start: u64,
    current: u64,
    line: u64,
    rt: &'a RunTime
}

const KEYWORDS: HashMap<&str, TokenType> = HashMap::from([
    ("and", TokenType::And),
    ("class", TokenType::Class),
    ("else", TokenType::Else),
    ("false", TokenType::False),
    ("fun", TokenType::Fun),
    ("for", TokenType::For),
    ("if", TokenType::If),
    ("nil", TokenType::Nil),
    ("or", TokenType::Or),
    ("print", TokenType::Print),
    ("return", TokenType::Return),
    ("super", TokenType::Super),
    ("this", TokenType::This),
    ("true", TokenType::True),
    ("var", TokenType::Var),
    ("while", TokenType::While),
]);

impl Scanner<'_> {
    fn new(source: String, &rt: RunTime) -> Scanner<'static> {
        Scanner { source: source, tokens: Vec::new(), start: 0, current: 0, line: 1, rt}
    }

    pub fn tokens(&self) -> Vec<Token> {
        return self.tokens;
    }

    pub fn scan_tokens(&self) {
        while !self.is_at_end() {
            self.start = self.current;
            self.scan_token();
        }

        self.tokens.push(Token{ t: TokenType::Eof, lexeme: "", literal: None, line: self.line})
    }

    fn scan_token(&self) {
        let c = self.advance();
        match c {
            // single tokens
            '(' => self.add_token(TokenType::LeftParen),
            ')' => self.add_token(TokenType::RightParen),
            '{' => self.add_token(TokenType::LeftBrace),
            '}' => self.add_token(TokenType::RightBrace),
            ',' => self.add_token(TokenType::Comma),
            '.' => self.add_token(TokenType::Dot),
            '-' => self.add_token(TokenType::Minus),
            '+' => self.add_token(TokenType::Plus),
            ';' => self.add_token(TokenType::Semicolon),
            '*' => self.add_token(TokenType::Star),

            // multi-tokens
            '!' => self.add_token(if self.match_next('=') {TokenType::BangEqual} else {TokenType::Bang}),
            '=' => self.add_token(if self.match_next('=') {TokenType::EqualEqual} else {TokenType::Equal}),
            '<' => self.add_token(if self.match_next('=') {TokenType::LessEqual} else  {TokenType::Less}),
            '>' => self.add_token(if self.match_next('=') {TokenType::GreaterEqual} else  {TokenType::Greater}),
            '/' => {
                if self.match_next('/') {
                    // A comment goes to the end of the line
                    while self.peek() != '\n' && !self.is_at_end() { self.advance() }
                } else {
                    self.add_token(TokenType::Slash)
                }
            },

            // whitespace
            ' ' => {},
            '\t' => {},
            '\n' => self.line += 1,
            '\r' => {},

            // literals
            '"' => self.string(),

            // ruh roh
            _ => {
                if self.is_digit(c) {
                    self.number();
                } else if self.is_alpha(c) {
                    self.identifier();
                } else {
                    self.rt.error(self.line, format!("Unexpected character, {}", c));
                }
            }
        }
    }

    // HELPER FUNCTIONS
    fn is_at_end(&self) -> bool {
        return self.current >= self.source.len().try_into().unwrap()
    }

    fn advance(&self) -> &str {
        let next = self.source[self.current];
        self.current += 1;
        return next;
    }

    fn match_next(&self, expected: &str) -> bool {
        if self.is_at_end() {
            return false
        }

        if self.source[self.current] != expected {
            return false;
        }

        self.current += 1;
        return true;
    }

    fn peek(&self) -> character {
        if self.is_at_end() {
            return '\0';
        } else {
            return self.source[self.current];
        }
    }

    fn peek_next(&self) -> character {
        if self.current + 1 > self.source.len().try_into().unwrap() { return '\0' }

        return self.source[self.current + 1];
    }

    fn is_digit(&self, c: character) -> bool {
        return c >= '0' && c <= '9';
    }

    fn is_alpha(&self, c: character) -> bool {
        return (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || c == '_';
    }

    fn is_alphanumeric(&self, c: character) -> bool {
        return self.is_alpha(c) || self.is_digit(c)
    }

    fn string(&self) {
        while self.peek() != '"' && !self.is_at_end() {
            if self.peek() == '\n' {self.line += 1}
            self.advance();
        }

        if self.is_at_end() {
            self.rt.error(self.line, "Unterminated string");
            return;
        }

        self.advance();
        self.add_literal_token(TokenType::String, self.source[self.start+1..self.current-1]);
    }

    fn number(&self) {
        while self.is_digit(self.peek()) { self.advance(); };

        if self.peek() == '.'&& self.is_digit(self.peek_next()) {
            self.advance();

            while self.is_digit(self.peek()) { self.advance(); };
        }

        self.add_literal_token(TokenType::Number, f64::from_str(self.source[self.start..self.current]).unwrap())
    }

    fn identifier(&self) {
        while self.is_alphanumeric(self.peek()) { self.advance(); };

        let text = self.source[self.start..self.current];
        let token_type = match KEYWORDS.get(text) {
            Some(t) => t,
            None => TokenType::Identifier
        };

        self.add_token(token_type)
    }

    fn add_token(&self, t: TokenType) {
        self.add_literal_token(t, None)
    }

    fn add_literal_token<T>(&self, t: TokenType, literal: T) {
        let lexeme = self.source[self.start..self.current];
        self.tokens.push(Token { t, lexeme, literal, line: self.line})
    }
}
