use std::str::FromStr;
use std::result::Result;

use crate::token::{Token, TokenType};

pub struct Scanner<'code> {
    source: &'code String,
    pub tokens: Vec<Token<'code>>,
    start: usize,
    current: usize,
    line: usize
}


const KEYWORDS: &[(&str, TokenType)] = &[
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
];

// Use binary search to access the map:
fn get_keyword(key: &str) -> Result<TokenType, usize> {
    KEYWORDS.binary_search_by(|(k, _)| k.cmp(&key)).map(|x| KEYWORDS[x].1)
}

fn is_digit(c: &str) -> bool {
    return c >= "0" && c <= "9";
}

fn is_alpha(c: &str) -> bool {
    return (c >= "a" && c <= "z") || (c >= "A" && c <= "Z") || c == "_";
}

fn is_alphanumeric(c: &str) -> bool {
    return is_alpha(c) || is_digit(c)
}

impl<'code> Scanner<'code> {
    pub fn new(source: &'code String) -> Scanner<'code> {
        Scanner{ source: source, tokens: Vec::new(), start: 0, current: 0, line: 1}
    }

    pub fn scan_tokens(&mut self) {
        while !self.is_at_end() {
            self.start = self.current;
            self.scan_token();
        }

        self.tokens.push(Token{ t: TokenType::Eof, lexeme: "", line: self.line})
    }

    fn scan_token(&mut self) -> Result<(), &str> {
        let c = self.advance();
        match c {
            // single tokens
            "(" => self.add_token(TokenType::LeftParen),
            ")" => self.add_token(TokenType::RightParen),
            "{" => self.add_token(TokenType::LeftBrace),
            "}" => self.add_token(TokenType::RightBrace),
            "," => self.add_token(TokenType::Comma),
            "." => self.add_token(TokenType::Dot),
            "-" => self.add_token(TokenType::Minus),
            "+" => self.add_token(TokenType::Plus),
            ";" => self.add_token(TokenType::Semicolon),
            "*" => self.add_token(TokenType::Star),

            // multi-tokens
            "!" => {
                let is_match = self.match_next("=");
                self.add_token(if is_match {TokenType::BangEqual} else {TokenType::Bang})
            },
            "=" => {
                let is_match = self.match_next("=");
                self.add_token(if is_match {TokenType::EqualEqual} else {TokenType::Equal})
            },
            "<" => {
                let is_match = self.match_next("=");
                self.add_token(if is_match {TokenType::LessEqual} else  {TokenType::Less})
            },
            ">" => {
                let is_match = self.match_next("=");
                self.add_token(if is_match {TokenType::GreaterEqual} else  {TokenType::Greater})
            },
            "/" => {
                if self.match_next("/") {
                    // A comment goes to the end of the line
                    while self.peek() != "\n" && !self.is_at_end() { self.advance(); }
                } else {
                    self.add_token(TokenType::Slash)
                }
            },

            // whitespace
            " " => {},
            "\t" => {},
            "\n" => self.line += 1,
            "\r" => {},

            // literals
            "\"" => {
                match self.string() {
                    Err(e) => return Err(e),
                    _ => (),
                }
            },

            // ruh roh
            _ => {
                if is_digit(c) {
                    self.number();
                } else if is_alpha(c) {
                    self.identifier();
                } else {
                    return Err("Unexpected character")
                }
            }
        }

        return Ok(())
    }

    // HELPER FUNCTIONS
    fn next_char(&self, i: usize) -> usize {
        self.offset_char(i, 1)
    }

    fn offset_char(&self, i: usize, offset: i64) -> usize {
        let mut index = i;
        let mut rem_offset = offset;
        if offset > 0 {
            while rem_offset != 0 {
                index += 1;
                while !self.source.is_char_boundary(index) {
                    index += 1
                }
                rem_offset -= 1;
            }
        } else {
            while rem_offset != 0 {
                index -= 1;
                while !self.source.is_char_boundary(index) {
                    index -= 1
                }
                rem_offset += 1;
            }
        }

        return index
    }


    fn is_at_end(&self) -> bool {
        return self.current >= self.source.len().try_into().unwrap()
    }

    fn get_char(&self, i: usize) -> &str {
        &self.source[i..self.next_char(i)]
    }

    fn advance(&mut self) -> &str {
        let current = self.current.clone();
        self.current = self.next_char(current);
        let next = self.get_char(current);
        return next;
    }

    fn match_next(&mut self, expected: &str) -> bool {
        if self.is_at_end() {
            return false
        }

        if self.get_char(self.current) != expected {
            return false;
        }

        self.current = self.next_char(self.current);
        return true;
    }

    fn peek(&self) -> &str {
        if self.is_at_end() {
            return "\0";
        } else {
            return self.get_char(self.current);
        }
    }

    fn peek_next(&self) -> &str {
        if self.next_char(self.current) > self.source.len().try_into().unwrap() { return "\0" }

        return self.get_char(self.next_char(self.current));
    }

    fn string(&mut self) -> Result<(), &str> {
        while self.peek() != "\"" && !self.is_at_end() {
            if self.peek() == "\n" {self.line += 1}
            self.advance();
        }

        if self.is_at_end() {
            return Err("Unterminated string");
        }

        self.advance();
        self.add_token(TokenType::String(&self.source[self.next_char(self.start)..self.offset_char(self.current, -1)]));

        return Ok(())
    }

    fn number(&mut self) {
        while is_digit(self.peek()) { self.advance(); };

        if self.peek() == "." && is_digit(self.peek_next()) {
            self.advance();

            while is_digit(self.peek()) { self.advance(); };
        }

        // f64::from_str(self.source[self.start..self.current]).unwrap()
        self.add_token(TokenType::Number(f64::from_str(&self.source[self.start..self.next_char(self.current)]).unwrap()))
    }

    fn identifier(&mut self) {
        while is_alphanumeric(self.peek()) { self.advance(); };

        let text = &self.source[self.start..self.next_char(self.current)];
        match get_keyword(&text) {
            Ok(t) => self.add_token(t),
            Err(_) => self.add_token(TokenType::Identifier(&text))
        };
    }

    fn add_token(&mut self, t: TokenType<'code>) {
        let lexeme = &self.source[self.start..self.current];
        self.tokens.push(Token { t, lexeme, line: self.line})
    }
}
