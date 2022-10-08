use std::str::FromStr;

use crate::token::{Token, TokenType};
use crate::Reportable;

pub struct Scanner<'code> {
    source: &'code String,
    tokens: Vec<Token>,
    start: usize,
    current: usize,
    line: usize,
}

pub struct ScanError {
    line: usize,
    message: String,
}

impl ScanError {
    pub fn new(line: usize, message: impl AsRef<str>) -> Self {
        Self {
            line,
            message: message.as_ref().to_string(),
        }
    }
}

impl Reportable for ScanError {
    fn report(&self) -> String {
        format!("[line {}] Error (Scanner): {}", self.line, self.message)
    }
}

// Can't have a constant hashmap, so use a slice of pairs and then search over it
const KEYWORDS: &[(&str, TokenType)] = &[
    ("and", TokenType::And),
    ("class", TokenType::Class),
    ("else", TokenType::Else),
    ("false", TokenType::False),
    ("for", TokenType::For),
    ("fun", TokenType::Fun),
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

// Use binary search to access the map
fn get_keyword(key: &str) -> Result<TokenType, usize> {
    KEYWORDS
        .binary_search_by(|(k, _)| k.cmp(&key))
        .map(|x| KEYWORDS[x].1.clone())
}

fn is_digit(c: &str) -> bool {
    ("0"..="9").contains(&c)
}

fn is_alpha(c: &str) -> bool {
    ("a"..="z").contains(&c) || ("A"..="Z").contains(&c) || c == "_"
}

fn is_alphanumeric(c: &str) -> bool {
    is_alpha(c) || is_digit(c)
}

impl<'code> Scanner<'code> {
    pub fn new(source: &'code String) -> Scanner<'code> {
        Scanner {
            source,
            tokens: Vec::new(),
            start: 0,
            current: 0,
            line: 1,
        }
    }

    pub fn scan_tokens(&mut self) -> (&Vec<Token>, Vec<ScanError>) {
        let mut errs = Vec::new();
        loop {
            match self.scan_token() {
                Err(err) => errs.push(err),
                Ok(Some(_)) => continue,
                Ok(None) => break,
            }
        }

        self.tokens
            .push(Token::new(TokenType::Eof, "", self.line, self.start));

        (&self.tokens, errs)
    }

    fn scan_token(&mut self) -> Result<Option<()>, ScanError> {
        if self.is_at_end() {
            return Ok(None);
        }
        self.start = self.current;
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
                self.add_token(if is_match {
                    TokenType::BangEqual
                } else {
                    TokenType::Bang
                })
            }
            "=" => {
                let is_match = self.match_next("=");
                self.add_token(if is_match {
                    TokenType::EqualEqual
                } else {
                    TokenType::Equal
                })
            }
            "<" => {
                let is_match = self.match_next("=");
                self.add_token(if is_match {
                    TokenType::LessEqual
                } else {
                    TokenType::Less
                })
            }
            ">" => {
                let is_match = self.match_next("=");
                self.add_token(if is_match {
                    TokenType::GreaterEqual
                } else {
                    TokenType::Greater
                })
            }
            "/" => {
                if self.match_next("/") {
                    // A comment goes to the end of the line
                    while self.peek() != "\n" && !self.is_at_end() {
                        self.advance();
                    }
                } else {
                    self.add_token(TokenType::Slash)
                }
            }

            // whitespace
            " " => {}
            "\t" => {}
            "\n" => self.line += 1,
            "\r" => {}

            // literals
            "\"" => return self.string(),

            // ruh roh
            _ => {
                if is_digit(c) {
                    self.number();
                } else if is_alpha(c) {
                    self.identifier();
                } else {
                    let message = format!("Unexpected character: {}", c);
                    return Err(self.error(message));
                }
            }
        }

        Ok(Some(()))
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

        index
    }

    fn is_at_end(&self) -> bool {
        let length = self.source.len();
        self.current >= length
            || (self.current == length - 1 && self.get_char(self.current) == "\n")
    }

    fn get_char(&self, i: usize) -> &str {
        &self.source[i..self.next_char(i)]
    }

    fn advance(&mut self) -> &str {
        let current = self.current;
        self.current = self.next_char(current);

        self.get_char(current) as _
    }

    fn match_next(&mut self, expected: &str) -> bool {
        if self.is_at_end() {
            return false;
        }

        if self.get_char(self.current) != expected {
            return false;
        }

        self.current = self.next_char(self.current);
        true
    }

    fn peek(&self) -> &str {
        if self.is_at_end() {
            "\0"
        } else {
            self.get_char(self.current)
        }
    }

    fn peek_next(&self) -> &str {
        if self.next_char(self.current) > self.source.len() {
            return "\0";
        }

        self.get_char(self.next_char(self.current))
    }

    fn string(&mut self) -> Result<Option<()>, ScanError> {
        while self.peek() != "\"" && !self.is_at_end() {
            if self.peek() == "\n" {
                self.line += 1
            }
            self.advance();
        }

        if self.is_at_end() {
            return Err(self.error("Unterminated string"));
        }

        self.advance();
        self.add_token(TokenType::String(
            (self.source[self.next_char(self.start)..self.offset_char(self.current, -1)])
                .to_string(),
        ));

        Ok(Some(()))
    }

    fn number(&mut self) {
        while is_digit(self.peek()) {
            self.advance();
        }

        if self.peek() == "." && is_digit(self.peek_next()) {
            self.advance();

            while is_digit(self.peek()) {
                self.advance();
            }
        }

        // f64::from_str(self.source[self.start..self.current]).unwrap()
        self.add_token(TokenType::Number(
            f64::from_str(&self.source[self.start..self.current]).unwrap(),
        ))
    }

    fn identifier(&mut self) {
        while is_alphanumeric(self.peek()) {
            self.advance();
        }

        let text = &self.source[self.start..self.current];
        match get_keyword(text) {
            Ok(t) => self.add_token(t),
            Err(_) => self.add_token(TokenType::Identifier),
        };
    }

    fn add_token(&mut self, t: TokenType) {
        let lexeme = &self.source[self.start..self.current];
        self.tokens
            .push(Token::new(t, lexeme, self.line, self.start))
    }

    fn error(&self, msg: impl AsRef<str>) -> ScanError {
        ScanError::new(self.line, msg)
    }
}
