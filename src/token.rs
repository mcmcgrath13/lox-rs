use std::fmt;

use crate::PrettyPrinting;

#[derive(Copy, Clone, Debug, PartialEq)]
pub enum TokenType<'literal> {
    // Single-character tokens
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

    // One or two character tokens
    Bang,
    BangEqual,
    Equal,
    EqualEqual,
    Greater,
    GreaterEqual,
    Less,
    LessEqual,

    // Literals
    Identifier,
    String(&'literal str),
    Number(f64),

    // Keywords
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

    // Other
    Eof,
}

impl fmt::Display for TokenType<'_> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            TokenType::Number(v) => write!(f, "Number {}", v),
            TokenType::String(v) => write!(f, "String {}", v),
            _ => write!(f, "{:?}", self),
        }
    }
}

#[derive(Copy, Clone, Debug, PartialEq)]
pub struct Token<'code> {
    pub t: TokenType<'code>,
    pub lexeme: &'code str,
    pub line: usize,
}

impl fmt::Display for Token<'_> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "[{}] {} {}", self.line, self.t, self.lexeme)
    }
}

impl PrettyPrinting for Token<'_> {
    fn print(&self) -> String {
        match self.t {
            TokenType::Number(v) => format!("{}", v),
            _ => self.lexeme.to_string(),
        }
    }
}
