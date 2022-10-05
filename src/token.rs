use std::fmt;
use std::hash::{Hash, Hasher};

use crate::PrettyPrinting;

#[derive(Clone, Debug, PartialEq)]
pub enum TokenType {
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
    String(String),
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

impl fmt::Display for TokenType {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            TokenType::Number(v) => write!(f, "Number {}", v),
            TokenType::String(v) => write!(f, "String {}", v),
            _ => write!(f, "{:?}", self),
        }
    }
}

// offset is only used to ensure unique hash for each token
#[derive(Clone, Debug)]
pub struct Token {
    pub t: TokenType,
    pub lexeme: String,
    pub line: usize,
    offset: usize,
}

impl Token {
    pub fn new(t: TokenType, lexeme: impl AsRef<str>, line: usize, offset: usize) -> Self {
        Token {
            t,
            lexeme: lexeme.as_ref().to_string(),
            line,
            offset,
        }
    }
}

impl PartialEq for Token {
    fn eq(&self, other: &Self) -> bool {
        self.lexeme == other.lexeme && self.line == other.line && self.offset == other.offset
    }
}

impl Hash for Token {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.lexeme.hash(state);
        self.line.hash(state);
        self.offset.hash(state);
    }
}

impl Eq for Token {}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "[{}] {} {}", self.line, self.t, self.lexeme)
    }
}

impl PrettyPrinting for Token {
    fn print(&self) -> String {
        match self.t {
            TokenType::Number(v) => format!("{}", v),
            _ => self.lexeme.to_string(),
        }
    }
}
