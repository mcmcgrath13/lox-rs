use std::fmt;

#[derive(Copy, Clone)]
pub enum TokenType<'literal> {
    // Single-character tokens.
    LeftParen, RightParen, LeftBrace, RightBrace,
    Comma, Dot, Minus, Plus, Semicolon, Slash, Star,

    // One or two character tokens.
    Bang, BangEqual,
    Equal, EqualEqual,
    Greater, GreaterEqual,
    Less, LessEqual,

    // Literals.
    Identifier(&'literal str),
    String(&'literal str),
    Number(f64),

    // Keywords.
    And, Class, Else, False, Fun, For, If, Nil, Or,
    Print, Return, Super, This, True, Var, While,

    Eof
}

impl fmt::Display for TokenType<'_> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            TokenType::Identifier(v) => write!(f, "Identifier {}", v),
            TokenType::Number(v) => write!(f, "Number {}", v),
            TokenType::String(v) => write!(f, "String {}", v),
            _ => write!(f, "Something else")
        }
    }
}

pub struct Token<'code> {
    pub t: TokenType<'code>,
    pub lexeme: &'code str,
    pub line: usize
}

// TODO: fix this
impl fmt::Display for Token<'_> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "[{}] {} {}", self.line, self.t, self.lexeme)
    }
}
