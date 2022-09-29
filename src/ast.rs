use crate::token::Token;
use crate::PrettyPrinting;

pub enum Expr<'a> {
    Binary {
        left: Box<Expr<'a>>,
        right: Box<Expr<'a>>,
        op: Token<'a>,
    },
    Grouping {
        expression: Box<Expr<'a>>,
    },
    Literal {
        value: Token<'a>,
    },
    Unary {
        right: Box<Expr<'a>>,
        op: Token<'a>,
    },
    Variable {
        name: Token<'a>,
    },
}

impl PrettyPrinting for Expr<'_> {
    fn print(&self) -> String {
        match self {
            Expr::Binary { left, right, op } => {
                format!("({} {} {})", op.print(), left.print(), right.print())
            }
            Expr::Unary { right, op } => format!("({} {})", op.print(), right.print()),
            Expr::Grouping { expression } => format!("(group {})", expression.print()),
            Expr::Literal { value } => value.print(),
            Expr::Variable { name } => name.print(),
        }
    }
}

pub enum Stmt<'a> {
    Expression {
        expression: Expr<'a>,
    },
    Print {
        expression: Expr<'a>,
    },
    Var {
        name: Token<'a>,
        initializer: Option<Expr<'a>>,
    },
}

// TODO: JUST USE FMT::DISPLAY
impl PrettyPrinting for Stmt<'_> {
    fn print(&self) -> String {
        match self {
            Stmt::Print { expression } => {
                format!("(print {} )", expression.print())
            }
            Stmt::Expression { expression } => format!("(; {})", expression.print()),
            Stmt::Var { name, initializer } => match initializer {
                Some(v) => format!("(var {} {})", name.print(), v.print()),
                None => format!("(var {})", name.print()),
            },
        }
    }
}

#[cfg(test)]
mod tests {
    // Note this useful idiom: importing names from outer (for mod tests) scope.
    use super::*;
    use crate::token::{Token, TokenType};

    #[test]
    fn test_pretty_print() {
        let expression = Expr::Binary {
            left: Box::new(Expr::Unary {
                op: Token {
                    t: TokenType::Minus,
                    lexeme: "-",
                    line: 1,
                },
                right: Box::new(Expr::Literal {
                    value: Token {
                        t: TokenType::Number(123.0),
                        lexeme: "45.67",
                        line: 1,
                    },
                }),
            }),
            op: Token {
                t: TokenType::Star,
                lexeme: "*",
                line: 1,
            },
            right: Box::new(Expr::Grouping {
                expression: Box::new(Expr::Literal {
                    value: Token {
                        t: TokenType::Number(45.67),
                        lexeme: "45.67",
                        line: 1,
                    },
                }),
            }),
        };

        assert_eq!(expression.print(), "(* (- 123) (group 45.67))");
    }
}
