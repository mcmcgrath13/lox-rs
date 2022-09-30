use crate::token::Token;
use crate::PrettyPrinting;

#[derive(Clone, Debug)]
pub enum Expr {
    Assign {
        name: Token,
        value: Box<Expr>,
    },
    Binary {
        left: Box<Expr>,
        op: Token,
        right: Box<Expr>,
    },
    Grouping {
        expression: Box<Expr>,
    },
    Literal {
        value: Token,
    },
    Unary {
        op: Token,
        right: Box<Expr>,
    },
    Variable {
        name: Token,
    },
}

impl PrettyPrinting for Expr {
    fn print(&self) -> String {
        match self {
            Expr::Assign { name, value } => format!("(= {} {})", name.print(), value.print()),
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

pub enum Stmt {
    Block {
        statements: Vec<Stmt>,
    },
    Expression {
        expression: Expr,
    },
    Print {
        expression: Expr,
    },
    Var {
        name: Token,
        initializer: Option<Expr>,
    },
}

impl PrettyPrinting for Stmt {
    fn print(&self) -> String {
        match self {
            Stmt::Block { statements } => {
                let mut s = "(block".to_string();
                for statement in statements {
                    s = s + &format!(" {}", statement.print())
                }
                s + ")"
            }
            Stmt::Expression { expression } => format!("(; {})", expression.print()),
            Stmt::Print { expression } => {
                format!("(print {})", expression.print())
            }
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
