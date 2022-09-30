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
    Logical {
        left: Box<Expr>,
        op: Token,
        right: Box<Expr>,
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
            Expr::Logical { left, op, right } => {
                format!("({} {} {})", op.print(), left.print(), right.print())
            }
            Expr::Variable { name } => name.print(),
        }
    }
}

#[derive(Clone, Debug)]
pub enum Stmt {
    Block {
        statements: Vec<Stmt>,
    },
    Expression {
        expression: Expr,
    },
    If {
        condition: Expr,
        then_branch: Box<Stmt>,
        else_branch: Option<Box<Stmt>>,
    },
    Print {
        expression: Expr,
    },
    Var {
        name: Token,
        initializer: Option<Expr>,
    },
    While {
        condition: Expr,
        body: Box<Stmt>,
    },
}

impl PrettyPrinting for Stmt {
    fn print(&self) -> String {
        match self {
            Stmt::Block { statements } => {
                let mut s = "(block".to_string();
                for statement in statements {
                    s += " ";
                    s += &statement.print();
                }
                s + ")"
            }
            Stmt::Expression { expression } => format!("(; {})", expression.print()),
            Stmt::If {
                condition,
                then_branch,
                else_branch,
            } => {
                let mut s = format!(
                    "(if {} then {} else ",
                    condition.print(),
                    then_branch.print()
                );
                match else_branch {
                    Some(v) => s += &*v.print(),
                    None => s += "<none>",
                }

                s + ")"
            }
            Stmt::Print { expression } => {
                format!("(print {})", expression.print())
            }
            Stmt::Var { name, initializer } => match initializer {
                Some(v) => format!("(var {} {})", name.print(), v.print()),
                None => format!("(var {})", name.print()),
            },
            Stmt::While { condition, body } => {
                format!("(while {} {})", condition.print(), body.print())
            }
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
