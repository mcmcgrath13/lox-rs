use crate::token::Token;
use crate::PrettyPrinting;

#[derive(Clone, Debug)]
pub enum Expr {
    Assign {name: Token,
        value: Box<Expr>,
    },
    Binary {
        left: Box<Expr>,
        op: Token,
        right: Box<Expr>,
    },
    Call {
        callee: Box<Expr>,
        paren: Token,
        arguments: Vec<Expr>,
    },
    Get {
        object: Box<Expr>,
        name: Token,
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
    Set {
        object: Box<Expr>,
        name: Token,
        value: Box<Expr>,
    },
    This {
        keyword: Token,
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
            Expr::Call {
                callee, arguments, ..
            } => {
                format!("({} {})", callee.print(), arguments.print())
            }
            Expr::Get { object, name } => format!("(get {} {})", name.print(), object.print()),
            Expr::Grouping { expression } => format!("(group {})", expression.print()),
            Expr::Literal { value } => value.print(),
            Expr::Logical { left, op, right } => {
                format!("({} {} {})", op.print(), left.print(), right.print())
            }
            Expr::Set {
                object,
                name,
                value,
            } => format!(
                "(set {} {} {})",
                name.print(),
                object.print(),
                value.print()
            ),
            Expr::This { keyword } => keyword.print(),
            Expr::Unary { right, op } => format!("({} {})", op.print(), right.print()),
            Expr::Variable { name } => name.print(),
        }
    }
}

#[derive(Clone, Debug)]
pub enum Stmt {
    Block {
        statements: Vec<Stmt>,
    },
    Class {
        name: Token,
        methods: Vec<Stmt>, // specifically Stmt::Function
    },
    Expression {
        expression: Expr,
    },
    Function {
        name: Token,
        parameters: Vec<Token>,
        body: Vec<Stmt>,
    },
    If {
        condition: Expr,
        then_branch: Box<Stmt>,
        else_branch: Option<Box<Stmt>>,
    },
    Print {
        expression: Expr,
    },
    Return {
        keyword: Token,
        value: Option<Expr>,
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
            Stmt::Block { statements } => format!("(block {})", statements.print()),
            Stmt::Class { name, methods } => format!("({} {})", name.print(), methods.print()),
            Stmt::Expression { expression } => format!("(; {})", expression.print()),
            Stmt::Function {
                name,
                parameters,
                body,
            } => {
                format!(
                    "({} {} (body {}))",
                    name.print(),
                    parameters.print(),
                    body.print()
                )
            }
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
            Stmt::Print { expression } => format!("(print {})", expression.print()),
            Stmt::Return { value, .. } => match value {
                Some(expression) => format!("(return {})", expression.print()),
                None => "(return)".to_string(),
            },
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
