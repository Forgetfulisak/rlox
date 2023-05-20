use crate::error::Result;
use crate::scanner::Token;

#[derive(Debug, Clone)]
enum Expression {
  Literal(Literal),
  Binary {
    exp1: Box<Expression>,
    op: Operator,
    exp2: Box<Expression>,
  },
  Unary {
    op: UnaryOp,
    exp: Box<Expression>,
  },
  Grouping(Box<Expression>),
}

#[derive(Debug, Clone)]
enum Literal {
  Num(f64),
  String(String),
  True,
  False,
  Nil,
}

#[derive(Debug, Clone)]
enum Operator {
  EqualEqual,
  BangEqual,
  Less,
  LessEqual,
  Greater,
  GreaterEqual,
  Plus,
  Minus,
  Star,
  Slash,
}

#[derive(Debug, Clone)]
enum UnaryOp {
  Minus,
  Bang,
}

fn print_exp(exp: Expression) -> String {
  match exp {
    Expression::Literal(lit) => format!("{:?}", lit),
    Expression::Binary { exp1, op, exp2 } => {
      format!("{} {:?} {}", print_exp(exp1.as_ref().clone()), op, print_exp(exp2.as_ref().clone()))
    },
    Expression::Unary { op, exp } => format!("{:?}({})", op, print_exp(exp.as_ref().clone())),
    Expression::Grouping(exp) => format!("({})", print_exp(exp.as_ref().clone())),
  }
}

#[cfg(test)]
mod tests {
  use crate::parser::{print_exp, Expression, Literal, Operator, UnaryOp};

  #[test]

  fn print_tree() {
    let exp = Expression::Unary {
      op: UnaryOp::Minus,
      exp: Box::new(Expression::Binary {
        exp1: Box::new(Expression::Grouping(Box::new(Expression::Binary {
          exp1: Box::new(Expression::Literal(Literal::Num(42.))),
          op: Operator::Plus,
          exp2: Box::new(Expression::Literal(Literal::Num(42.))),
        }))),
        op: Operator::Minus,
        exp2: Box::new(Expression::Literal(Literal::Num(42.))),
      }),
    };
    //  op: Operator::Plus, exp2: Literal::Num(42.) }), op: Operator::Minus, exp2: Box::new(Literal::Num(2.)) }
    let yo = print_exp(exp);
    println!("{}", yo);
  }
}
