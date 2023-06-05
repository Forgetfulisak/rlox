use std::fmt::Display;

use crate::error::Result;
use crate::parser::{Expression, Literal, Operator, UnaryOp};

pub fn interpreter(exp: Expression) {
  let val = evaluate(exp);

  println!("{}", val);
}

#[derive(Debug)]
pub enum LoxValue {
  Value,
  Nil,
  Boolean(bool),
  Number(f64),
  String(String),
}
impl Display for LoxValue {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    match self {
      LoxValue::Value => f.write_str("Value"),
      LoxValue::Nil => f.write_str("Nil"),
      LoxValue::Boolean(b) => f.write_str(&format!("{}", b)),
      LoxValue::Number(n) => f.write_str(&format!("{}", n)),
      LoxValue::String(s) => f.write_str(&format!("{}", s)),
    }
  }
}

impl From<Literal> for LoxValue {
  fn from(lit: Literal) -> Self {
    match lit {
      Literal::Num(n) => LoxValue::Number(n),
      Literal::String(s) => LoxValue::String(s),
      Literal::True => LoxValue::Boolean(true),
      Literal::False => LoxValue::Boolean(false),
      Literal::Nil => LoxValue::Nil,
    }
  }
}

fn is_truthy(lit: Literal) -> bool {
  match lit {
    Literal::Num(_) => true,
    Literal::String(_) => true,
    Literal::True => true,
    Literal::False => false,
    Literal::Nil => false,
  }
}

fn is_equal(v1: LoxValue, v2: LoxValue) -> bool {
  match (v1, v2) {
    (LoxValue::Value, LoxValue::Value) => false,
    (LoxValue::Nil, LoxValue::Nil) => true,
    (LoxValue::Boolean(a), LoxValue::Boolean(b)) => a == b,
    (LoxValue::Number(n1), LoxValue::Number(n2)) => n1 == n2,
    (LoxValue::String(s1), LoxValue::String(s2)) => s1 == s2,
    (v1, v2) => panic!("Comparison between incompatible types: {:?}, {:?}", v1, v2),
  }
}

fn is_less(v1: LoxValue, v2: LoxValue) -> bool {
  match (v1, v2) {
    (LoxValue::Number(n1), LoxValue::Number(n2)) => n1 < n2,
    (v1, v2) => panic!("wront types for is_less {:?}, {:?}", v1, v2),
  }
}

fn is_less_eq(v1: LoxValue, v2: LoxValue) -> bool {
  match (v1, v2) {
    (LoxValue::Number(n1), LoxValue::Number(n2)) => n1 <= n2,
    (v1, v2) => panic!("wront types for is_less {:?}, {:?}", v1, v2),
  }
}

pub fn evaluate(exp: Expression) -> LoxValue {
  match exp {
    Expression::Literal(lit) => lit.into(),
    Expression::Binary { exp1, op, exp2 } => {
      let v1 = evaluate(*exp1);
      let v2 = evaluate(*exp2);

      match (op, v1, v2) {
        (Operator::EqualEqual, v1, v2) => LoxValue::Boolean(is_equal(v1, v2)),
        (Operator::BangEqual, v1, v2) => LoxValue::Boolean(!is_equal(v1, v2)),

        (Operator::Less, v1, v2) => LoxValue::Boolean(is_less(v1, v2)),
        (Operator::LessEqual, v1, v2) => LoxValue::Boolean(is_less_eq(v1, v2)),

        (Operator::Greater, v1, v2) => LoxValue::Boolean(!is_less_eq(v1, v2)),
        (Operator::GreaterEqual, v1, v2) => LoxValue::Boolean(!is_less(v1, v2)),

        (Operator::Plus, LoxValue::Number(n1), LoxValue::Number(n2)) => LoxValue::Number(n1 + n2),
        (Operator::Plus, LoxValue::String(s1), LoxValue::String(s2)) => LoxValue::String(s1 + &s2),
        (Operator::Plus, v1, v2) => panic!("wrong type for Plus"),

        (Operator::Minus, LoxValue::Number(n1), LoxValue::Number(n2)) => LoxValue::Number(n1 - n2),
        (Operator::Minus, v1, v2) => panic!("wrong type for Minus"),

        (Operator::Star, LoxValue::Number(n1), LoxValue::Number(n2)) => LoxValue::Number(n1 * n2),
        (Operator::Star, v1, v2) => panic!("wrong type for Star"),

        (Operator::Slash, LoxValue::Number(n1), LoxValue::Number(n2)) => LoxValue::Number(n1 / n2),
        (Operator::Slash, v1, v2) => panic!("wrong type for Slash"),
      }
    },
    Expression::Unary { op, exp } => match (op, *exp) {
      (UnaryOp::Minus, Expression::Literal(Literal::Num(n))) => LoxValue::Number(-n),
      (UnaryOp::Minus, exp) => evaluate(exp),
      (UnaryOp::Bang, Expression::Literal(lit)) => LoxValue::Boolean(!is_truthy(lit)),
      (UnaryOp::Bang, exp) => match evaluate(exp) {
        LoxValue::Boolean(b) => LoxValue::Boolean(!b),
        exp => panic!("Bang operator, but got: {:?}", exp),
      },
    },
  }
}
