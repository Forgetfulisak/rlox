use std::collections::HashMap;
use std::fmt::Display;

use crate::parser::{Expression, Literal, Operator, Stmt, UnaryOp};

pub struct Interpreter {
  pub values: HashMap<String, LoxValue>,
}

impl Interpreter {
  pub fn interpret(&mut self, stmts: Vec<Stmt>) {
    for stmt in stmts {
      self.execute(stmt);
    }
  }

  pub fn execute(&mut self, stmt: Stmt) -> LoxValue {
    match stmt {
      Stmt::ExprStmt(exp) => self.evaluate(*exp),
      Stmt::PrintStmt(exp) => {
        let val = self.evaluate(*exp);
        println!("{}", val);
        LoxValue::Void
      },
      Stmt::IfStmt {
        cond,
        if_stmt,
        else_stmt,
      } => {
        if is_truthy(self.evaluate(cond)) {
          self.execute(*if_stmt);
        } else if let Some(stmt) = else_stmt {
          self.execute(*stmt);
        }
        LoxValue::Void
      },
      Stmt::Block(decls) => {
        for decl in decls {
          self.execute(decl);
        }
        LoxValue::Void
      },
      Stmt::WhileStmt { cond, body } => {
        while is_truthy(self.evaluate(cond.clone())) {
          self.execute(*body.clone());
        }

        LoxValue::Void
      },
      Stmt::VarDecl { ident, exp } => {
        let val: LoxValue = match exp {
          Some(exp) => self.evaluate(exp),
          None => LoxValue::Nil,
        };

        self.values.insert(ident, val);
        LoxValue::Void
      },
      // Stmt::ForStmt { .. } => panic!("Should be desugared away"),
    }
  }

  fn val_from_lit(&self, lit: Literal) -> LoxValue {
    match lit {
      Literal::Num(n) => LoxValue::Number(n),
      Literal::String(s) => LoxValue::String(s),
      Literal::True => LoxValue::Boolean(true),
      Literal::False => LoxValue::Boolean(false),
      Literal::Nil => LoxValue::Nil,
      Literal::Identifier(ident) => self.values.get(&ident).expect("Fetching variable").clone(),
    }
  }

  pub fn evaluate(&mut self, exp: Expression) -> LoxValue {
    match exp {
      Expression::Literal(lit) => self.val_from_lit(lit),
      Expression::Binary { exp1, op, exp2 } => {
        let v1 = self.evaluate(*exp1);
        let v2 = self.evaluate(*exp2);

        match (op, v1, v2) {
          (Operator::EqualEqual, v1, v2) => LoxValue::Boolean(is_equal(v1, v2)),
          (Operator::BangEqual, v1, v2) => LoxValue::Boolean(!is_equal(v1, v2)),

          (Operator::Less, v1, v2) => LoxValue::Boolean(is_less(v1, v2)),
          (Operator::LessEqual, v1, v2) => LoxValue::Boolean(is_less_eq(v1, v2)),

          (Operator::Greater, v1, v2) => LoxValue::Boolean(!is_less_eq(v1, v2)),
          (Operator::GreaterEqual, v1, v2) => LoxValue::Boolean(!is_less(v1, v2)),

          (Operator::Plus, LoxValue::Number(n1), LoxValue::Number(n2)) => LoxValue::Number(n1 + n2),
          (Operator::Plus, LoxValue::String(s1), LoxValue::String(s2)) => LoxValue::String(s1 + &s2),
          (Operator::Plus, _v1, _v2) => panic!("wrong type for Plus"),

          (Operator::Minus, LoxValue::Number(n1), LoxValue::Number(n2)) => LoxValue::Number(n1 - n2),
          (Operator::Minus, _v1, _v2) => panic!("wrong type for Minus"),

          (Operator::Star, LoxValue::Number(n1), LoxValue::Number(n2)) => LoxValue::Number(n1 * n2),
          (Operator::Star, _v1, _v2) => panic!("wrong type for Star"),

          (Operator::Slash, LoxValue::Number(n1), LoxValue::Number(n2)) => LoxValue::Number(n1 / n2),
          (Operator::Slash, _v1, _v2) => panic!("wrong type for Slash"),

          (Operator::Mod, LoxValue::Number(n1), LoxValue::Number(n2)) => LoxValue::Number(n1 % n2),
          (Operator::Mod, _v1, _v2) => panic!("wrong type for Slash"),

          (Operator::And, _, _) => panic!("Parse error, unexpected AND"),
          (Operator::Or, _, _) => panic!("Parse error, unexpected OR"),
        }
      },
      Expression::Unary { op, exp } => match (op, *exp) {
        (UnaryOp::Minus, exp) => match self.evaluate(exp) {
          LoxValue::Number(n) => LoxValue::Number(-n),
          exp => panic!("Minus operator, but got: {:?}", exp),
        },
        (UnaryOp::Bang, exp) => LoxValue::Boolean(!is_truthy(self.evaluate(exp))),
      },
      Expression::Assignment { ident, value } => {
        let val = self.evaluate(*value);
        if self.values.contains_key(&ident) {
          self.values.insert(ident, val.clone());
          return val;
        }

        panic!("Undefined variable")
      },
      Expression::Logic { and1, op, and2 } => {
        let val1 = self.evaluate(*and1);

        if matches!(op, Operator::Or) {
          if is_truthy(val1.clone()) {
            return val1;
          }
        } else {
          if !is_truthy(val1.clone()) {
            return val1;
          }
        }
        self.evaluate(*and2)
      },
    }
  }
}

#[derive(Debug, Clone)]
pub enum LoxValue {
  // Value,
  Nil,
  Boolean(bool),
  Number(f64),
  String(String),
  Void,
}
impl Display for LoxValue {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    match self {
      // LoxValue::Value => f.write_str("Value"),
      LoxValue::Nil => f.write_str("Nil"),
      LoxValue::Boolean(b) => f.write_str(&format!("{}", b)),
      LoxValue::Number(n) => f.write_str(&format!("{}", n)),
      LoxValue::String(s) => f.write_str(&format!("{}", s)),
      LoxValue::Void => f.write_str("Void"),
    }
  }
}

fn is_truthy(v: LoxValue) -> bool {
  match v {
    // LoxValue::Value => true,
    LoxValue::Nil => false,
    LoxValue::Boolean(t) => t,
    LoxValue::Number(_) => true,
    LoxValue::String(_) => true,
    LoxValue::Void => panic!("Should not check truthynes of void"),
  }
}
fn is_equal(v1: LoxValue, v2: LoxValue) -> bool {
  match (v1, v2) {
    // (LoxValue::Value, LoxValue::Value) => false,
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
