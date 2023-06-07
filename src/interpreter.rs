use std::fmt::{Debug, Display};
use std::time::{SystemTime, UNIX_EPOCH};

use crate::environ::Environ;
use crate::parser::{Expression, Literal, Operator, Stmt, UnaryOp};

pub struct Interpreter {
  pub env: Environ,
}

impl Interpreter {
  pub fn new() -> Self {
    let mut inter = Interpreter {
      env: Environ::new(None),
    };

    inter
      .env
      .define("clock".to_string(), LoxValue::LoxCallable(LoxCallable::Clock));

    inter
  }

  pub fn interpret(&mut self, stmts: Vec<Stmt>) {
    for stmt in stmts {
      self.execute(stmt);
    }
  }

  pub fn execute(&mut self, stmt: Stmt) -> LoxValue {
    match stmt {
      Stmt::Expr(exp) => self.evaluate(exp),
      Stmt::Print(exp) => {
        let val = self.evaluate(exp);
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
        let prev = std::mem::take(&mut self.env);
        self.env = Environ::new(Some(Box::new(prev)));
        for decl in decls {
          self.execute(decl);
        }
        // Good lord...
        self.env = *std::mem::take(&mut self.env.enclosing).unwrap();
        LoxValue::Void
      },
      Stmt::While { cond, body } => {
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

        self.env.define(ident, val);
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
      Literal::Identifier(ident) => self.env.get(ident),
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
        self.env.assign(ident, val);

        LoxValue::Void
      },
      Expression::Logic { and1, op, and2 } => {
        let val1 = self.evaluate(*and1);

        if matches!(op, Operator::Or) {
          if is_truthy(val1.clone()) {
            return val1;
          }
        } else if !is_truthy(val1.clone()) {
          return val1;
        }
        self.evaluate(*and2)
      },
      Expression::Call { callee, args } => {
        let callee = self.evaluate(*callee);
        let args: Vec<_> = args.into_iter().map(|arg| self.evaluate(arg)).collect();

        match callee {
          LoxValue::LoxCallable(mut fun) => {
            if fun.arity() != args.len() {
              panic!("Invalid number of arguments to funciton");
            }

            fun.call(self, args)
          },
          _ => panic!("Invalid type for call"),
        }
      },
    }
  }
}

#[derive(Debug, Clone)]
pub enum LoxValue {
  // Value,
  LoxCallable(LoxCallable),
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
      LoxValue::LoxCallable(_) => f.write_str("Fun"),
      LoxValue::Nil => f.write_str("Nil"),
      LoxValue::Boolean(b) => f.write_str(&b.to_string()),
      LoxValue::Number(n) => f.write_str(&n.to_string()),
      LoxValue::String(s) => f.write_str(&s.to_string()),
      LoxValue::Void => f.write_str("Void"),
    }
  }
}

fn is_truthy(v: LoxValue) -> bool {
  match v {
    LoxValue::LoxCallable(_) => true,
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

#[derive(Debug, Clone)]
pub enum LoxCallable {
  Clock,
  _Normal,
}

impl LoxCallable {
  pub fn call(&mut self, _interpreter: &mut Interpreter, _args: Vec<LoxValue>) -> LoxValue {
    match self {
      LoxCallable::Clock => LoxValue::Number(
        SystemTime::now()
          .duration_since(UNIX_EPOCH)
          .expect("Time goes forward")
          .as_secs_f64(),
      ),
      LoxCallable::_Normal => todo!(),
    }
  }

  pub fn arity(&self) -> usize {
    match self {
      LoxCallable::Clock => 0,
      LoxCallable::_Normal => 0,
    }
  }
}

// trait LoxCallable {
//   fn arity(&self) -> usize;
//   fn call(&mut self, interpreter: &mut Interpreter, args: Vec<LoxValue>) -> LoxValue;
// }
// impl Debug for dyn LoxCallable {
//   fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
//     write!(f, "LoxCallable")
//   }
// }

// #[derive(Debug, Clone)]
// pub struct LoxFunction {
//   arity: usize,
// }

// impl LoxCallable for LoxFunction {
//   fn call(&mut self, interpreter: &mut Interpreter, args: Vec<LoxValue>) -> LoxValue {
//     todo!()
//   }

//   fn arity(&self) -> usize {
//     todo!()
//   }
// }

// #[derive(Debug, Clone)]
// pub struct ClockFunction {}

// impl LoxCallable for ClockFunction {
//   fn call(&mut self, _interpreter: &mut Interpreter, _args: Vec<LoxValue>) -> LoxValue {
//     dbg!("called clockfunction");
//     LoxValue::Number(
//       SystemTime::now()
//         .duration_since(UNIX_EPOCH)
//         .expect("Time goes forward")
//         .as_secs_f64(),
//     )
//   }

//   fn arity(&self) -> usize {
//     0
//   }
// }

/*
|| {

        } */
