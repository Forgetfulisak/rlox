use crate::error::Result;
use crate::scanner::Token;

type Expression = Equality;

type Equality = (Comparison, Vec<(EqualOp, Comparison)>);

type Comparison = (Term, Vec<(ComparisonOp, Term)>);

type Term = (Factor, Vec<(TermOp, Factor)>);

type Factor = (Unary, Vec<(UnaryOp, Unary)>);

enum Unary {
  Unary { op: UnaryOp, unary: Box<Unary> },
  Primary(Primary),
}
enum Primary {
  Num(f64),
  String(String),
  True,
  False,
  Nil,
  Expression(Box<Expression>),
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

enum TermOp {
  Minus,
  Plus,
}

enum FactorOp {
  Slash,
  Star,
}

enum ComparisonOp {
  Greater,
  GreaterEqual,
  Less,
  LessEqual,
}

enum EqualOp {
  BangEqual,
  EqualEqual,
}

#[derive(Debug, Clone)]
enum UnaryOp {
  Minus,
  Bang,
}

pub struct Parser {
  tokens: Vec<Token>,
  current: u64,
}

impl Parser {
  pub fn new(tokens: Vec<Token>) -> Self {
    Parser { tokens, current: 0 }
  }

  fn expression(&mut self) -> Expression {
    return self.equality();
  }

  fn equality(&mut self) -> Equality {
    let expr: Comparison = self.comparison();

    expr
  }

  fn comparison(&mut self) -> Comparison {}
}
