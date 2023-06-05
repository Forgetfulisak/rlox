use crate::error::Result;
use crate::scanner::Token;
use anyhow::anyhow;

// struct program(Vec<Stmt>)

pub enum Stmt {
  ExprSttmt(Box<Expression>),
  PrintStmt(Box<Expression>),
}

#[derive(Debug, Clone)]
pub enum Expression {
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
}

#[derive(Debug, Clone)]
pub enum Literal {
  Num(f64),
  String(String),
  True,
  False,
  Nil,
}

#[derive(Debug, Clone)]
pub enum Operator {
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

impl TryFrom<Token> for Operator {
  type Error = anyhow::Error;

  fn try_from(tok: Token) -> std::result::Result<Self, Self::Error> {
    match tok {
      Token::Minus => Ok(Operator::Minus),
      Token::Plus => Ok(Operator::Plus),
      Token::Slash => Ok(Operator::Slash),
      Token::Star => Ok(Operator::Star),
      // Token::Bang => Ok(Operator::Bang),
      Token::BangEqual => Ok(Operator::BangEqual),
      // Token::Equal => Ok(Operator::Equal),
      Token::EqualEqual => Ok(Operator::EqualEqual),
      Token::Greater => Ok(Operator::Greater),
      Token::GreaterEqual => Ok(Operator::GreaterEqual),
      Token::Less => Ok(Operator::Less),
      Token::LessEqual => Ok(Operator::LessEqual),
      tok => Err(anyhow!("Unknown operator: {:?}", tok)),
    }
  }
}

#[derive(Debug, Clone)]
pub enum UnaryOp {
  Minus,
  Bang,
}

pub struct Parser {
  tokens: Vec<Token>,
  current: usize,
}

impl Parser {
  pub fn new(tokens: Vec<Token>) -> Self {
    Parser { tokens, current: 0 }
  }

  pub fn parse(&mut self) -> Result<Vec<Stmt>> {
    let mut statements: Vec<Stmt> = vec![];
    while !self.is_at_end() {
      statements.push(self.statement()?)
    }
    // let exp = self.expression()?;
    // dbg!(self.tokens.len(), self.current);
    Ok(statements)
  }

  fn is_at_end(&self) -> bool {
    if let Token::Eof = self.peek() {
      true
    } else {
      false
    }
    // self.current >= self.tokens.len()
  }

  fn advance(&mut self) -> Token {
    let tok = self.tokens.get(self.current).unwrap_or(&Token::Eof).clone();
    self.current += 1;

    tok
  }

  fn peek(&self) -> Token {
    self.tokens.get(self.current).unwrap_or(&Token::Eof).clone()
  }

  fn matchn(&mut self, toks: Vec<Token>) -> bool {
    toks.iter().any(|t| self.match1(t.clone()))
  }

  fn match2(&mut self, a: Token, b: Token) -> bool {
    if self.match1(a) || self.match1(b) {
      return true;
    }

    false
  }

  fn match1(&mut self, a: Token) -> bool {
    if std::mem::discriminant(&self.peek()) == std::mem::discriminant(&a) {
      self.advance();
      return true;
    }

    // if matches!(self.peek(), a) {}
    false
  }

  fn previous(&mut self) -> Token {
    self.tokens[self.current - 1].clone()
  }

  fn statement(&mut self) -> Result<Stmt> {
    if self.match1(Token::Print) {
      self.print_statement()
    } else {
      self.expression_statment()
    }
  }

  fn print_statement(&mut self) -> Result<Stmt> {
    let exp = self.expression()?;
    let semicolon = self.advance();
    if let Token::Semicolon = semicolon {
    } else {
      Err(anyhow!("Expected ; after value"))?
    }

    Ok(Stmt::PrintStmt(Box::new(exp)))
  }
  fn expression_statment(&mut self) -> Result<Stmt> {
    let exp = self.expression()?;
    let semicolon = self.advance();
    if let Token::Semicolon = semicolon {
    } else {
      Err(anyhow!("Expected ; after value"))?
    }

    Ok(Stmt::ExprSttmt(Box::new(exp)))
  }

  fn expression(&mut self) -> Result<Expression> {
    return self.equality();
  }

  fn equality(&mut self) -> Result<Expression> {
    let mut exp = self.comparison()?;
    // dbg!(exp.clone());
    while self.matchn(vec![Token::BangEqual, Token::EqualEqual]) {
      let op = self.previous();

      exp = Expression::Binary {
        exp1: Box::new(exp),
        op: op.try_into()?,
        exp2: Box::new(self.comparison()?),
      }
    }
    // dbg!(&exp);

    Ok(exp)
  }

  fn comparison(&mut self) -> Result<Expression> {
    let mut exp = self.term()?;

    while self.matchn(vec![Token::Greater, Token::GreaterEqual, Token::Less, Token::LessEqual]) {
      let op = self.previous();
      exp = Expression::Binary {
        exp1: Box::new(exp),
        op: op.try_into()?,
        exp2: Box::new(self.term()?),
      }
    }
    // dbg!(&exp);

    Ok(exp)
  }
  fn term(&mut self) -> Result<Expression> {
    let mut exp = self.factor()?;

    while self.matchn(vec![Token::Minus, Token::Plus]) {
      let op = self.previous();

      exp = Expression::Binary {
        exp1: Box::new(exp),
        op: op.try_into()?,
        exp2: Box::new(self.factor()?),
      }
    }
    // dbg!(&exp);

    Ok(exp)
  }
  fn factor(&mut self) -> Result<Expression> {
    let mut exp = self.unary()?;
    while self.match2(Token::Slash, Token::Star) {
      let op = self.previous();

      exp = Expression::Binary {
        exp1: Box::new(exp),
        op: op.try_into()?,
        exp2: Box::new(self.unary()?),
      }
    }
    // dbg!(&exp);

    Ok(exp)
  }
  fn unary(&mut self) -> Result<Expression> {
    let exp = match self.peek() {
      Token::Bang => {
        self.advance();
        Ok(Expression::Unary {
          op: UnaryOp::Bang,
          exp: Box::new(self.unary()?),
        })
      },
      Token::Minus => {
        self.advance();
        Ok(Expression::Unary {
          op: UnaryOp::Minus,
          exp: Box::new(self.unary()?),
        })
      },
      _ => self.primary(),
    }?;
    // dbg!(&exp);
    // assert!(self.match1(Token::Semicolon));

    Ok(exp)
  }
  fn primary(&mut self) -> Result<Expression> {
    let tok = match self.advance() {
      Token::LeftParen => {
        // self.advance();
        let exp = self.expression()?;
        let paren = self.advance();
        // dbg!(&self.tokens);
        // dbg!(&paren);

        if let Token::RightParen = paren {
          Ok(exp)
        } else {
          Err(anyhow!("Expected right paren, got: {:?}", paren))
        }
      },
      Token::String(s) => Ok(Expression::Literal(Literal::String(s))),
      Token::Number(n) => Ok(Expression::Literal(Literal::Num(n))),
      Token::Nil => Ok(Expression::Literal(Literal::Nil)),
      Token::True => Ok(Expression::Literal(Literal::True)),
      Token::False => Ok(Expression::Literal(Literal::False)),
      tok => Err(anyhow!("invalid token: {:?}", tok)),
    }?;

    Ok(tok)
  }
}
