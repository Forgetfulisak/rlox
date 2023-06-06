use crate::error::Result;
use crate::scanner::Token;
use anyhow::anyhow;

// struct program(Vec<Stmt>)

#[derive(Debug, Clone)]
pub enum Stmt {
  VarDecl {
    ident: String,
    exp: Option<Expression>,
  },
  ExprStmt(Box<Expression>),
  IfStmt {
    cond: Expression,
    if_stmt: Box<Stmt>,
    else_stmt: Option<Box<Stmt>>,
  },
  PrintStmt(Box<Expression>),
  WhileStmt {
    cond: Expression,
    body: Box<Stmt>,
  },
  // ForStmt {
  //   init: Option<Box<Stmt>>,
  //   cond: Option<Expression>,
  //   inc: Option<Expression>,
  //   body: Box<Stmt>,
  // },
  Block(Vec<Stmt>),
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
  Assignment {
    // And now the cracks are starting to show
    ident: String,
    value: Box<Expression>,
  },
  Logic {
    and1: Box<Expression>,
    op: Operator,
    and2: Box<Expression>,
  },
}

#[derive(Debug, Clone)]
pub enum Literal {
  Num(f64),
  String(String),
  True,
  False,
  Nil,
  Identifier(String), // Kinda messy cause my `Literal` is their `primary`
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
  Or,
  And,
  Mod,
}

impl TryFrom<Token> for Operator {
  type Error = anyhow::Error;

  fn try_from(tok: Token) -> std::result::Result<Self, Self::Error> {
    match tok {
      Token::Minus => Ok(Operator::Minus),
      Token::Plus => Ok(Operator::Plus),
      Token::Slash => Ok(Operator::Slash),
      Token::Star => Ok(Operator::Star),
      Token::Mod => Ok(Operator::Mod),
      Token::BangEqual => Ok(Operator::BangEqual),
      Token::Or => Ok(Operator::Or),
      Token::And => Ok(Operator::And),
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
    let mut stmts: Vec<Stmt> = vec![];
    while !self.is_at_end() {
      stmts.push(self.declaration()?)
    }
    // let exp = self.expression()?;
    // dbg!(self.tokens.len(), self.current);
    Ok(stmts)
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

  fn check(&self, tok: Token) -> bool {
    std::mem::discriminant(&self.peek()) == std::mem::discriminant(&tok)
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
    if self.check(a) {
      self.advance();
      return true;
    }

    // if matches!(self.peek(), a) {}
    false
  }

  fn previous(&mut self) -> Token {
    self.tokens[self.current - 1].clone()
  }

  fn declaration(&mut self) -> Result<Stmt> {
    if self.match1(Token::Var) {
      Ok(self.var_declaration()?)
    } else {
      let stmt = self.statement()?;
      Ok(stmt)
    }

    // match decl {
    //   Ok(decl) => Some(decl),
    //   Err(_) => None,
    // }
  }

  fn var_declaration(&mut self) -> Result<Stmt> {
    // let name = self.advance();
    if let Token::Identifier(ident) = self.advance() {
      let exp = if self.match1(Token::Equal) {
        Some(self.expression()?)
      } else {
        None
      };

      let semicolon = self.advance();
      if let Token::Semicolon = semicolon {
      } else {
        Err(anyhow!("Expected ; after value"))?
      }

      return Ok(Stmt::VarDecl { ident, exp });
    }

    Err(anyhow!("Expected identified after var"))
  }

  fn statement(&mut self) -> Result<Stmt> {
    if self.match1(Token::Print) {
      self.print_statement()
    } else if self.match1(Token::If) {
      self.if_statement()
    } else if self.match1(Token::LeftBrace) {
      self.block_statement()
    } else if self.match1(Token::While) {
      self.while_statement()
    } else if self.match1(Token::For) {
      self.for_statement()
    } else {
      self.expression_statment()
    }
  }

  fn block_statement(&mut self) -> Result<Stmt> {
    let mut decls = vec![];
    while !self.match1(Token::RightBrace) {
      decls.push(self.declaration()?);
    }

    Ok(Stmt::Block(decls))
  }

  fn if_statement(&mut self) -> Result<Stmt> {
    if !self.match1(Token::LeftParen) {
      Err(anyhow!("Expected ( in if-test"))?
    }

    let cond = self.expression()?;

    if !self.match1(Token::RightParen) {
      Err(anyhow!("Expected ) after expression in if-test"))?
    }
    let stmt = Box::new(self.statement()?);

    let else_stmt = if self.match1(Token::Else) {
      Some(Box::new(self.statement()?))
    } else {
      None
    };

    Ok(Stmt::IfStmt {
      cond,
      if_stmt: stmt,
      else_stmt,
    })
  }

  fn for_statement(&mut self) -> Result<Stmt> {
    if !self.match1(Token::LeftParen) {
      Err(anyhow!("Expect ( after for"))?;
    }

    let init = if self.match1(Token::Semicolon) {
      None
    } else if self.match1(Token::Var) {
      Some(self.var_declaration()?)
    } else {
      Some(self.expression_statment()?)
    };

    let cond = if !self.check(Token::Semicolon) {
      Some(self.expression()?)
    } else {
      None
    };

    if !self.match1(Token::Semicolon) {
      Err(anyhow!("Expected ; for-loop-condition"))?
    }

    let inc = if !self.check(Token::RightParen) {
      Some(self.expression()?)
    } else {
      None
    };

    if !self.match1(Token::RightParen) {
      Err(anyhow!("Expected ) after for clauses"))?
    }

    let mut body = self.statement()?;

    if let Some(inc) = inc {
      body = Stmt::Block(vec![body, Stmt::ExprStmt(Box::new(inc))]);
    }

    let cond = cond.unwrap_or_else(|| Expression::Literal(Literal::True));
    body = Stmt::WhileStmt {
      cond,
      body: Box::new(body),
    };

    if let Some(init) = init {
      body = Stmt::Block(vec![init, body]);
    }

    Ok(body)
  }

  fn while_statement(&mut self) -> Result<Stmt> {
    if !self.match1(Token::LeftParen) {
      Err(anyhow!("Expected ( in while-condition"))?
    }

    let cond = self.expression()?;

    if !self.match1(Token::RightParen) {
      Err(anyhow!("Expected ) after expression in while-condition"))?
    }

    let stmt = Box::new(self.statement()?);

    Ok(Stmt::WhileStmt { cond, body: stmt })
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

    Ok(Stmt::ExprStmt(Box::new(exp)))
  }

  fn expression(&mut self) -> Result<Expression> {
    return self.assignment();
  }

  fn assignment(&mut self) -> Result<Expression> {
    let exp = self.or()?;

    if self.match1(Token::Equal) {
      let value = self.assignment()?;

      if let Expression::Literal(Literal::Identifier(ident)) = exp {
        return Ok(Expression::Assignment {
          ident,
          value: Box::new(value),
        });
      } else {
        Err(anyhow!("Invalid assignment target"))?;
      }
    }

    return Ok(exp);
  }

  fn or(&mut self) -> Result<Expression> {
    let mut exp = self.and()?;

    while self.match1(Token::Or) {
      let op = self.previous();
      exp = Expression::Logic {
        and1: Box::new(exp),
        op: op.try_into()?,
        and2: Box::new(self.and()?),
      }
    }

    Ok(exp)
  }

  fn and(&mut self) -> Result<Expression> {
    let mut exp = self.equality()?;

    while self.match1(Token::And) {
      let op = self.previous();
      exp = Expression::Logic {
        and1: Box::new(exp),
        op: op.try_into()?,
        and2: Box::new(self.equality()?),
      }
    }

    Ok(exp)
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
    while self.matchn(vec![Token::Slash, Token::Star, Token::Mod]) {
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
      Token::Identifier(s) => Ok(Expression::Literal(Literal::Identifier(s))),
      tok => Err(anyhow!("invalid token: {:?}", tok)),
    }?;

    Ok(tok)
  }
}
