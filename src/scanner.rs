use std::{collections::HashMap, str::Chars};

use crate::error::Result;
use anyhow::{anyhow, Context};
use phf::phf_map;

const KEYWORDS: phf::Map<&'static str, Token> = phf_map! {
  "and" =>     Token::And,
  "class" =>   Token::Class,
  "else" =>    Token::Else,
  "false" =>   Token::False,
  "for" =>     Token::Fun,
  "fun" =>     Token::For,
  "if" =>      Token::If,
  "nil" =>     Token::Nil,
  "or" =>      Token::Or,
  "print" =>   Token::Print,
  "return" =>  Token::Return,
  "super" =>   Token::Super,
  "this" =>    Token::This,
  "true" =>    Token::True,
  "var" =>     Token::Var,
  "while" =>   Token::While,
};

#[allow(dead_code)]
#[derive(Debug, Clone)]
pub enum Token {
  // Single-character tokens.
  LeftParen,
  RightParen,
  LeftBrace,
  RightBrace,
  Comma,
  Dot,
  Minus,
  Plus,
  Semicolon,
  Slash,
  Star,

  // One or two character tokens.
  Bang,
  BangEqual,
  Equal,
  EqualEqual,
  Greater,
  GreaterEqual,
  Less,
  LessEqual,

  // Literals.
  Identifier(String),
  String(String),
  Number(f64),

  // Keywords.
  And,
  Class,
  Else,
  False,
  Fun,
  For,
  If,
  Nil,
  Or,
  Print,
  Return,
  Super,
  This,
  True,
  Var,
  While,

  Eof,
}

#[derive(Debug)]
pub struct Scanner<'a> {
  source: String,
  chars: Chars<'a>,
  current: usize,
  line: usize,
  start: usize,
}

impl<'a> Scanner<'a> {
  pub fn new(source: &'a str) -> Self {
    Scanner {
      source: source.to_string(),
      chars: source.chars(),
      line: 1,
      start: 0,
      current: 0,
    }
  }

  pub fn scan_tokens(&mut self) -> std::result::Result<Vec<Token>, Vec<anyhow::Error>> {
    let mut tokens: Vec<Token> = vec![];
    let mut errs: Vec<anyhow::Error> = vec![];

    dbg!(self.chars.clone().collect::<String>());

    while !self.is_at_end() {
      self.start = self.current;
      let token_attempt = self.scan_token();

      match token_attempt {
        Ok(Some(token)) => tokens.push(token),
        Ok(None) => (),
        Err(err) => errs.push(err),
      }
    }

    if !errs.is_empty() {
      return Err(errs);
    }

    tokens.push(Token::Eof);
    Ok(tokens)
  }

  fn is_at_end(&self) -> bool {
    self.chars.clone().as_str().len() == 0
  }

  fn advance(&mut self) -> Option<char> {
    let c = self.chars.next();
    self.current += 1;
    c
  }

  fn peek(&self) -> char {
    self.chars.clone().next().unwrap_or('0')
  }
  fn peek2(&self) -> char {
    let mut chars = self.chars.clone();
    chars.next();
    chars.next().unwrap_or('0')
  }

  fn match_next(&mut self, c: char) -> bool {
    if c == self.peek() {
      self.advance();
      return true;
    }
    false
  }

  fn string(&mut self) -> Result<Token> {
    while self.peek() != '"' && !self.is_at_end() {
      self.advance();
    }

    if self.is_at_end() {
      Err(anyhow!("unterminated string at {}", self.start))?;
    }

    // Consume ending "
    let c = self.advance();
    assert_eq!(c, Some('"'));

    Ok(Token::String(self.sub_string(self.start + 1, self.current - 1)))
  }

  fn is_digit(&self, c: char) -> bool {
    c >= '0' && c <= '9'
  }

  fn sub_string(&self, start: usize, end: usize) -> String {
    self.source.chars().skip(start).take(end - start).collect()
  }

  fn number(&mut self) -> Result<Token> {
    while self.is_digit(self.peek()) {
      self.advance();
    }

    if self.peek() == '.' && self.is_digit(self.peek2()) {
      self.advance();

      while self.is_digit(self.peek()) {
        self.advance();
      }
    }

    let num = self
      .sub_string(self.start, self.current)
      .parse::<f64>()
      .context(format!("failed to parse number at {}", self.start))?;

    Ok(Token::Number(num))
  }

  fn identifier(&mut self) -> Result<Token> {
    while self.peek().is_ascii_alphanumeric() {
      self.advance();
    }

    let ident = self.sub_string(self.start, self.current);

    if let Some(token) = KEYWORDS.get(&ident) {
      Ok(token.clone())
    } else {
      Ok(Token::Identifier(ident))
    }
  }

  fn scan_token(&mut self) -> Result<Option<Token>> {
    let c = self.advance().context("avanced after reaching the end")?;
    dbg!("Scanning token", c);
    let token = match c {
      '(' => Some(Token::LeftParen),
      ')' => Some(Token::RightParen),
      '{' => Some(Token::LeftBrace),
      '}' => Some(Token::RightBrace),
      ',' => Some(Token::Comma),
      '.' => Some(Token::Dot),
      '-' => Some(Token::Minus),
      '+' => Some(Token::Plus),
      ';' => Some(Token::Semicolon),
      '*' => Some(Token::Star),
      '!' => {
        if self.match_next('=') {
          Some(Token::BangEqual)
        } else {
          Some(Token::Bang)
        }
      },
      '=' => {
        if self.match_next('=') {
          Some(Token::EqualEqual)
        } else {
          Some(Token::Equal)
        }
      },
      '<' => {
        if self.match_next('=') {
          Some(Token::LessEqual)
        } else {
          Some(Token::Less)
        }
      },
      '>' => {
        if self.match_next('=') {
          Some(Token::GreaterEqual)
        } else {
          Some(Token::Greater)
        }
      },
      '/' => {
        if self.match_next('/') {
          while self.peek() != '\n' && !self.is_at_end() {
            self.advance();
          }
          None
        } else {
          Some(Token::Slash)
        }
      },
      ' ' => None,
      '\r' => None,
      '\t' => None,
      '\n' => None,
      '"' => Some(self.string()?),
      c if self.is_digit(c) => Some(self.number()?),
      c if c.is_ascii_alphanumeric() => Some(self.identifier()?),
      _ => Err(anyhow!("Unexpected character {} in pos {}", c, self.current))?,
    };

    Ok(token)
  }
}
