mod error;
mod parser;
mod scanner;

use crate::{error::Result, scanner::Scanner};

use std::{
  env, fs,
  io::{stdin, stdout, Write},
};

fn run(source: String) -> Result<()> {
  let mut scanner = Scanner::new(source.as_str());
  let tokens = scanner.scan_tokens();
  dbg!(tokens);
  Ok(())
}

fn run_prompt() -> Result<()> {
  let sin = stdin();
  let mut sout = stdout();
  loop {
    sout.write_all(b"> ")?;
    sout.flush()?;

    let mut line = String::new();
    sin.read_line(&mut line)?;

    if line.is_empty() {
      println!("exiting");
      break;
    }
    let res = run(line);
    match res {
      Ok(_) => (),
      Err(err) => {
        dbg!(err);
        ()
      },
    };
  }
  Ok(())
}

fn run_file(file: String) -> Result<()> {
  let source = fs::read_to_string(file)?;
  run(source)?;
  Ok(())
}

fn main() {
  let args: Vec<String> = env::args().collect();

  if args.len() == 1 {
    run_prompt().unwrap();
  } else if args.len() == 2 {
    run_file(args[1].to_string()).unwrap();
  } else {
    println!("Usage: rlox <script>")
  }
}
