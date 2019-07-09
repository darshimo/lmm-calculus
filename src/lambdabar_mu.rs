use std::char;
use std::collections::VecDeque;
use std::fmt;

use LbMCommand::Command;
use LbMContext::{CStack, CVariable};
use LbMTerm::{LAbstraction, MAbstraction, TVariable};

pub enum LbMCommand {
  Command(Box<LbMTerm>, Box<LbMContext>),
}

pub enum LbMTerm {
  TVariable(String),
  LAbstraction(String, Box<LbMTerm>),
  MAbstraction(String, Box<LbMCommand>),
}

pub enum LbMContext {
  CVariable(String),
  CStack(Box<LbMTerm>, Box<LbMContext>),
}

impl LbMCommand {
  pub fn new(s: String) -> LbMCommand {
    let mut q = lexer(s, " ⟨|⟩⋅λμ.()");
    LbMCommand::parser(&mut q, false)
  }

  fn parser(q: &mut VecDeque<String>, in_abst: bool) -> LbMCommand {
    let langle = q.pop_front().expect("panic");
    if langle != "⟨" {
      panic!("syntax error");
    }
    let t = LbMTerm::parser(q, false);
    let vbar = q.pop_front().expect("panic");
    if vbar != "|" {
      panic!("syntax error");
    }
    let c = LbMContext::parser(q, false);
    let rangle = q.pop_front().expect("panic");
    if rangle != "⟩" {
      panic!("syntax error");
    }
    Command(Box::new(t), Box::new(c))
  }

  /*
  fn substitution(&self, x: &String, v: &LbMCommand) -> LbMCommand {
    match self {
      Variable(y) => {
        if x == y {
          v.clone()
        } else {
          self.clone()
        }
      }
      Abstraction(y, t) => {
        if x == y {
          self.clone()
        } else {
          let tmp = generate_variable();
          Abstraction(
            tmp.clone(),
            Box::new(
              t.substitution(&y, &Variable(tmp.clone()))
                .substitution(&x, &v),
            ),
          )
        }
      }
      Application(t1, t2) => Application(
        Box::new(t1.substitution(&x, &v)),
        Box::new(t2.substitution(&x, &v)),
      ),
    }
  }

  fn step(&self) -> Option<LbMCommand> {
    if let Application(t1, t2) = self {
      if t1.is_abstraction() {
        if t2.is_abstraction() {
          t1.application(t2)
        } else {
          match t2.step() {
            None => None,
            Some(t22) => Some(Application(t1.clone(), Box::new(t22.clone()))),
          }
        }
      } else {
        match t1.step() {
          None => None,
          Some(t11) => Some(Application(Box::new(t11.clone()), t2.clone())),
        }
      }
    } else {
      None
    }
  }

  pub fn reduction(&self) -> LbMCommand {
    let mut tmp = self.clone();
    loop {
      println!("{}", tmp);
      match tmp.step() {
        None => {
          break;
        }
        Some(s) => tmp = s.clone(),
      }
    }
    tmp
  }
  */
}

impl Clone for LbMCommand {
  fn clone(&self) -> LbMCommand {
    let Command(t, c) = self;
    Command(t.clone(), c.clone())
  }
}

impl fmt::Display for LbMCommand {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    let Command(t, c) = self;
    write!(f, "⟨{}|{}⟩", t, c)
  }
}

impl fmt::Debug for LbMCommand {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    let Command(t, c) = self;
    write!(f, "Command({:?}, {:?})", t, c)
  }
}

impl LbMContext {
  fn parser(q: &mut VecDeque<String>, in_abst: bool) -> LbMContext {
    let mut s = q.get(0).expect("panic");
    while s == " " || s == "(" || s == ")" {
      q.pop_front().expect("panic");
      s = q.get(0).expect("panic");
    }
    if s == "λ" || s == "μ" {
      let t = LbMTerm::parser(q, false);
      let dot = q.pop_front().expect("panic");
      if dot != "⋅" {
        panic!("is not dot");
      }
      let c = LbMContext::parser(q, false);
      CStack(Box::new(t), Box::new(c))
    } else if s == "." || s == "⟨" || s == "⟩" || s == "|" || s == "⋅" {
      panic!("syntax error");
    } else {
      let v = q.pop_front().expect("panic");
      let dot_or_rangle = q.get(0).expect("panic");
      if dot_or_rangle == "⋅" {
        q.pop_front().expect("panic");
        let c = LbMContext::parser(q, false);
        CStack(Box::new(TVariable(v)), Box::new(c))
      } else if dot_or_rangle == "⟩" {
        CVariable(v)
      } else {
        panic!("syntax error");
      }
    }
  }

  fn is_cvariable(&self) -> bool {
    match self {
      CVariable(_) => true,
      _ => false,
    }
  }

  /*
  fn substitution(&self, x: &String, v: &LbMContext) -> LbMContext {
    match self {
      Variable(y) => {
        if x == y {
          v.clone()
        } else {
          self.clone()
        }
      }
      Abstraction(y, t) => {
        if x == y {
          self.clone()
        } else {
          let tmp = generate_variable();
          Abstraction(
            tmp.clone(),
            Box::new(
              t.substitution(&y, &Variable(tmp.clone()))
                .substitution(&x, &v),
            ),
          )
        }
      }
      Application(t1, t2) => Application(
        Box::new(t1.substitution(&x, &v)),
        Box::new(t2.substitution(&x, &v)),
      ),
    }
  }

  fn step(&self) -> Option<LbMContext> {
    if let Application(t1, t2) = self {
      if t1.is_abstraction() {
        if t2.is_abstraction() {
          t1.application(t2)
        } else {
          match t2.step() {
            None => None,
            Some(t22) => Some(Application(t1.clone(), Box::new(t22.clone()))),
          }
        }
      } else {
        match t1.step() {
          None => None,
          Some(t11) => Some(Application(Box::new(t11.clone()), t2.clone())),
        }
      }
    } else {
      None
    }
  }

  pub fn reduction(&self) -> LbMContext {
    let mut tmp = self.clone();
    loop {
      println!("{}", tmp);
      match tmp.step() {
        None => {
          break;
        }
        Some(s) => tmp = s.clone(),
      }
    }
    tmp
  }
  */
}

impl Clone for LbMContext {
  fn clone(&self) -> LbMContext {
    match self {
      CVariable(x) => CVariable(x.clone()),
      CStack(t, e) => CStack(t.clone(), e.clone()),
    }
  }
}

impl fmt::Display for LbMContext {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match self {
      CVariable(x) => write!(f, "{}", x),
      CStack(t, e) => write!(f, "{}⋅{}", t, e),
    }
  }
}

impl fmt::Debug for LbMContext {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match self {
      CVariable(x) => write!(f, "CVariable({:?})", x),
      CStack(t, e) => write!(f, "CStack({:?}, {:?})", t, e),
    }
  }
}

impl LbMTerm {
  fn parser(q: &mut VecDeque<String>, in_abst: bool) -> LbMTerm {
    let mut s = q.pop_front().expect("panic");
    while s == " " || s == "(" || s == ")" {
      s = q.pop_front().expect("panic");
    }
    if s == "λ" {
      let v = q.pop_front().expect("panic");
      let dot = q.pop_front().expect("panic");
      if dot != "." {
        panic!("is not dot");
      }
      let t = LbMTerm::parser(q, true);
      LAbstraction(v, Box::new(t))
    } else if s == "μ" {
      let v = q.pop_front().expect("panic");
      let dot = q.pop_front().expect("panic");
      if dot != "." {
        panic!("is not dot");
      }
      let c = LbMCommand::parser(q, true);
      MAbstraction(v, Box::new(c))
    } else if s == "." || s == "⟨" || s == "⟩" || s == "|" || s == "⋅" {
      panic!("syntax error.");
    } else {
      TVariable(s)
    }
  }

  fn is_labstraction(&self) -> bool {
    match self {
      LAbstraction(_, _) => true,
      _ => false,
    }
  }

  fn is_mabstraction(&self) -> bool {
    match self {
      MAbstraction(_, _) => true,
      _ => false,
    }
  }

  /*
  fn substitution(&self, x: &String, v: &LbMTerm) -> LbMTerm {
    match self {
      Variable(y) => {
        if x == y {
          v.clone()
        } else {
          self.clone()
        }
      }
      Abstraction(y, t) => {
        if x == y {
          self.clone()
        } else {
          let tmp = generate_variable();
          Abstraction(
            tmp.clone(),
            Box::new(
              t.substitution(&y, &Variable(tmp.clone()))
                .substitution(&x, &v),
            ),
          )
        }
      }
      Application(t1, t2) => Application(
        Box::new(t1.substitution(&x, &v)),
        Box::new(t2.substitution(&x, &v)),
      ),
    }
  }

  fn application(&self, v2: &LbMTerm) -> Option<LbMTerm> {
    if let Abstraction(x, t12) = self {
      Some(t12.substitution(x, v2))
    } else {
      None
    }
  }

  fn step(&self) -> Option<LbMTerm> {
    if let Application(t1, t2) = self {
      if t1.is_abstraction() {
        if t2.is_abstraction() {
          t1.application(t2)
        } else {
          match t2.step() {
            None => None,
            Some(t22) => Some(Application(t1.clone(), Box::new(t22.clone()))),
          }
        }
      } else {
        match t1.step() {
          None => None,
          Some(t11) => Some(Application(Box::new(t11.clone()), t2.clone())),
        }
      }
    } else {
      None
    }
  }

  pub fn reduction(&self) -> LbMTerm {
    let mut tmp = self.clone();
    loop {
      println!("{}", tmp);
      match tmp.step() {
        None => {
          break;
        }
        Some(s) => tmp = s.clone(),
      }
    }
    tmp
  }
  */
}

impl Clone for LbMTerm {
  fn clone(&self) -> LbMTerm {
    match self {
      TVariable(x) => TVariable(x.clone()),
      LAbstraction(x, t) => LAbstraction(x.clone(), t.clone()),
      MAbstraction(x, c) => MAbstraction(x.clone(), c.clone()),
    }
  }
}

impl fmt::Display for LbMTerm {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match self {
      TVariable(x) => write!(f, "{}", x),
      LAbstraction(x, t) => write!(f, "λ{}.{}", x, t),
      MAbstraction(x, c) => write!(f, "μ{}.{}", x, c),
    }
  }
}

impl fmt::Debug for LbMTerm {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match self {
      TVariable(x) => write!(f, "TVariable({:?})", x),
      LAbstraction(x, t) => write!(f, "LAbstraction({:?}, {:?})", x, t),
      MAbstraction(x, c) => write!(f, "MAbstraction({:?}, {:?})", x, c),
    }
  }
}

static mut TCNT: u32 = 0;

fn generate_tvariable() -> String {
  let mut a: u32;
  unsafe {
    TCNT += 1;
    a = TCNT;
  }

  let mut s = String::new();

  while a > 0 {
    let b = a % 10;
    a /= 10;
    s = format!("{}{}", char::from_u32(8320 + b).unwrap(), s);
  }

  format!("x{}", s)
}

static mut CCNT: u32 = 0;

fn generate_cvariable() -> String {
  let mut a: u32;
  unsafe {
    CCNT += 1;
    a = CCNT;
  }

  let mut s = String::new();

  while a > 0 {
    let b = a % 10;
    a /= 10;
    s = format!("{}{}", char::from_u32(8320 + b).unwrap(), s);
  }

  format!("α{}", s)
}

fn lexer(s: String, sep: &str) -> VecDeque<String> {
  let alphabets: Vec<char> = s.chars().collect();
  let l = alphabets.len();
  let sep: Vec<char> = sep.chars().collect();

  let mut tmp: Vec<char> = vec![];

  let mut ret: VecDeque<String> = VecDeque::new();

  let c = alphabets[0];
  if sep.contains(&c) {
    ret.push_back(c.to_string());
  } else {
    tmp.push(c);
  }

  for i in 1..l {
    let c = alphabets[i];
    if sep.contains(&c) {
      if !tmp.is_empty() {
        ret.push_back(tmp.into_iter().collect());
        tmp = vec![];
      }
      if c != ' ' && c != '(' && c != ')' {
        ret.push_back(c.to_string());
      }
    } else {
      tmp.push(c);
    }
  }
  if !tmp.is_empty() {
    ret.push_back(tmp.into_iter().collect());
  }

  ret
}
