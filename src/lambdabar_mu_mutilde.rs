use std::char;
use std::collections::VecDeque;
use std::fmt;

use LbMMtCommand::Command;
use LbMMtContext::{CStack, CVariable, MtAbstraction};
use LbMMtTerm::{LAbstraction, MAbstraction, TVariable};

pub enum LbMMtCommand {
  Command(Box<LbMMtTerm>, Box<LbMMtContext>),
}

pub enum LbMMtTerm {
  TVariable(String),
  LAbstraction(String, Box<LbMMtTerm>),
  MAbstraction(String, Box<LbMMtCommand>),
}

pub enum LbMMtContext {
  CVariable(String),
  MtAbstraction(String, Box<LbMMtCommand>),
  CStack(Box<LbMMtTerm>, Box<LbMMtContext>),
}

impl LbMMtCommand {
  pub fn new(s: String) -> LbMMtCommand {
    let mut q = lexer(s, " ⟨|⟩⋅λμ.()");
    LbMMtCommand::parser(&mut q)
  }

  fn parser(q: &mut VecDeque<String>) -> LbMMtCommand {
    let langle = q.pop_front().expect("panic");
    if langle != "⟨" {
      panic!("syntax error");
    }
    let t = LbMMtTerm::parser(q);
    let vbar = q.pop_front().expect("panic");
    if vbar != "|" {
      panic!("syntax error");
    }
    let c = LbMMtContext::parser(q);
    let rangle = q.pop_front().expect("panic");
    if rangle != "⟩" {
      panic!("syntax error");
    }
    Command(Box::new(t), Box::new(c))
  }

  fn substitution_term(&self, x: &String, v: &LbMMtTerm) -> LbMMtCommand {
    let Command(t, c) = self;
    Command(
      Box::new(t.substitution_term(x, v)),
      Box::new(c.substitution_term(x, v)),
    )
  }

  fn substitution_context(&self, beta: &String, e: &LbMMtContext) -> LbMMtCommand {
    let Command(t, c) = self;
    Command(
      Box::new(t.substitution_context(beta, e)),
      Box::new(c.substitution_context(beta, e)),
    )
  }

  fn step(&self, cbn: bool) -> Option<LbMMtCommand> {
    let Command(v, e) = self;

    if v.is_labstraction() && e.is_cstack() {
      Some(LbMMtCommand::to_quote(v, e))
    } else if cbn {
      if e.is_mtabstraction() {
        Some(LbMMtCommand::mu_tilde(v, e))
      } else if v.is_mabstraction() {
        Some(LbMMtCommand::mu(v, e))
      } else {
        None
      }
    } else {
      if v.is_mabstraction() {
        Some(LbMMtCommand::mu(v, e))
      } else if e.is_mtabstraction() {
        Some(LbMMtCommand::mu_tilde(v, e))
      } else {
        None
      }
    }
  }

  pub fn reduction(&self, cbn: bool) -> LbMMtCommand {
    let mut tmp = self.clone();
    println!("  {}", tmp);
    loop {
      match tmp.step(cbn) {
        None => {
          break;
        }
        Some(s) => {
          tmp = s.clone();
          println!("→ {}", tmp);
        }
      }
    }
    tmp
  }

  fn to(v0: &LbMMtTerm, e0: &LbMMtContext) -> LbMMtCommand {
    match v0 {
      LAbstraction(x, v1) => match e0 {
        CStack(v2, e) => Command(Box::new(v1.substitution_term(x, v2)), e.clone()),
        _ => {
          panic!("panic");
        }
      },
      _ => {
        panic!("panic");
      }
    }
  }

  fn to_quote(v0: &LbMMtTerm, e0: &LbMMtContext) -> LbMMtCommand {
    match v0 {
      LAbstraction(x, v1) => match e0 {
        CStack(v2, e) => {
          let tmp = generate_tvariable();
          Command(
            v2.clone(),
            Box::new(MtAbstraction(
              tmp.clone(),
              Box::new(Command(
                Box::new(v1.substitution_term(x, &TVariable(tmp))),
                e.clone(),
              )),
            )),
          )
        }
        _ => {
          panic!("panic");
        }
      },
      _ => {
        panic!("panic");
      }
    }
  }

  fn mu(v0: &LbMMtTerm, e0: &LbMMtContext) -> LbMMtCommand {
    match v0 {
      MAbstraction(beta, c) => c.substitution_context(beta, e0),
      _ => {
        panic!("panic");
      }
    }
  }

  fn mu_tilde(v0: &LbMMtTerm, e0: &LbMMtContext) -> LbMMtCommand {
    match e0 {
      MtAbstraction(x, c) => c.substitution_term(x, v0),
      _ => {
        panic!("panic");
      }
    }
  }
}

impl Clone for LbMMtCommand {
  fn clone(&self) -> LbMMtCommand {
    let Command(t, c) = self;
    Command(t.clone(), c.clone())
  }
}

impl fmt::Display for LbMMtCommand {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    let Command(t, c) = self;
    write!(f, "⟨{}|{}⟩", t, c)
  }
}

impl fmt::Debug for LbMMtCommand {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    let Command(t, c) = self;
    write!(f, "Command({:?}, {:?})", t, c)
  }
}

impl LbMMtContext {
  fn parser(q: &mut VecDeque<String>) -> LbMMtContext {
    let mut s = q.get(0).expect("panic");
    while s == " " || s == "(" || s == ")" {
      q.pop_front().expect("panic");
      s = q.get(0).expect("panic");
    }
    if s == "μ̃" {
      q.pop_front().expect("panic");
      let v = q.pop_front().expect("panic");
      let dot = q.pop_front().expect("panic");
      if dot != "." {
        panic!("is not dot");
      }
      let c = LbMMtCommand::parser(q);
      MtAbstraction(v, Box::new(c))
    } else if s == "λ" || s == "μ" {
      let t = LbMMtTerm::parser(q);
      let dot = q.pop_front().expect("panic");
      if dot != "⋅" {
        panic!("is not dot");
      }
      let c = LbMMtContext::parser(q);
      CStack(Box::new(t), Box::new(c))
    } else if s == "." || s == "⟨" || s == "⟩" || s == "|" || s == "⋅" {
      panic!("syntax error");
    } else {
      let v = q.pop_front().expect("panic");
      let dot_or_rangle = q.get(0).expect("panic");
      if dot_or_rangle == "⋅" {
        q.pop_front().expect("panic");
        let c = LbMMtContext::parser(q);
        CStack(Box::new(TVariable(v)), Box::new(c))
      } else if dot_or_rangle == "⟩" {
        CVariable(v)
      } else {
        panic!("syntax error");
      }
    }
  }

  fn is_cstack(&self) -> bool {
    match self {
      CStack(_, _) => true,
      _ => false,
    }
  }

  fn is_mtabstraction(&self) -> bool {
    match self {
      MtAbstraction(_, _) => true,
      _ => false,
    }
  }

  fn substitution_term(&self, x: &String, v: &LbMMtTerm) -> LbMMtContext {
    match self {
      CVariable(_) => self.clone(),
      MtAbstraction(y, c) => {
        if x == y {
          self.clone()
        } else {
          let tmp = generate_tvariable();
          MtAbstraction(
            tmp.clone(),
            Box::new(
              c.substitution_term(y, &TVariable(tmp.clone()))
                .substitution_term(x, v),
            ),
          )
        }
      }
      CStack(t, e) => CStack(
        Box::new(t.substitution_term(x, v)),
        Box::new(e.substitution_term(x, v)),
      ),
    }
  }

  fn substitution_context(&self, beta: &String, e: &LbMMtContext) -> LbMMtContext {
    match self {
      CVariable(alpha) => {
        if alpha == beta {
          e.clone()
        } else {
          self.clone()
        }
      }
      MtAbstraction(x, c) => MtAbstraction(x.clone(), Box::new(c.substitution_context(beta, e))),
      CStack(v, t) => CStack(
        Box::new(v.substitution_context(beta, e)),
        Box::new(t.substitution_context(beta, e)),
      ),
    }
  }
}

impl Clone for LbMMtContext {
  fn clone(&self) -> LbMMtContext {
    match self {
      CVariable(alpha) => CVariable(alpha.clone()),
      MtAbstraction(x, c) => MtAbstraction(x.clone(), c.clone()),
      CStack(t, e) => CStack(t.clone(), e.clone()),
    }
  }
}

impl fmt::Display for LbMMtContext {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match self {
      CVariable(x) => write!(f, "{}", x),
      MtAbstraction(x, c) => write!(f, "μ̃{}.{}", x, c),
      CStack(t, e) => write!(f, "{}⋅{}", t, e),
    }
  }
}

impl fmt::Debug for LbMMtContext {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match self {
      CVariable(x) => write!(f, "CVariable({:?})", x),
      MtAbstraction(x, c) => write!(f, "MtAbstraction({:?}, {:?})", x, c),
      CStack(t, e) => write!(f, "CStack({:?}, {:?})", t, e),
    }
  }
}

impl LbMMtTerm {
  fn parser(q: &mut VecDeque<String>) -> LbMMtTerm {
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
      let t = LbMMtTerm::parser(q);
      LAbstraction(v, Box::new(t))
    } else if s == "μ" {
      let v = q.pop_front().expect("panic");
      let dot = q.pop_front().expect("panic");
      if dot != "." {
        panic!("is not dot");
      }
      let c = LbMMtCommand::parser(q);
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

  fn substitution_term(&self, x: &String, v: &LbMMtTerm) -> LbMMtTerm {
    match self {
      TVariable(y) => {
        if x == y {
          v.clone()
        } else {
          self.clone()
        }
      }
      LAbstraction(y, t) => {
        if x == y {
          self.clone()
        } else {
          let tmp = generate_tvariable();
          LAbstraction(
            tmp.clone(),
            Box::new(
              t.substitution_term(y, &TVariable(tmp.clone()))
                .substitution_term(x, v),
            ),
          )
        }
      }
      MAbstraction(beta, c) => MAbstraction(beta.clone(), Box::new(c.substitution_term(x, v))),
    }
  }

  fn substitution_context(&self, beta: &String, e: &LbMMtContext) -> LbMMtTerm {
    match self {
      TVariable(_) => self.clone(),
      LAbstraction(x, t) => LAbstraction(x.clone(), Box::new(t.substitution_context(beta, e))),
      MAbstraction(alpha, c) => {
        if alpha == beta {
          self.clone()
        } else {
          let tmp = generate_cvariable();
          MAbstraction(
            tmp.clone(),
            Box::new(
              c.substitution_context(alpha, &CVariable(tmp.clone()))
                .substitution_context(beta, e),
            ),
          )
        }
      }
    }
  }
}

impl Clone for LbMMtTerm {
  fn clone(&self) -> LbMMtTerm {
    match self {
      TVariable(x) => TVariable(x.clone()),
      LAbstraction(x, t) => LAbstraction(x.clone(), t.clone()),
      MAbstraction(x, c) => MAbstraction(x.clone(), c.clone()),
    }
  }
}

impl fmt::Display for LbMMtTerm {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match self {
      TVariable(x) => write!(f, "{}", x),
      LAbstraction(x, t) => write!(f, "λ{}.{}", x, t),
      MAbstraction(x, c) => write!(f, "μ{}.{}", x, c),
    }
  }
}

impl fmt::Debug for LbMMtTerm {
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
    } else if c == '̃' {
      let mut last = ret.pop_back().expect("panic");
      last.push(c);
      ret.push_back(last);
    } else {
      tmp.push(c);
    }
  }
  if !tmp.is_empty() {
    ret.push_back(tmp.into_iter().collect());
  }

  ret
}
