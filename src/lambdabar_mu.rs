use std::collections::VecDeque;
use std::fmt;

use LbMCommand::Command;
use LbMContext::{CStack, CVariable};
use LbMTerm::{LAbstraction, MAbstraction, TVariable};

use crate::generator::*;
use crate::lexer::*;

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

  fn substitution_term(&self, x: &String, v: &LbMTerm) -> LbMCommand {
    let Command(t, c) = self;
    Command(
      Box::new(t.substitution_term(x, v)),
      Box::new(c.substitution_term(x, v)),
    )
  }

  fn substitution_context(&self, beta: &String, e: &LbMContext) -> LbMCommand {
    let Command(t, c) = self;
    Command(
      Box::new(t.substitution_context(beta, e)),
      Box::new(c.substitution_context(beta, e)),
    )
  }

  fn step(&self) -> Option<LbMCommand> {
    let Command(v, e) = self;
    if v.is_labstraction() && e.is_cstack() {
      Some(v.to(e))
    } else if v.is_mabstraction() {
      Some(v.mu(e))
    } else {
      None
    }
  }

  pub fn reduction(&self) -> LbMCommand {
    let mut tmp = self.clone();
    println!("  {}", tmp);
    loop {
      match tmp.step() {
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

  fn is_cstack(&self) -> bool {
    match self {
      CStack(_, _) => true,
      _ => false,
    }
  }

  fn substitution_term(&self, x: &String, v: &LbMTerm) -> LbMContext {
    match self {
      CVariable(_) => self.clone(),
      CStack(t, e) => CStack(
        Box::new(t.substitution_term(x, v)),
        Box::new(e.substitution_term(x, v)),
      ),
    }
  }

  fn substitution_context(&self, beta: &String, e: &LbMContext) -> LbMContext {
    match self {
      CVariable(alpha) => {
        if alpha == beta {
          e.clone()
        } else {
          self.clone()
        }
      }
      CStack(v, t) => CStack(
        Box::new(v.substitution_context(beta, e)),
        Box::new(t.substitution_context(beta, e)),
      ),
    }
  }
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

  fn substitution_term(&self, x: &String, v: &LbMTerm) -> LbMTerm {
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

  fn substitution_context(&self, beta: &String, e: &LbMContext) -> LbMTerm {
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

  fn to(&self, e0: &LbMContext) -> LbMCommand {
    match self {
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

  fn mu(&self, e: &LbMContext) -> LbMCommand {
    match self {
      MAbstraction(beta, c) => c.substitution_context(beta, e),
      _ => {
        panic!("panic");
      }
    }
  }
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
