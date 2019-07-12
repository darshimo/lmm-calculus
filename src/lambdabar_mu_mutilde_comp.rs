use std::fmt;

use LbMMtCompCommand::Command;
use LbMMtCompContext::{CLAbstraction, CStack, CVariable, MtAbstraction};
use LbMMtCompTerm::{MAbstraction, TLAbstraction, TStack, TVariable};

use crate::lambdabar_mu_mutilde::*;
use crate::variable::*;

pub enum LbMMtCompCommand {
  Command(Box<LbMMtCompTerm>, Box<LbMMtCompContext>),
}

pub enum LbMMtCompTerm {
  TVariable(String),
  TLAbstraction(String, Box<LbMMtCompTerm>),
  MAbstraction(String, Box<LbMMtCompCommand>),
  TStack(Box<LbMMtCompContext>, Box<LbMMtCompTerm>),
}

pub enum LbMMtCompContext {
  CVariable(String),
  CLAbstraction(String, Box<LbMMtCompContext>),
  MtAbstraction(String, Box<LbMMtCompCommand>),
  CStack(Box<LbMMtCompTerm>, Box<LbMMtCompContext>),
}

impl LbMMtCompCommand {
  fn substitution_term(&self, x: &String, v: &LbMMtCompTerm) -> LbMMtCompCommand {
    let Command(t, c) = self;
    Command(
      Box::new(t.substitution_term(x, v)),
      Box::new(c.substitution_term(x, v)),
    )
  }

  fn substitution_context(&self, beta: &String, e: &LbMMtCompContext) -> LbMMtCompCommand {
    let Command(t, c) = self;
    Command(
      Box::new(t.substitution_context(beta, e)),
      Box::new(c.substitution_context(beta, e)),
    )
  }

  fn step(&self, cbn: bool) -> Option<LbMMtCompCommand> {
    let Command(v, e) = self;

    if v.is_tlabstraction() && e.is_cstack() {
      Some(LbMMtCompCommand::to_quote(v, e))
    } else if v.is_tstack() && e.is_clabstraction() {
      Some(LbMMtCompCommand::minus_quote(v, e))
    } else if cbn {
      if e.is_mtabstraction() {
        Some(LbMMtCompCommand::mu_tilde(v, e))
      } else if v.is_mabstraction() {
        Some(LbMMtCompCommand::mu(v, e))
      } else {
        None
      }
    } else {
      if v.is_mabstraction() {
        Some(LbMMtCompCommand::mu(v, e))
      } else if e.is_mtabstraction() {
        Some(LbMMtCompCommand::mu_tilde(v, e))
      } else {
        None
      }
    }
  }

  pub fn reduction(&self, cbn: bool) -> LbMMtCompCommand {
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

  fn to_quote(v0: &LbMMtCompTerm, e0: &LbMMtCompContext) -> LbMMtCompCommand {
    match v0 {
      TLAbstraction(x, v1) => match e0 {
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

  fn minus_quote(v0: &LbMMtCompTerm, e0: &LbMMtCompContext) -> LbMMtCompCommand {
    match e0 {
      CLAbstraction(beta, e1) => match v0 {
        TStack(e2, v) => {
          let tmp = generate_cvariable();
          Command(
            Box::new(MAbstraction(
              tmp.clone(),
              Box::new(Command(
                v.clone(),
                Box::new(e1.substitution_context(beta, &CVariable(tmp))),
              )),
            )),
            e2.clone(),
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

  fn mu(v0: &LbMMtCompTerm, e0: &LbMMtCompContext) -> LbMMtCompCommand {
    match v0 {
      MAbstraction(beta, c) => c.substitution_context(beta, e0),
      _ => {
        panic!("panic");
      }
    }
  }

  fn mu_tilde(v0: &LbMMtCompTerm, e0: &LbMMtCompContext) -> LbMMtCompCommand {
    match e0 {
      MtAbstraction(x, c) => c.substitution_term(x, v0),
      _ => {
        panic!("panic");
      }
    }
  }

  pub fn from(c: &LbMMtCommand) -> LbMMtCompCommand {
    let LbMMtCommand::Command(v, e) = c;
    Command(
      Box::new(LbMMtCompTerm::from(v)),
      Box::new(LbMMtCompContext::from(e)),
    )
  }

  pub fn reverse(&self) -> LbMMtCompCommand {
    let mut bv: Vec<String> = vec![];
    self.reverse_sub(&mut bv)
  }

  fn reverse_sub(&self, bv: &mut Vec<String>) -> LbMMtCompCommand {
    let Command(v, e) = self;
    Command(Box::new(e.reverse_sub(bv)), Box::new(v.reverse_sub(bv)))
  }
}

impl Clone for LbMMtCompCommand {
  fn clone(&self) -> LbMMtCompCommand {
    let Command(t, c) = self;
    Command(t.clone(), c.clone())
  }
}

impl fmt::Display for LbMMtCompCommand {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    let Command(t, c) = self;
    write!(f, "⟨{}|{}⟩", t, c)
  }
}

impl fmt::Debug for LbMMtCompCommand {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    let Command(t, c) = self;
    write!(f, "Command({:?}, {:?})", t, c)
  }
}

impl LbMMtCompContext {
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

  fn is_clabstraction(&self) -> bool {
    match self {
      CLAbstraction(_, _) => true,
      _ => false,
    }
  }

  fn substitution_term(&self, x: &String, v: &LbMMtCompTerm) -> LbMMtCompContext {
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
      CLAbstraction(beta, e) => CLAbstraction(beta.clone(), Box::new(e.substitution_term(x, v))),
    }
  }

  fn substitution_context(&self, beta: &String, e: &LbMMtCompContext) -> LbMMtCompContext {
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
      CLAbstraction(alpha, e1) => {
        if beta == alpha {
          self.clone()
        } else {
          let tmp = generate_cvariable();
          CLAbstraction(
            tmp.clone(),
            Box::new(
              e1.substitution_context(alpha, &CVariable(tmp.clone()))
                .substitution_context(beta, e),
            ),
          )
        }
      }
    }
  }

  pub fn from(e: &LbMMtContext) -> LbMMtCompContext {
    match e {
      LbMMtContext::CVariable(alpha) => CVariable(alpha.clone()),
      LbMMtContext::MtAbstraction(x, c) => {
        let y = generate_tvariable();
        MtAbstraction(
          y.clone(),
          Box::new(LbMMtCompCommand::from(c).substitution_term(x, &TVariable(y))),
        )
      }
      LbMMtContext::CStack(v, e1) => CStack(
        Box::new(LbMMtCompTerm::from(v)),
        Box::new(LbMMtCompContext::from(e1)),
      ),
    }
  }

  pub fn reverse_sub(&self, bv: &mut Vec<String>) -> LbMMtCompTerm {
    match self {
      CVariable(alpha) => {
        if bv.contains(alpha) {
          TVariable(reverse_bound_variable(alpha))
        } else {
          TVariable(reverse_free_variable(alpha))
        }
      }
      MtAbstraction(x, c) => {
        bv.push(x.clone());
        let ret = MAbstraction(reverse_bound_variable(x), Box::new(c.reverse_sub(bv)));
        bv.pop();
        ret
      }
      CStack(v, e) => TStack(Box::new(v.reverse_sub(bv)), Box::new(e.reverse_sub(bv))),
      CLAbstraction(beta, e) => {
        bv.push(beta.clone());
        let ret = TLAbstraction(beta.clone(), Box::new(e.reverse_sub(bv)));
        bv.pop();
        ret
      }
    }
  }
}

impl Clone for LbMMtCompContext {
  fn clone(&self) -> LbMMtCompContext {
    match self {
      CVariable(alpha) => CVariable(alpha.clone()),
      MtAbstraction(x, c) => MtAbstraction(x.clone(), c.clone()),
      CStack(t, e) => CStack(t.clone(), e.clone()),
      CLAbstraction(beta, e) => CLAbstraction(beta.clone(), e.clone()),
    }
  }
}

impl fmt::Display for LbMMtCompContext {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match self {
      CVariable(x) => write!(f, "{}", x),
      MtAbstraction(x, c) => write!(f, "μ̃{}.{}", x, c),
      CStack(t, e) => write!(f, "{}⋅{}", t, e),
      CLAbstraction(beta, e) => write!(f, "{}λ.{}", beta, e),
    }
  }
}

impl fmt::Debug for LbMMtCompContext {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match self {
      CVariable(x) => write!(f, "CVariable({:?})", x),
      MtAbstraction(x, c) => write!(f, "MtAbstraction({:?}, {:?})", x, c),
      CStack(t, e) => write!(f, "CStack({:?}, {:?})", t, e),
      CLAbstraction(beta, e) => write!(f, "CLAbstraction({:?}, {:?})", beta, e),
    }
  }
}

impl LbMMtCompTerm {
  fn is_tlabstraction(&self) -> bool {
    match self {
      TLAbstraction(_, _) => true,
      _ => false,
    }
  }

  fn is_mabstraction(&self) -> bool {
    match self {
      MAbstraction(_, _) => true,
      _ => false,
    }
  }

  fn is_tstack(&self) -> bool {
    match self {
      TStack(_, _) => true,
      _ => false,
    }
  }

  fn substitution_term(&self, x: &String, v: &LbMMtCompTerm) -> LbMMtCompTerm {
    match self {
      TVariable(y) => {
        if x == y {
          v.clone()
        } else {
          self.clone()
        }
      }
      TLAbstraction(y, t) => {
        if x == y {
          self.clone()
        } else {
          let tmp = generate_tvariable();
          TLAbstraction(
            tmp.clone(),
            Box::new(
              t.substitution_term(y, &TVariable(tmp.clone()))
                .substitution_term(x, v),
            ),
          )
        }
      }
      MAbstraction(beta, c) => MAbstraction(beta.clone(), Box::new(c.substitution_term(x, v))),
      TStack(e, v1) => TStack(
        Box::new(e.substitution_term(x, v)),
        Box::new(v1.substitution_term(x, v)),
      ),
    }
  }

  fn substitution_context(&self, beta: &String, e: &LbMMtCompContext) -> LbMMtCompTerm {
    match self {
      TVariable(_) => self.clone(),
      TLAbstraction(x, t) => TLAbstraction(x.clone(), Box::new(t.substitution_context(beta, e))),
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
      TStack(e1, v) => TStack(
        Box::new(e1.substitution_context(beta, e)),
        Box::new(v.substitution_context(beta, e)),
      ),
    }
  }

  pub fn from(v: &LbMMtTerm) -> LbMMtCompTerm {
    match v {
      LbMMtTerm::TVariable(x) => TVariable(x.clone()),
      LbMMtTerm::LAbstraction(x, v1) => {
        let y = generate_tvariable();
        TLAbstraction(
          y.clone(),
          Box::new(LbMMtCompTerm::from(v1).substitution_term(x, &TVariable(y))),
        )
      }
      LbMMtTerm::MAbstraction(beta, c) => {
        let alpha = generate_cvariable();
        MAbstraction(
          alpha.clone(),
          Box::new(LbMMtCompCommand::from(c).substitution_context(beta, &CVariable(alpha))),
        )
      }
    }
  }

  pub fn reverse_sub(&self, bv: &mut Vec<String>) -> LbMMtCompContext {
    match self {
      TVariable(x) => {
        if bv.contains(x) {
          CVariable(reverse_bound_variable(x))
        } else {
          CVariable(reverse_free_variable(x))
        }
      }
      TLAbstraction(x, v) => {
        bv.push(x.clone());
        let ret = CLAbstraction(reverse_bound_variable(x), Box::new(v.reverse_sub(bv)));
        bv.pop();
        ret
      }
      MAbstraction(beta, c) => {
        bv.push(beta.clone());
        let ret = MtAbstraction(reverse_bound_variable(beta), Box::new(c.reverse_sub(bv)));
        bv.pop();
        ret
      }
      TStack(e, v) => CStack(Box::new(e.reverse_sub(bv)), Box::new(v.reverse_sub(bv))),
    }
  }
}

impl Clone for LbMMtCompTerm {
  fn clone(&self) -> LbMMtCompTerm {
    match self {
      TVariable(x) => TVariable(x.clone()),
      TLAbstraction(x, t) => TLAbstraction(x.clone(), t.clone()),
      MAbstraction(x, c) => MAbstraction(x.clone(), c.clone()),
      TStack(e, v) => TStack(e.clone(), v.clone()),
    }
  }
}

impl fmt::Display for LbMMtCompTerm {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match self {
      TVariable(x) => write!(f, "{}", x),
      TLAbstraction(x, t) => write!(f, "λ{}.{}", x, t),
      MAbstraction(x, c) => write!(f, "μ{}.{}", x, c),
      TStack(e, v) => write!(f, "{}⋅{}", e, v),
    }
  }
}

impl fmt::Debug for LbMMtCompTerm {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match self {
      TVariable(x) => write!(f, "TVariable({:?})", x),
      TLAbstraction(x, t) => write!(f, "TLAbstraction({:?}, {:?})", x, t),
      MAbstraction(x, c) => write!(f, "MAbstraction({:?}, {:?})", x, c),
      TStack(e, v) => write!(f, "TStack({:?}, {:?})", e, v),
    }
  }
}
