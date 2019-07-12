use std::collections::VecDeque;
use std::fmt;

use LDTerm::{Abstraction, Application, DAbstraction, Pair, Variable};
use RedType::{CBN, CBVR};

use crate::lambdabar_mu_mutilde_comp::*;
use crate::lexer::*;
use crate::variable::*;

pub enum LDTerm {
  Variable(String),
  Abstraction(String, Box<LDTerm>),
  Application(Box<LDTerm>, Box<LDTerm>),
  DAbstraction(String, String, Box<LDTerm>),
  Pair(Box<LDTerm>, Box<LDTerm>),
}

pub enum RedType {
  CBN,
  CBVL,
  CBVR,
}

impl LDTerm {
  pub fn new(s: String) -> LDTerm {
    let mut q = lexer(s, " λ,.()");
    LDTerm::parser(&mut q, false)
  }

  fn unbox(&self) -> &LDTerm {
    self
  }

  fn parser(q: &mut VecDeque<String>, in_parentheses: bool) -> LDTerm {
    let mut tmp: Option<LDTerm> = None;
    while let Some(s) = q.pop_front() {
      if s == " " {
      } else if s == "(" {
        let tmp2 = LDTerm::parser(q, true);
        match tmp {
          None => tmp = Some(tmp2),
          Some(t) => tmp = Some(Application(Box::new(t), Box::new(tmp2))),
        }
      } else if s == ")" {
        if !in_parentheses {
          q.push_front(String::from(")"));
        }
        return tmp.expect("panic");
      } else if s == "," {
        if !in_parentheses {
          q.push_front(String::from(","));
          return tmp.expect("panic");
        }
        let tmp2 = LDTerm::parser(q, true);
        return Pair(Box::new(tmp.expect("panic")), Box::new(tmp2));
      } else if s == "λ" {
        let s1 = q.pop_front().expect("panic");
        if s1 == "(" {
          let x = q.pop_front().expect("panic");
          let comma = q.pop_front().expect("panic");
          if comma != "," {
            panic!("syntax error");
          }
          let beta = q.pop_front().expect("panic");
          let s2 = q.pop_front().expect("panic");
          let s3 = q.pop_front().expect("panic");
          if s2 != ")" || s3 != "." {
            panic!("syntax error");
          }
          let t = LDTerm::parser(q, false);
          let tmp2 = DAbstraction(x, beta, Box::new(t));
          match tmp {
            None => tmp = Some(tmp2),
            Some(t) => tmp = Some(Application(Box::new(t), Box::new(tmp2))),
          }
        } else {
          let v = s1;
          let dot = q.pop_front().expect("panic");
          if dot != "." {
            panic!("is not dot");
          }
          let t = LDTerm::parser(q, false);
          let tmp2 = Abstraction(v, Box::new(t));
          match tmp {
            None => tmp = Some(tmp2),
            Some(t) => tmp = Some(Application(Box::new(t), Box::new(tmp2))),
          }
        }
      } else if s == "." {
        panic!("invalid dot.");
      } else {
        let tmp2 = Variable(s);
        match tmp {
          None => tmp = Some(tmp2),
          Some(t) => tmp = Some(Application(Box::new(t), Box::new(tmp2))),
        }
      }
    }
    tmp.expect("panic")
  }

  fn is_variable(&self) -> bool {
    match self {
      Variable(_) => true,
      _ => false,
    }
  }

  fn is_abstraction(&self) -> bool {
    match self {
      Abstraction(_, _) => true,
      _ => false,
    }
  }

  fn is_dabstraction(&self) -> bool {
    match self {
      DAbstraction(_, _, _) => true,
      _ => false,
    }
  }

  fn is_application(&self) -> bool {
    match self {
      Application(_, _) => true,
      _ => false,
    }
  }

  fn is_pair(&self) -> bool {
    match self {
      Pair(_, _) => true,
      _ => false,
    }
  }

  fn substitution(&self, x: &String, v: &LDTerm) -> LDTerm {
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
          let tmp: String;
          if y.starts_with("x") {
            tmp = generate_tvariable();
          } else if y.starts_with("k") {
            tmp = generate_kvariable();
          } else {
            tmp = generate_cvariable();
          }
          Abstraction(
            tmp.clone(),
            Box::new(t.substitution(y, &Variable(tmp.clone())).substitution(x, v)),
          )
        }
      }
      Application(t1, t2) => Application(
        Box::new(t1.substitution(x, v)),
        Box::new(t2.substitution(x, v)),
      ),
      DAbstraction(y, beta, t) => {
        if x == y || x == beta {
          self.clone()
        } else {
          let ttmp = generate_tvariable();
          let ctmp = generate_cvariable();
          DAbstraction(
            ttmp.clone(),
            ctmp.clone(),
            Box::new(
              t.substitution(y, &Variable(ttmp.clone()))
                .substitution(beta, &Variable(ctmp.clone()))
                .substitution(x, v),
            ),
          )
        }
      }
      Pair(t1, t2) => Pair(
        Box::new(t1.substitution(x, v)),
        Box::new(t2.substitution(x, v)),
      ),
    }
  }

  fn step(&self, rt: &RedType) -> Option<LDTerm> {
    if let Application(t1, t2) = self {
      if let Abstraction(x, t12) = t1.unbox() {
        match rt {
          CBN => Some(t12.substitution(x, t2)),
          _ => match t2.step(rt) {
            Some(v2) => Some(Application(t1.clone(), Box::new(v2))),
            None => {
              if !t2.is_application() {
                Some(t12.substitution(x, t2))
              } else {
                None
              }
            }
          },
        }
      } else if let DAbstraction(x, beta, t12) = t1.unbox() {
        match rt {
          CBN => {
            if let Pair(t21, t22) = t2.unbox() {
              Some(t12.substitution(x, t21).substitution(beta, t22))
            } else {
              None
            }
          }
          _ => match t2.step(rt) {
            Some(v2) => Some(Application(t1.clone(), Box::new(v2))),
            None => None,
          },
        }
      } else {
        match rt {
          CBVR => match t2.step(rt) {
            Some(v2) => Some(Application(t1.clone(), Box::new(v2))),
            _ => match t1.step(rt) {
              Some(v1) => Some(Application(Box::new(v1), t2.clone())),
              _ => None,
            },
          },
          _ => match t1.step(rt) {
            Some(v1) => Some(Application(Box::new(v1), t2.clone())),
            _ => match t2.step(rt) {
              Some(v2) => Some(Application(t1.clone(), Box::new(v2))),
              _ => None,
            },
          },
        }
      }
    } else {
      None
    }
  }

  pub fn reduction(&self, rt: &RedType) -> LDTerm {
    let mut tmp = self.clone();
    println!("  {}", tmp);
    loop {
      match tmp.step(&rt) {
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

  pub fn translate_command_cbv(c: &LbMMtCompCommand) -> LDTerm {
    let LbMMtCompCommand::Command(v, e) = c;
    Application(
      Box::new(LDTerm::translate_term_cbv(v)),
      Box::new(LDTerm::translate_context_cbv(e)),
    )
  }

  fn translate_context_cbv(e: &LbMMtCompContext) -> LDTerm {
    match e {
      LbMMtCompContext::CVariable(alpha) => Variable(alpha.clone()),
      LbMMtCompContext::MtAbstraction(x, c) => {
        Abstraction(x.clone(), Box::new(LDTerm::translate_command_cbv(c)))
      }
      LbMMtCompContext::CStack(v, e1) => {
        let k = generate_kvariable();
        let x = generate_tvariable();
        let t1 = Pair(
          Box::new(Variable(x.clone())),
          Box::new(LDTerm::translate_context_cbv(e1)),
        );
        let t2 = Application(Box::new(Variable(k.clone())), Box::new(t1));
        let t3 = Abstraction(x.clone(), Box::new(t2));
        let t4 = Application(Box::new(LDTerm::translate_term_cbv(v)), Box::new(t3));
        Abstraction(k.clone(), Box::new(t4))
      }
      LbMMtCompContext::CLAbstraction(beta, e) => {
        let y = generate_tvariable();
        let t1 = Application(
          Box::new(LDTerm::translate_context_cbv(e)),
          Box::new(Variable(y.clone())),
        );
        DAbstraction(y.clone(), beta.clone(), Box::new(t1))
      }
    }
  }

  fn translate_term_cbv(v: &LbMMtCompTerm) -> LDTerm {
    match v {
      LbMMtCompTerm::TVariable(x) => {
        let k = generate_kvariable();
        let t1 = Application(Box::new(Variable(k.clone())), Box::new(Variable(x.clone())));
        Abstraction(k.clone(), Box::new(t1))
      }
      LbMMtCompTerm::MAbstraction(beta, c) => {
        Abstraction(beta.clone(), Box::new(LDTerm::translate_command_cbv(c)))
      }
      LbMMtCompTerm::TLAbstraction(x, v) => {
        let k = generate_kvariable();
        let beta = generate_cvariable();
        let t1 = Application(
          Box::new(LDTerm::translate_term_cbv(v)),
          Box::new(Variable(beta.clone())),
        );
        let t2 = DAbstraction(x.clone(), beta.clone(), Box::new(t1));
        let t3 = Application(Box::new(Variable(k.clone())), Box::new(t2));
        Abstraction(k.clone(), Box::new(t3))
      }
      LbMMtCompTerm::TStack(e, v) => {
        let k = generate_kvariable();
        let y = generate_tvariable();
        let t1 = Pair(
          Box::new(Variable(y.clone())),
          Box::new(LDTerm::translate_context_cbv(e)),
        );
        let t2 = Application(Box::new(Variable(k.clone())), Box::new(t1));
        let t3 = Abstraction(y.clone(), Box::new(t2));
        let t4 = Application(Box::new(LDTerm::translate_term_cbv(v)), Box::new(t3));
        Abstraction(k.clone(), Box::new(t4))
      }
    }
  }

  pub fn translate_command_cbn(c: &LbMMtCompCommand) -> LDTerm {
    LDTerm::translate_command_cbv(&c.reverse())
  }
}

impl Clone for LDTerm {
  fn clone(&self) -> LDTerm {
    match self {
      Variable(x) => Variable(x.clone()),
      Abstraction(x, t) => Abstraction(x.clone(), t.clone()),
      Application(t1, t2) => Application(t1.clone(), t2.clone()),
      DAbstraction(x, beta, t) => DAbstraction(x.clone(), beta.clone(), t.clone()),
      Pair(t1, t2) => Pair(t1.clone(), t2.clone()),
    }
  }
}

impl fmt::Display for LDTerm {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match self {
      Variable(x) => write!(f, "{}", x),
      Abstraction(x, t) => write!(f, "λ{}. {}", x, t),
      Application(t1, t2) => {
        let mut s1: String;
        if t1.is_abstraction() || t1.is_dabstraction() {
          s1 = format!("({})", t1);
        } else {
          s1 = format!("{}", t1);
        }
        let mut s2: String;
        if t2.is_variable() || t2.is_pair() {
          s2 = format!("{}", t2);
        } else {
          s2 = format!("({})", t2);
        }
        write!(f, "{} {}", s1, s2)
      }
      DAbstraction(x, beta, t) => write!(f, "λ({}, {}). {}", x, beta, t),
      Pair(t1, t2) => write!(f, "({}, {})", t1, t2),
    }
  }
}

impl fmt::Debug for LDTerm {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match self {
      Variable(x) => write!(f, "Variable({})", x),
      Abstraction(x, t) => write!(f, "Abstraction({}, {:?})", x, t),
      Application(t1, t2) => write!(f, "Application({:?}, {:?})", t1, t2),
      DAbstraction(x, beta, t) => write!(f, "DAbstraction({}, {}, {:?})", x, beta, t),
      Pair(t1, t2) => write!(f, "Pair({:?}, {:?})", t1, t2),
    }
  }
}
