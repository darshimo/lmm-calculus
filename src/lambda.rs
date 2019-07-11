use std::collections::VecDeque;
use std::fmt;

use LTerm::{Abstraction, Application, Variable};
use RedType::{CBN, CBVR};

use crate::generator::*;
use crate::lambdabar_mu_mutilde_comp::*;
use crate::lexer::*;

pub enum LTerm {
  Variable(String),
  Abstraction(String, Box<LTerm>),
  Application(Box<LTerm>, Box<LTerm>),
}

pub enum RedType {
  CBN,
  CBVL,
  CBVR,
}

impl LTerm {
  pub fn new(s: String) -> LTerm {
    let mut q = lexer(s, " λ.()");
    LTerm::parser(&mut q, false)
  }

  fn unbox(&self) -> &LTerm {
    self
  }

  fn parser(q: &mut VecDeque<String>, in_parentheses: bool) -> LTerm {
    let mut tmp: Option<LTerm> = None;
    while let Some(s) = q.pop_front() {
      if s == " " {
      } else if s == "(" {
        let tmp2 = LTerm::parser(q, true);
        match tmp {
          None => tmp = Some(tmp2),
          Some(t) => tmp = Some(Application(Box::new(t), Box::new(tmp2))),
        }
      } else if s == ")" {
        if !in_parentheses {
          q.push_front(String::from(")"));
        }
        return tmp.expect("panic");
      } else if s == "λ" {
        let v = q.pop_front().expect("panic");
        let dot = q.pop_front().expect("panic");
        if dot != "." {
          panic!("is not dot");
        }
        let t = LTerm::parser(q, false);
        let tmp2 = Abstraction(v, Box::new(t));
        match tmp {
          None => tmp = Some(tmp2),
          Some(t) => tmp = Some(Application(Box::new(t), Box::new(tmp2))),
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

  fn substitution(&self, x: &String, v: &LTerm) -> LTerm {
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
          let tmp = generate_tvariable();
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
    }
  }

  fn step(&self, rt: &RedType) -> Option<LTerm> {
    if let Application(t1, t2) = self {
      if let Abstraction(x, t12) = t1.unbox() {
        match rt {
          CBN => Some(t12.substitution(x, t2)),
          _ => match t2.step(rt) {
            Some(v2) => Some(Application(t1.clone(), Box::new(v2))),
            None => {
              if t2.is_variable() || t2.is_abstraction() {
                Some(t12.substitution(x, t2))
              } else {
                None
              }
            }
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

  pub fn reduction(&self, rt: &RedType) -> LTerm {
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

  pub fn translate_command_cbv(c: &LbMMtCompCommand) -> LTerm {
    let LbMMtCompCommand::Command(v, e) = c;
    Application(
      Box::new(LTerm::translate_term_cbv(v)),
      Box::new(LTerm::translate_context_cbv(e)),
    )
  }

  fn translate_context_cbv(e: &LbMMtCompContext) -> LTerm {
    match e {
      LbMMtCompContext::CVariable(alpha) => Variable(alpha.clone()),
      LbMMtCompContext::MtAbstraction(x, c) => {
        Abstraction(x.clone(), Box::new(LTerm::translate_command_cbv(c)))
      }
      LbMMtCompContext::CStack(v, e1) => {
        let k = generate_kvariable();
        let x = generate_tvariable();
        let t1 = Application(Box::new(Variable(k.clone())), Box::new(Variable(x.clone())));
        let t2 = Application(Box::new(t1), Box::new(LTerm::translate_context_cbv(e1)));
        let t3 = Abstraction(x.clone(), Box::new(t2));
        let t4 = Application(Box::new(LTerm::translate_term_cbv(v)), Box::new(t3));
        Abstraction(k.clone(), Box::new(t4))
      }
      LbMMtCompContext::CLAbstraction(beta, e) => {
        let y = generate_tvariable();
        let t1 = Application(
          Box::new(LTerm::translate_context_cbv(e)),
          Box::new(Variable(y.clone())),
        );
        let t2 = Abstraction(beta.clone(), Box::new(t1));
        Abstraction(y.clone(), Box::new(t2))
      }
    }
  }

  fn translate_term_cbv(v: &LbMMtCompTerm) -> LTerm {
    match v {
      LbMMtCompTerm::TVariable(x) => {
        let k = generate_kvariable();
        let t1 = Application(Box::new(Variable(k.clone())), Box::new(Variable(x.clone())));
        Abstraction(k.clone(), Box::new(t1))
      }
      LbMMtCompTerm::MAbstraction(beta, c) => {
        Abstraction(beta.clone(), Box::new(LTerm::translate_command_cbv(c)))
      }
      LbMMtCompTerm::TLAbstraction(x, v) => {
        let k = generate_kvariable();
        let beta = generate_cvariable();
        let t1 = Application(
          Box::new(LTerm::translate_term_cbv(v)),
          Box::new(Variable(beta.clone())),
        );
        let t2 = Abstraction(beta.clone(), Box::new(t1));
        let t3 = Abstraction(x.clone(), Box::new(t2));
        let t4 = Application(Box::new(Variable(k.clone())), Box::new(t3));
        Abstraction(k.clone(), Box::new(t4))
      }
      LbMMtCompTerm::TStack(e, v) => {
        let k = generate_kvariable();
        let y = generate_tvariable();
        let t1 = Application(Box::new(Variable(k.clone())), Box::new(Variable(y.clone())));
        let t2 = Application(Box::new(t1), Box::new(LTerm::translate_context_cbv(e)));
        let t3 = Abstraction(y.clone(), Box::new(t2));
        let t4 = Application(Box::new(LTerm::translate_term_cbv(v)), Box::new(t3));
        Abstraction(k.clone(), Box::new(t4))
      }
    }
  }

  pub fn translate_command_cbn(c: &LbMMtCompCommand) -> LTerm {
    LTerm::translate_command_cbv(&c.reverse())
  }

  fn translate_context_cbn(e: &LbMMtCompContext) -> LTerm {
    LTerm::translate_term_cbv(&e.reverse())
  }

  fn translate_term_cbn(v: &LbMMtCompTerm) -> LTerm {
    LTerm::translate_context_cbv(&v.reverse())
  }
}

impl Clone for LTerm {
  fn clone(&self) -> LTerm {
    match self {
      Variable(x) => Variable(x.clone()),
      Abstraction(x, t1) => Abstraction(x.clone(), t1.clone()),
      Application(t1, t2) => Application(t1.clone(), t2.clone()),
    }
  }
}

impl fmt::Display for LTerm {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match self {
      Variable(x) => write!(f, "{}", x),
      Abstraction(x, t) => write!(f, "λ{}. {}", x, t),
      Application(t1, t2) => {
        let mut s1: String;
        if t1.is_abstraction() {
          s1 = format!("({})", t1);
        } else {
          s1 = format!("{}", t1);
        }
        let mut s2: String;
        if t2.is_variable() {
          s2 = format!("{}", t2);
        } else {
          s2 = format!("({})", t2);
        }

        write!(f, "{} {}", s1, s2)
      }
    }
  }
}

impl fmt::Debug for LTerm {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match self {
      Variable(x) => write!(f, "Variable({})", x),
      Abstraction(x, t) => write!(f, "Abstraction({}, {:?})", x, t),
      Application(t1, t2) => write!(f, "Application({:?}, {:?})", t1, t2),
    }
  }
}

/*
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
      ret.push_back(c.to_string());
    } else {
      tmp.push(c);
    }
  }
  if !tmp.is_empty() {
    ret.push_back(tmp.into_iter().collect());
  }

  ret
}
*/
