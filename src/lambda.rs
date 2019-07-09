use std::char;
use std::collections::VecDeque;
use std::fmt;

use LTerm::{Abstraction, Application, Variable};

pub enum LTerm {
  Variable(String),
  Abstraction(String, Box<LTerm>),
  Application(Box<LTerm>, Box<LTerm>),
}

impl LTerm {
  pub fn new(s: String) -> LTerm {
    let mut q = lexer(s, " λ.()");
    LTerm::parser(&mut q, false)
  }

  fn parser(q: &mut VecDeque<String>, in_abst: bool) -> LTerm {
    let mut tmp: Option<LTerm> = None;
    while let Some(s) = q.pop_front() {
      if s == " " {
      } else if s == "(" {
        let tmp2 = LTerm::parser(q, false);
        match tmp {
          None => tmp = Some(tmp2),
          Some(t) => tmp = Some(Application(Box::new(t), Box::new(tmp2))),
        }
      } else if s == ")" {
        if in_abst {
          q.push_front(String::from(")"));
        }
        return tmp.expect("panic");
      } else if s == "λ" {
        let v = q.pop_front().expect("panic");
        let dot = q.pop_front().expect("panic");
        if dot != "." {
          panic!("is not dot");
        }
        let t = LTerm::parser(q, true);
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
          let tmp = generate_variable();
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

  fn application(&self, v2: &LTerm) -> Option<LTerm> {
    if let Abstraction(x, t12) = self {
      Some(t12.substitution(x, v2))
    } else {
      None
    }
  }

  fn step(&self) -> Option<LTerm> {
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

  pub fn reduction(&self) -> LTerm {
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

static mut CNT: u32 = 0;

fn generate_variable() -> String {
  let mut a: u32;
  unsafe {
    CNT += 1;
    a = CNT;
  }

  let mut s = String::new();

  while a > 0 {
    let b = a % 10;
    a /= 10;
    s = format!("{}{}", char::from_u32(8320 + b).unwrap(), s);
  }

  format!("x{}", s)
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
