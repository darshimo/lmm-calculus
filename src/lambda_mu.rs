use std::char;
use std::collections::VecDeque;
use std::fmt;

use LMCommand::Command;
use LMTerm::{Application, LAbstraction, MAbstraction, Variable};

pub enum LMCommand {
  Command(String, Box<LMTerm>),
}

pub enum LMTerm {
  Variable(String),
  LAbstraction(String, Box<LMTerm>),
  MAbstraction(String, Box<LMCommand>),
  Application(Box<LMTerm>, Box<LMTerm>),
}

impl LMCommand {
  pub fn new(s: String) -> LMCommand {
    let mut q = lexer(s, " []λμ.()");
    LMCommand::parser(&mut q)
  }

  fn parser(q: &mut VecDeque<String>) -> LMCommand {
    let lbracket = q.pop_front().expect("panic");
    if lbracket != "[" {
      panic!("syntax error");
    }
    let alpha = q.pop_front().expect("panic");
    let rbracket = q.pop_front().expect("panic");
    if rbracket != "]" {
      panic!("syntax error");
    }
    let m = LMTerm::parser(q, false);
    Command(alpha, Box::new(m))
  }

  fn substitution_term(&self, x: &String, n: &LMTerm) -> LMCommand {
    let Command(gamma, m) = self;
    Command(gamma.clone(), Box::new(m.substitution_term(x, n)))
  }

  fn substitution_cvariable(&self, beta: &String, alpha: &String) -> LMCommand {
    let Command(gamma, m) = self;
    if beta == gamma {
      Command(
        alpha.clone(),
        Box::new(m.substitution_cvariable(beta, alpha)),
      )
    } else {
      Command(
        gamma.clone(),
        Box::new(m.substitution_cvariable(beta, alpha)),
      )
    }
  }

  fn replacement_subterm(&self, beta: &String, alpha: &String, n: &LMTerm) -> LMCommand {
    let Command(gamma, m) = self;
    if beta == gamma {
      Command(
        alpha.clone(),
        Box::new(Application(
          Box::new(m.replacement_subterm(beta, alpha, n)),
          Box::new(n.clone()),
        )),
      )
    } else {
      Command(
        gamma.clone(),
        Box::new(m.replacement_subterm(beta, alpha, n)),
      )
    }
  }

  fn step(&self) -> Option<LMCommand> {
    let Command(alpha, m) = self;
    if let MAbstraction(beta, c) = m.unbox() {
      Some(c.substitution_cvariable(beta, alpha))
    } else {
      match m.step() {
        Some(n) => Some(Command(alpha.clone(), Box::new(n.clone()))),
        None => None,
      }
    }
  }

  pub fn reduction(&self) -> LMCommand {
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

impl Clone for LMCommand {
  fn clone(&self) -> LMCommand {
    let Command(alpha, m) = self;
    Command(alpha.clone(), m.clone())
  }
}

impl fmt::Display for LMCommand {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    let Command(alpha, m) = self;
    write!(f, "[{}]{}", alpha, m)
  }
}

impl fmt::Debug for LMCommand {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    let Command(alpha, m) = self;
    write!(f, "Command({}, {:?})", alpha, m)
  }
}

impl LMTerm {
  fn unbox(&self) -> &LMTerm {
    self
  }

  fn parser(q: &mut VecDeque<String>, in_parentheses: bool) -> LMTerm {
    let mut tmp: Option<LMTerm> = None;
    while let Some(s) = q.pop_front() {
      if s == " " {
      } else if s == "(" {
        let tmp2 = LMTerm::parser(q, true);
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
        let t = LMTerm::parser(q, false);
        let tmp2 = LAbstraction(v, Box::new(t));
        match tmp {
          None => tmp = Some(tmp2),
          Some(t) => tmp = Some(Application(Box::new(t), Box::new(tmp2))),
        }
      } else if s == "μ" {
        let beta = q.pop_front().expect("panic");
        let dot = q.pop_front().expect("panic");
        if dot != "." {
          panic!("is not dot");
        }
        let c = LMCommand::parser(q);
        let tmp2 = MAbstraction(beta, Box::new(c));
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

  fn substitution_term(&self, x: &String, n: &LMTerm) -> LMTerm {
    match self {
      Variable(y) => {
        if x == y {
          n.clone()
        } else {
          self.clone()
        }
      }
      LAbstraction(y, m) => {
        if x == y {
          self.clone()
        } else {
          let tmp = generate_tvariable();
          LAbstraction(
            tmp.clone(),
            Box::new(
              m.substitution_term(y, &Variable(tmp.clone()))
                .substitution_term(x, n),
            ),
          )
        }
      }
      MAbstraction(beta, c) => MAbstraction(beta.clone(), Box::new(c.substitution_term(x, n))),
      Application(m1, m2) => Application(
        Box::new(m1.substitution_term(x, n)),
        Box::new(m2.substitution_term(x, n)),
      ),
    }
  }

  fn substitution_cvariable(&self, beta: &String, alpha: &String) -> LMTerm {
    match self {
      Variable(_) => self.clone(),
      LAbstraction(x, m) => {
        LAbstraction(x.clone(), Box::new(m.substitution_cvariable(beta, alpha)))
      }
      MAbstraction(gamma, c) => {
        if gamma == beta {
          self.clone()
        } else if gamma != alpha {
          MAbstraction(
            gamma.clone(),
            Box::new(c.substitution_cvariable(beta, alpha)),
          )
        } else {
          let tmp = generate_cvariable();
          MAbstraction(
            tmp.clone(),
            Box::new(
              c.substitution_cvariable(gamma, &tmp)
                .substitution_cvariable(beta, alpha),
            ),
          )
        }
      }
      Application(m, n) => Application(
        Box::new(m.substitution_cvariable(beta, alpha)),
        Box::new(n.substitution_cvariable(beta, alpha)),
      ),
    }
  }

  fn replacement_subterm(&self, beta: &String, alpha: &String, n: &LMTerm) -> LMTerm {
    match self {
      Variable(_) => self.clone(),
      LAbstraction(x, m) => {
        let tmp = generate_tvariable();
        LAbstraction(
          tmp.clone(),
          Box::new(
            m.substitution_term(x, &Variable(tmp.clone()))
              .replacement_subterm(beta, alpha, n),
          ),
        )
      }
      MAbstraction(gamma, c) => {
        let tmp = generate_cvariable();
        MAbstraction(
          tmp.clone(),
          Box::new(
            c.substitution_cvariable(gamma, &tmp)
              .replacement_subterm(beta, alpha, n),
          ),
        )
      }
      Application(m1, m2) => Application(
        Box::new(m1.replacement_subterm(beta, alpha, n)),
        Box::new(m2.replacement_subterm(beta, alpha, n)),
      ),
    }
  }

  fn step(&self) -> Option<LMTerm> {
    if let Application(m1, m2) = self {
      match m1.unbox() {
        LAbstraction(x, m) => Some(m.substitution_term(x, m2)),
        MAbstraction(beta, c) => Some(MAbstraction(
          beta.clone(),
          Box::new(c.replacement_subterm(beta, beta, m2)),
        )),
        _ => match m1.step() {
          Some(n1) => Some(Application(Box::new(n1.clone()), m2.clone())),
          None => None,
        },
      }
    } else {
      None
    }
  }
}

impl Clone for LMTerm {
  fn clone(&self) -> LMTerm {
    match self {
      Variable(x) => Variable(x.clone()),
      LAbstraction(x, m) => LAbstraction(x.clone(), m.clone()),
      MAbstraction(beta, c) => MAbstraction(beta.clone(), c.clone()),
      Application(m, n) => Application(m.clone(), n.clone()),
    }
  }
}

impl fmt::Display for LMTerm {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match self {
      Variable(x) => write!(f, "{}", x),
      LAbstraction(x, t) => write!(f, "λ{}. {}", x, t),
      MAbstraction(beta, c) => write!(f, "μ{}.{}", beta, c),
      Application(t1, t2) => {
        let mut s1: String;
        if t1.is_labstraction() || t1.is_mabstraction() {
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

impl fmt::Debug for LMTerm {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match self {
      Variable(x) => write!(f, "Variable({})", x),
      MAbstraction(x, t) => write!(f, "MAbstraction({}, {:?})", x, t),
      LAbstraction(beta, c) => write!(f, "LAbstraction({}, {:?})", beta, c),
      Application(t1, t2) => write!(f, "Application({:?}, {:?})", t1, t2),
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
    a = TCNT;
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
