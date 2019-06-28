//use std::io;
use std::char;
use std::fmt;

enum Lambda {
    Variable(String),
    Abstraction(String, Box<Lambda>),
    Application(Box<Lambda>, Box<Lambda>),
}

impl Lambda {
    fn is_variable(&self) -> bool {
        match self {
            Lambda::Variable(_) => true,
            _ => false,
        }
    }

    fn is_abstraction(&self) -> bool {
        match self {
            Lambda::Abstraction(_, _) => true,
            _ => false,
        }
    }

    fn substitution(&self, x: &String, v: &Lambda) -> Lambda {
        match self {
            Lambda::Variable(y) => {
                if x == y {
                    v.clone()
                } else {
                    self.clone()
                }
            }
            Lambda::Abstraction(y, t) => {
                if x == y {
                    self.clone()
                } else {
                    let tmp = generate_variable();
                    Lambda::Abstraction(
                        tmp.clone(),
                        Box::new(
                            t.substitution(&y, &Lambda::Variable(tmp.clone()))
                                .substitution(&x, &v),
                        ),
                    )
                }
            }
            Lambda::Application(t1, t2) => Lambda::Application(
                Box::new(t1.substitution(&x, &v)),
                Box::new(t2.substitution(&x, &v)),
            ),
        }
    }

    fn application(&self, v2: &Lambda) -> Option<Lambda> {
        if let Lambda::Abstraction(x, t12) = self {
            Some(t12.substitution(x, v2))
        } else {
            None
        }
    }

    fn step(&self) -> Option<Lambda> {
        if let Lambda::Application(t1, t2) = self {
            if t1.is_abstraction() {
                if t2.is_abstraction() {
                    t1.application(t2)
                } else {
                    match t2.step() {
                        None => None,
                        Some(t22) => Some(Lambda::Application(t1.clone(), Box::new(t22.clone()))),
                    }
                }
            } else {
                match t1.step() {
                    None => None,
                    Some(t11) => Some(Lambda::Application(Box::new(t11.clone()), t2.clone())),
                }
            }
        } else {
            None
        }
    }

    fn reduction(&self) -> Lambda {
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
}

impl Clone for Lambda {
    fn clone(&self) -> Lambda {
        match self {
            Lambda::Variable(x) => Lambda::Variable(x.clone()),
            Lambda::Abstraction(x, t1) => Lambda::Abstraction(x.clone(), t1.clone()),
            Lambda::Application(t1, t2) => Lambda::Application(t1.clone(), t2.clone()),
        }
    }
}

impl fmt::Display for Lambda {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Lambda::Variable(x) => write!(f, "{}", x),
            Lambda::Abstraction(x, t1) => write!(f, "λ{}. {}", x, t1),
            Lambda::Application(t1, t2) => {
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

fn main() {
    println!("λ-calculus");
    println!("λμ-calculus");
    println!("λ̅μ-calculus");
    println!("λ̅μμ̃-calculus");
    println!("");

    let x = String::from("x");
    let y = String::from("y");
    let z = String::from("z");
    let w = String::from("w");

    let a = Lambda::Abstraction(
        x.clone(),
        Box::new(Lambda::Abstraction(
            y.clone(),
            Box::new(Lambda::Application(
                Box::new(Lambda::Variable(x.clone())),
                Box::new(Lambda::Variable(y.clone())),
            )),
        )),
    );
    let b = Lambda::Abstraction(z.clone(), Box::new(Lambda::Variable(z.clone())));
    let c = Lambda::Application(Box::new(a.clone()), Box::new(b.clone()));
    let d = Lambda::Abstraction(w.clone(), Box::new(Lambda::Variable(w.clone())));
    let e = Lambda::Application(Box::new(c.clone()), Box::new(d.clone()));

    e.reduction();
}
