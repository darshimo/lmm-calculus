mod lambda;
mod lambda_mu;
//mod lambdabar_mu;
mod lambdabar_mu_mutilde;
mod read;

use crate::lambda::RedType::{CBN, CBVL, CBVR};
use std::collections::VecDeque;

use lambda::*;
use lambda_mu::*;
//use lambdabar_mu::*;
use lambdabar_mu_mutilde::*;

enum Calculus {
    L,
    LM,
    LbMMt,
}

use Calculus::{LbMMt, L, LM};

fn main() {
    //println!("head reduction machine in λ̅μ-calculus");
    //println!("please input λ̅μ-term");

    println!("please input term to reduce.");

    let (s, clcls) = remove_head_whitespace(&read::read());

    println!("");
    println!("{}", s);
    println!("");

    match clcls {
        L => {
            println!("head reduction in λ-calculus");
            println!("select CBN(0) or CBVL(1) or CBVR(2)");
            let i: u8 = read::read();
            let rt: RedType;
            if i == 0 {
                rt = CBN;
            } else if i == 1 {
                rt = CBVL;
            } else if i == 2 {
                rt = CBVR;
            } else {
                panic!("error");
            }
            let e = LTerm::new(s);
            println!("");
            e.reduction(&rt);
        }
        LM => {
            println!("head reduction in λμ-calculus");
            let e = LMCommand::new(s);
            println!("");
            e.reduction();
        }
        LbMMt => {
            println!("head reduction in λ̅μμ̃-calculus");
            println!("select CBN(0) or CBV(1)");
            let i: u8 = read::read();
            let rt: bool;
            if i == 0 {
                rt = true;
            } else if i == 1 {
                rt = false;
            } else {
                panic!("error");
            }
            let e = LbMMtCommand::new(s);
            println!("");
            e.reduction(rt);
        }
    }
}

fn remove_head_whitespace(s: &String) -> (String, Calculus) {
    let mut vd: VecDeque<char> = s.chars().collect();
    let clcls: Calculus;
    loop {
        let c = vd.get(0).expect("empty string.");
        if *c == ' ' {
            vd.pop_front();
        } else {
            if *c == '[' {
                clcls = LM;
            } else if *c == '⟨' {
                clcls = LbMMt;
            } else {
                clcls = L;
            }
            break;
        }
    }
    (vd.into_iter().collect::<String>(), clcls)
}
