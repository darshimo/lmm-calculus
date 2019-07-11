mod generator;
mod lambda;
mod lambda_double;
mod lambda_mu;
mod lambdabar_mu;
mod lambdabar_mu_mutilde;
mod lambdabar_mu_mutilde_comp;
mod lexer;
mod read;

use lambda::*;
use lambda_double::*;
use lambda_mu::*;
use lambdabar_mu::*;
use lambdabar_mu_mutilde::*;
use lambdabar_mu_mutilde_comp::*;

fn main() {
    let s: String = read::read();

    let e = LMCommand::new(s);
    let e1 = LbMMtCompCommand::translate_l2r(&e);
    let e1r = e1.reverse();
    println!("e  : {}", e);
    println!("e1 : {}", e1);
    println!("e1r: {}", e1r);

    let e2 = LTerm::translate_command_cbn(&e1);
    println!("e2");
    e2.reduction(&lambda::RedType::CBN);

    let e3 = LTerm::translate_command_cbv(&e1);
    println!("e3");
    e3.reduction(&lambda::RedType::CBN);
}
