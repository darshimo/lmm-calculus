mod lambda;
mod lambdabar_mu;
mod lambdabar_mu_mutilde;
mod read;

//use lambda::*;
//use lambdabar_mu::*;
use lambdabar_mu_mutilde::*;

fn main() {
    //println!("head reduction machine in λ-calculus");
    //println!("please input λ-term");
    //println!("head reduction machine in λμ-calculus");
    //println!("please input λμ-term");
    //println!("head reduction machine in λ̅μ-calculus");
    //println!("please input λ̅μ-term");
    println!("head reduction machine in λ̅μμ̃-calculus");
    println!("please input λ̅μμ̃-term");

    let s: String = read::read();

    let e = LbMMtCommand::new(s);

    println!("");
    let cbn = true;
    e.reduction(cbn);
}
