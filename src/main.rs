mod lambda;
mod lambdabar_mu;
mod read;

use lambda::*;
use lambdabar_mu::*;

fn main() {
    //println!("λ-calculus head reduction machine");
    //println!("please input λ-term");
    //println!("λμ-calculus");
    println!("λ̅μ-calculus");
    println!("please input λ̅μ-term");
    //println!("λ̅μμ̃-calculus");

    let s: String = read::read();

    let e = LbMCommand::new(s);

    println!("");
    println!("{}", e);
    //e.reduction();
}
