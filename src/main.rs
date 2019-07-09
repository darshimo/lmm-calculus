mod lambda;
mod read;

use lambda::*;

fn main() {
    println!("λ-calculus head reduction machine");
    //println!("λμ-calculus");
    //println!("λ̅μ-calculus");
    //println!("λ̅μμ̃-calculus");
    println!("please input λ-term");

    let s: String = read::read();

    let e = Lambda::new(s);

    println!("");
    e.reduction();
}
