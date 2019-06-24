//use std::io;

enum Lambda {
    Variable(char),
    Abstraction(char, Box<Lambda>),
    Application(Box<Lambda>, Box<Lambda>),
}

fn main() {
    println!("λ-calculus");
    println!("λμ-calculus");
    println!("λ̅μ-calculus");
    println!("λ̅μμ̃-calculus");
    println!("");

    //let s = String::new();
    //io::stdin().read_line(&mut s).expect("error");

    let a = Lambda::Variable('x');
    let b = Lambda::Abstraction('x', Box::new(Lambda::Variable('x')));
    let c = Lambda::Application(
        Box::new(Lambda::Abstraction('x', Box::new(Lambda::Variable('x')))),
        Box::new(Lambda::Variable('y')),
    );

    print_lambda(&a);
    print_lambda(&b);
    print_lambda(&c);
}

fn print_lambda(l: &Lambda) {
    match l {
        Lambda::Variable(x) => print_lambda_variable(*x),
        Lambda::Abstraction(x, t) => print_lambda_abstraction(*x, &t),
        Lambda::Application(t1, t2) => print_lambda_application(&t1, &t2),
    }

    println!("");
}

fn print_lambda_variable(x: char) {
    print!("{}", x.to_string());
}

fn print_lambda_abstraction(x: char, l: &Lambda) {
    print!("λ{}.", x);
    match l {
        Lambda::Variable(x) => print_lambda_variable(*x),
        Lambda::Abstraction(x, t) => print_lambda_abstraction(*x, &t),
        Lambda::Application(t1, t2) => print_lambda_application(&t1, &t2),
    }
}

fn print_lambda_application(l1: &Lambda, l2: &Lambda) {
    match l1 {
        Lambda::Variable(x) => print_lambda_variable(*x),
        Lambda::Abstraction(x, t) => {
            print!("(");
            print_lambda_abstraction(*x, &t);
            print!(")");
        }
        Lambda::Application(t1, t2) => print_lambda_application(&t1, &t2),
    }
    match l2 {
        Lambda::Variable(x) => print_lambda_variable(*x),
        Lambda::Abstraction(x, t) => {
            print!("(");
            print_lambda_abstraction(*x, &t);
            print!(")");
        }
        Lambda::Application(t1, t2) => {
            print!("(");
            print_lambda_application(&t1, &t2);
            print!(")");
        }
    }
}
