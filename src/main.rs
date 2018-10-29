#[macro_use] extern crate lalrpop_util;

lalrpop_mod!(pub parser);
use self::Term::*;

#[derive(Debug,Clone,PartialEq)]
pub enum Term {
    Lam(String, Box<Term>),
    App(Box<Term>,Box<Term>),
    Var(String),
    Nat(u32)
}

fn apply(term: Term) -> Term {
    println!("apply {:?}", term);
    match term.clone() {
        App(f, arg) => {
            let f = apply(*f);
            let arg = apply(*arg);
            if let Lam(x, body) = f {
                return apply(replace(*body, x, arg))
            }
            term
        }
        _ => term
    }
}

fn replace(body: Term, var: String, arg: Term) -> Term {
    println!("replace in {:?} {}={:?}", body, var, arg);
    match body.clone() {
        Lam(x, b) => {
            if x == var {
                panic!("shadowed variable")
            }
            Lam(x, Box::new(replace(*b, var, arg)))
        },
        App(a, b) => {
            App(
                Box::new(replace(*a, var.clone(), arg.clone())),
                Box::new(replace(*b, var, arg))
            )
        },
        Var(s) => {
            if s == var {
                return arg;
            }
            body
        },
        _ => body
    }
}

fn main() {
    println!("Hello, world!");
    let test = "(\\t. t 1 2) (\\a. \\b. a)";
    let test_lam = parser::TermParser::new().parse(test).unwrap();
    println!("{:?}",test_lam);
    println!("{:?}",apply(test_lam));
}

#[test]
fn parser_test() {
    let parser = parser::TermParser::new();
    let x = "x".to_string();
    let y = "y".to_string();
    assert_eq!(parser.parse(r"\x. x").unwrap(),
        Lam(x.clone(), Box::new(Var(x.clone()))));
    // test that app grouping works correctly
    assert_eq!(parser.parse(r"\x.\y.x y").unwrap(),
        Lam(x.clone(),Box::new(
            Lam(y.clone(),Box::new(
                    App(Box::new(Var(x.clone())),Box::new(Var(y.clone()))))))));
}














