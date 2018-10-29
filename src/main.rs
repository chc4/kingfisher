#![feature(option_replace)]
#[macro_use] extern crate lalrpop_util;
#[macro_use] extern crate rental;

lalrpop_mod!(pub parser);
use self::Term::*;

#[derive(Debug,Clone,PartialEq)]
pub enum Term {
    Lam(String, Box<Term>),
    App(Box<Term>,Box<Term>),
    Var(String),
    Nat(u32)
}

#[derive(Debug)]
pub struct FarVar {
    name: String,
    val: Option<u32>
}

rental! {
pub mod rent_far {
    use crate::FarVar;
    #[rental_mut(debug)]
    pub struct FarLam<T: 'static> {
        arg: String,
        body: Box<T>,
        occ: Option<&'body mut FarVar>
    }
}
}
use self::rent_far::*;
#[derive(Debug)]
pub enum Far {
    Lam(FarLam<Far>),
    App(Box<Far>,Box<Far>),
    Var(FarVar),
}

fn find_ref<'a>(var: String, body: &'a mut Far) -> Option<&'a mut FarVar> {
    match body {
        Far::Var(val) => {
            if *val.name == var {
                return Some(val);
            }
            return None;
        },
        Far::App(left, right) => {
            // variables are affine, so we only ever return one option
            // lazily apply to right in case we've already found in left
            find_ref(var.clone(), left).or_else(move || find_ref(var, right) )
        },
        Far::Lam(lam) => {
            // we need to take mutable borrows of sublambdas and rust can't guarentee we aren't
            // invalidating `occ`. i'm...reasonably sure this is safe: lambdas are boxed and won't
            // be realloced, so accessing `occ` should stay safe
            struct priv_hack {
                arg: String,
                body: Box<Far>,
                occ: Option<*mut FarVar>
            }
            unsafe {
                let raw_lam = lam as *mut FarLam<Far> as *mut priv_hack;
                if (*raw_lam).arg == var { panic!("shadowed variable") }
                find_ref(var, &mut (*raw_lam).body)
            }
        }
    }
}

fn test_far() {
    let val: Far = Far::Var(FarVar { name: "x".to_string(), val: None });
    let mut t = FarLam::new("x".to_string(),
        |arg| Box::new(val),
        |body| { find_ref("x".to_string(),body) }); // if let Far::Var(body) = body { Some(&mut *body) } else { None } } );

    t.rent_mut(|occ|
        occ.as_mut().map(|occ| occ.val = Some(13) ) );

    println!("wtf {:?}", t);
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
    test_far();
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














