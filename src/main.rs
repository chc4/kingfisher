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
pub struct FarNat {
    val: u32
}

#[derive(Debug)]
pub struct FarVar {
    name: String,
    val: Option<Box<Far>>
}

pub struct __FarLam {
    arg: String,
    body: Box<Far>
}

impl std::ops::Deref for __FarLam {
    type Target = Box<Far>;

    fn deref(&self) -> &<Self as std::ops::Deref>::Target {
        &self.body
    }
}

impl std::ops::DerefMut for __FarLam {
    type Target = Box<Far>;

    fn deref_mut<'a>(&'a mut self) -> &'a mut <Self as std::ops::Deref>::Target {
        &mut self.body
    }
}

// returning a box, textbook stable deref
unsafe impl rental::__rental_prelude::StableDeref for __FarLam { }

rental! {
pub mod rent_far {
    use crate::{__FarLam,FarVar};
    #[rental_mut(debug)]
    pub struct FarLam {
        lambda: __FarLam,
        occ: Option<&'lambda mut FarVar>
    }
}
}
use self::rent_far::*;
#[derive(Debug)]
pub enum Far {
    Lam(FarLam),
    App(Box<Far>,Box<Far>),
    Var(FarVar),
    Nat(FarNat),
}

fn find_ref<'a>(var: String, body: &'a mut Far) -> Option<&'a mut FarVar> {
    match body {
        Far::Nat(nat) => None,
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
                let raw_lam = lam as *mut FarLam as *mut priv_hack;
                if (*raw_lam).arg == var { panic!("shadowed variable") }
                find_ref(var, &mut (*raw_lam).body)
            }
        }
    }
}

fn run(term: Far) -> Far {
    println!("run {:?}", term);
    match term {
        Far::App(f, arg) => {
            let f = run(*f);
            //let arg = run(*arg);
            if let Far::Lam(mut lam) = f {
                lam.rent_mut(|occ|
                    occ.as_mut().map(|occ| occ.val = Some(arg) ) );
                println!("updated? {:?}", lam);
                run(*lam.into_head().body)
                // into_head only returns the first member, while rent() returns the last member -
                // guess we're doing the unsafe transmute dance again
                // epistemic status: probably unsafe? https://github.com/jpernst/rental/issues/22
                // mentions it may break aliasing!
            } else {
                panic!("apply non-lambda");
            }
        },
        _ => term
    }
}

fn main() {
    let val: Far = Far::Var(FarVar { name: "x".to_string(), val: None });
    let mut t = FarLam::new(__FarLam { arg: "x".to_string(), body: Box::new(val) },
        |body| { find_ref("x".to_string(),body) }); // if let Far::Var(body) = body { Some(&mut *body) } else { None } } );

    //t.rent_mut(|occ|
    //    occ.as_mut().map(|occ| occ.val = Some(13) ) );

    println!("wtf {:?}", t);

    let parser = parser::TermParser::new();
    let test = parser.parse("(\\x.x) 1").unwrap();
    println!("{:?}", test);
    println!("{:?}", run(test));
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

/*
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
*/














