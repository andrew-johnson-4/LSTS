use crate::tlc::TLC;
use crate::term::{Term,TermId};
use std::collections::HashMap;

#[derive(Clone,Eq,PartialEq,Ord,PartialOrd,Hash)]
pub enum Constant {
   NaN,
   Boolean(bool),
   Integer(i64),
   Op(String),
   Tuple(Vec<Constant>),
}

impl std::fmt::Debug for Constant {
   fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
      match self {
        Constant::NaN => write!(f, "NaN"),
        Constant::Boolean(c) => write!(f, "{}", if *c {"True"} else {"False"}),
        Constant::Integer(i) => write!(f, "{}", i),
        Constant::Op(op) => write!(f, "{}", op),
        Constant::Tuple(ts) => write!(f, "({})", ts.iter()
           .map(|t|format!("{:?}",t)).collect::<Vec<String>>()
           .join(",") ),
      }
   }
}

impl Constant {
   pub fn parse(_tlc: &TLC, v: &str) -> Option<Constant> {
      if      v=="NaN" { Some(Constant::NaN) }
      else if v=="True" { Some(Constant::Boolean(true)) }
      else if v=="False" { Some(Constant::Boolean(false)) }
      else if let Ok(vi) = v.parse::<i64>() { Some(Constant::Integer(vi)) }
      else { Some(Constant::Op(v.to_string())) }
   }
   pub fn eval(tlc: &TLC, ctx: &HashMap<String,Constant>, term: TermId) -> Option<Constant> {
      match &tlc.rows[term.id].term {
         Term::Value(v) => {
            Constant::parse(tlc, v)
         },
         Term::Ident(v) => {
            ctx.get(v).cloned()
         },
         Term::Tuple(ts) => {
            let mut cts = Vec::new();
            for tc in ts.iter() {
               if let Some(tc) = Constant::eval(tlc, ctx, *tc) {
                  cts.push(tc);
               } else { return None; }
            }
            Some(Constant::Tuple(cts))
         },
         Term::App(g,x) => {
            let xc = if let Some(xc) = Constant::eval(tlc, ctx, *x) { xc } else { return None; };
            if let Term::Ident(op) = &tlc.rows[g.id].term {
               match (op.as_str(), xc) {
                  ("not", Constant::Boolean(c)) => { Some(Constant::Boolean(!c)) },
                  ("pos", Constant::Integer(c)) => { Some(Constant::Integer(c)) },
                  ("neg", Constant::Integer(c)) => { Some(Constant::Integer(-c)) },
                  (bop, Constant::Tuple(cs)) if cs.len()==2 => {
                     match (bop, &cs[0], &cs[1]) {
                        ("&&", Constant::Boolean(x), Constant::Boolean(y)) => { Some(Constant::Boolean(*x && *y)) },
                        ("||", Constant::Boolean(x), Constant::Boolean(y)) => { Some(Constant::Boolean(*x || *y)) },
                        ("+", Constant::Integer(x), Constant::Integer(y)) => { Some(Constant::Integer(x + y)) },
                        ("-", Constant::Integer(x), Constant::Integer(y)) => { Some(Constant::Integer(x - y)) },
                        ("*", Constant::Integer(x), Constant::Integer(y)) => { Some(Constant::Integer(x * y)) },
                        ("/", Constant::Integer(x), Constant::Integer(y)) => { Some(Constant::Integer(x / y)) },
                        ("%", Constant::Integer(x), Constant::Integer(y)) => { Some(Constant::Integer(x % y)) },
                        ("^", Constant::Integer(x), Constant::Integer(y)) => { Some(Constant::Integer( x.pow(*y as u32) )) },
                        ("<", Constant::Integer(x), Constant::Integer(y)) => { Some(Constant::Boolean(x < y)) },
                        ("<=", Constant::Integer(x), Constant::Integer(y)) => { Some(Constant::Boolean(x <= y)) },
                        (">", Constant::Integer(x), Constant::Integer(y)) => { Some(Constant::Boolean(x > y)) },
                        (">=", Constant::Integer(x), Constant::Integer(y)) => { Some(Constant::Boolean(x >= y)) },
                        ("==", Constant::Integer(x), Constant::Integer(y)) => { Some(Constant::Boolean(x == y)) },
                        ("!=", Constant::Integer(x), Constant::Integer(y)) => { Some(Constant::Boolean(x != y)) },
                        _ => panic!("Constant::eval unknown binary operator {}", bop)
                     }
                  },
                  (top, Constant::Tuple(cs)) if cs.len()==3 => {
                     match (top, &cs[0], &cs[1], &cs[2]) {
                        ("if", Constant::Boolean(c), x, y) => { Some(if *c { x.clone() } else { y.clone() }) },
                        _ => panic!("Constant::eval unknown ternary operator {}", top)
                     }
                  },
                  _ => panic!("Constant::eval unknown operator {}", op)
               }
            } else {
               unimplemented!("Constant::eval apply {}", tlc.print_term(*g))
            }
         },
         _ => { unimplemented!("Constant::eval {}", tlc.print_term(term)) }
      }
   }
}
