use crate::typ::Type;
use crate::kind::Kind;
use crate::scope::{Scope,ScopeId};
use crate::tlc::TLC;
use crate::constant::Constant;
use std::collections::HashMap;

#[derive(Clone,Copy,Eq,PartialEq,Ord,PartialOrd,Hash)]
pub struct TermId {
   pub id: usize,
}

#[derive(Clone)]
pub struct LetTerm {
   pub scope: ScopeId,
   pub name: String,
   pub parameters: Vec<Vec<(Option<String>,Option<Type>,Kind)>>,
   pub given: Vec<(String,Type)>,
   pub body: Option<TermId>,
   pub rtype: Type,
   pub rkind: Kind,
}

//does not implement Clone because terms are uniquely identified by their id
#[derive(Clone)] //clone seems to be needed to deconflict mutable borrows :(
pub enum Term {
   Ident(String),
   Value(String),
   Arrow(TermId,TermId),
   App(TermId,TermId),
   Let(LetTerm),
   Tuple(Vec<TermId>),
   Block(ScopeId,Vec<TermId>),
   Ascript(TermId,Type),
   As(TermId,Type),
   Constructor(String,Vec<(String,TermId)>),
   Substitution(TermId,TermId,TermId),
   RuleApplication(TermId,String),
   Literal(TermId),
}

impl Term {
   pub fn equals(tlc: &TLC, lt: TermId, rt: TermId) -> bool {
      match (&tlc.rows[lt.id].term, &tlc.rows[rt.id].term) {
         (Term::Ident(li), Term::Ident(ri)) => { li == ri },
         (Term::Value(lv), Term::Value(rv)) => { lv == rv },
         (Term::Arrow(lp,lb), Term::Arrow(rp,rb)) => {
            Term::equals(tlc, *lp, *rp) &&
            Term::equals(tlc, *lb, *rb)
         },
         (Term::App(lp,lb), Term::App(rp,rb)) => {
            Term::equals(tlc, *lp, *rp) &&
            Term::equals(tlc, *lb, *rb)
         },
         (Term::Tuple(ls), Term::Tuple(rs)) => {
            if ls.len() != rs.len() { return false; }
            for (lt, rt) in std::iter::zip(ls, rs) {
            if !Term::equals(tlc, *lt, *rt) {
               return false;
            }}
            true
         },
         _ => false
      }
   }
   pub fn reduce(tlc: &TLC, scope: &Option<ScopeId>, scope_constants: &HashMap<String,Constant>, term: TermId) -> Option<Constant> {
      //scope is only used to look up functions
      //all other variables should already be converted to values
      match &tlc.rows[term.id].term {
         Term::Value(v) => {
            Constant::parse(tlc, &v)
         },
         Term::Constructor(c,cps) if cps.len()==0 => {
            Constant::parse(tlc, &c)
         },
         Term::Tuple(ts) => {
            let mut cs = Vec::new();
            for ct in ts.iter() {
               if let Some(cc) = Term::reduce(tlc, scope, scope_constants, *ct) {
                  cs.push(cc);
               } else { return None; }
            }
            Some(Constant::Tuple(cs))
         },
         Term::App(g,x) => {
            if let Some(xc) = Term::reduce(tlc, scope, scope_constants, *x) {
               let sc = if let Some(sc) = scope { *sc } else { return None; };
               match &tlc.rows[g.id].term {
                  Term::Ident(gv) => {
                     if let Some(binding) = Scope::lookup_term(tlc, sc, gv, &tlc.rows[x.id].typ) {
                        if let Term::Let(lb) = &tlc.rows[binding.id].term {
                           unimplemented!("TODO: beta-reduce function body {}", tlc.print_term(binding))
                        } else {
                           panic!("unexpected lambda format in Term::reduce beta-reduction {}", tlc.print_term(binding))
                        }
                     } else { return None; }
                  },
                  _ => unimplemented!("implement Call-by-Value function call: {}({:?})", tlc.print_term(*g), xc)
               }
            } else { return None; }
         },
         _ => unimplemented!("implement Call-by-Value term reduction: {}", tlc.print_term(term))
      }
   }
}
