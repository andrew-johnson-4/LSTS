use crate::typ::Type;
use crate::kind::Kind;
use crate::scope::ScopeId;
use crate::tlc::TLC;

#[derive(Clone,Copy,Eq,PartialEq,Ord,PartialOrd,Hash)]
pub struct TermId {
   pub id: usize,
}
//does not implement Clone because terms are uniquely identified by their id
#[derive(Clone)] //clone seems to be needed to deconflict mutable borrows :(
pub enum Term {
   Ident(String),
   Value(String),
   Arrow(TermId,TermId),
   App(TermId,TermId),
   Let(ScopeId,String,Vec<Vec<(Option<String>,Option<Type>,Kind)>>,Option<TermId>,Type,Kind),
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
}
