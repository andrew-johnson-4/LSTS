use std::collections::HashMap;
use crate::typ::Type;
use crate::kind::Kind;
use crate::term::TermId;
use crate::tlc::TLC;

#[derive(Clone, Copy, Debug)]
pub struct ScopeId {
   pub id: usize,
}

#[derive(Clone)]
pub struct Scope {
   pub parent: Option<ScopeId>,
   pub children: Vec<(String,HashMap<Type,Kind>,Type,Option<TermId>)>,
}

impl Scope {
   pub fn lookup_term(tlc: &TLC, scope: ScopeId, v: &str, t: &Type) -> Option<TermId> {
      let mut candidates = Vec::new();
      for (cv,_ck,ct,cb) in tlc.scopes[scope.id].children.iter() {
         if cv == v {
         if !Type::implies(tlc, t, ct).is_bottom() {
         if let Some(cb) = cb {
            candidates.push((ct.clone(), *cb));
         }}}
      }
      if candidates.len() == 0 {
         if let Some(psc) = tlc.scopes[scope.id].parent {
            return Scope::lookup_term(tlc, psc, v, t);
         } else {
            return None;
         }
      } else if candidates.len() == 1 {
         return Some(candidates[0].1);
      } else {
         let mut cs = "".to_string();
         for (ct,_) in candidates.iter() {
            cs += &format!("\n{} : {:?}", v, ct);
         };
         panic!("Scope::lookup_term multiple viable candidate functions found for symbol {} : {:?}{}", v, t, cs)
      }
   }
}
