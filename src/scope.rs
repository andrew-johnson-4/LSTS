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
         // NO  neg:(Integer)->(Integer) => (Whole)->(Integer)
         // YES neg:(Integer)->(Integer) => (Integer)->(Integer)
         //arrow candidates are covariant because this is an existential context
         //domain(variable) => domain(candidate)
         //range (variable) => range (candidate)
         if cv == v {
         if let Some(cb) = cb {
         match (t,ct) {
            (Type::Arrow(td,tr),Type::Arrow(ctd,ctr)) => { if
               !Type::implies(tlc, td, ctd).is_bottom() &&
               !Type::implies(tlc, tr, ctr).is_bottom() {
               candidates.push((ct.clone(), *cb));
            }}, _ => { if !Type::implies(tlc, t, ct).is_bottom() {
               candidates.push((ct.clone(), *cb));
            }},
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
         //careful specialization can be made sound
         //symbol .binary : {(Integer)->(SignedBinary)+({Integer+Whole})->({Binary+SignedBinary})}
         // .binary : (Whole)->(Binary)
         // .binary : (Integer)->(SignedBinary)', src/scope.rs:57:10
         //choose (Whole)->(Binary) because
         // domain(Whole -> Binary) => domain(Integer -> SignedBinary)
         // range(Whole -> Binary) => range(Integer -> SignedBinary)
         for (xi,(xt,xb)) in candidates.iter().enumerate() {
            let mut all_accept = true;
            for (yi,(yt,_yb)) in candidates.iter().enumerate() {
               if xi==yi { continue; }
               match (xt,yt) {
                  (Type::Arrow(xd,xr),Type::Arrow(yd,yr)) => {
                  if Type::implies(tlc, xd, yd).is_bottom()
                  || Type::implies(tlc, xr, yr).is_bottom() {
                     all_accept = false;
                  }},
                  _ => { all_accept = false; }
               }
            }
            if all_accept { return Some(*xb); }
         }
         let mut cs = "".to_string();
         for (ct,_) in candidates.iter() {
            cs += &format!("\n{} : {:?}", v, ct);
         };
         panic!("Scope::lookup_term multiple viable candidate functions found for symbol {} : {:?}{}", v, t, cs)
      }
   }
}
