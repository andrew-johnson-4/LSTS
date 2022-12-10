use crate::typ::Type;
use crate::kind::Kind;
use crate::scope::{Scope,ScopeId};
use crate::tlc::TLC;
use crate::constant::Constant;
use crate::token::{Span};
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
   pub body: Option<TermId>,
   pub rtype: Type,
   pub rkind: Kind,
}

//does not implement Clone because terms are uniquely identified by their id
#[derive(Clone)] //clone seems to be needed to deconflict mutable borrows :(
pub enum Term {
   Ident(String),
   Value(String),
   Arrow(Option<ScopeId>,TermId,Option<Type>,TermId),
   App(TermId,TermId),
   Let(LetTerm),
   Tuple(Vec<TermId>),
   Block(ScopeId,Vec<TermId>),
   Ascript(TermId,Type),
   As(TermId,Type),
   Constructor(String,Vec<(String,TermId)>),
   RuleApplication(TermId,String),
   Match(
      TermId,
      Vec<(TermId,TermId)>, //lhs's here don't need scopes because these bindings can't be polymorphic
   ),
   Fail, //indicates that Term does not return a Value
}

impl Term {
   pub fn equals(tlc: &TLC, lt: TermId, rt: TermId) -> bool {
      match (&tlc.rows[lt.id].term, &tlc.rows[rt.id].term) {
         (Term::Ident(li), Term::Ident(ri)) => { li == ri },
         (Term::Value(lv), Term::Value(rv)) => { lv == rv },
         (Term::Arrow(_ls,lp,lr,lb), Term::Arrow(_rs,rp,rr,rb)) => {
            Term::equals(tlc, *lp, *rp) &&
            lr == rr &&
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
   fn scope_of_lhs_impl(tlc: &mut TLC, children: &mut Vec<(String,HashMap<Type,Kind>,Type,Option<TermId>)>, lhs: TermId) {
      match &tlc.rows[lhs.id].term.clone() {
         Term::Ident(n) if n=="_" => {},
         Term::Ident(n) => {
            children.push((n.clone(), HashMap::new(), tlc.rows[lhs.id].typ.clone(), None));
         },
         Term::Ascript(lt,ltt) => {
            tlc.rows[lt.id].typ = ltt.clone();
            tlc.rows[lhs.id].typ = ltt.clone();
            Term::scope_of_lhs_impl(tlc, children, *lt);
         },
         _ => unimplemented!("destructure lhs in Term::scope_of_lhs({})", tlc.print_term(lhs)),
      }
   }
   pub fn scope_of_lhs(tlc: &mut TLC, scope: Option<ScopeId>, lhs: TermId, span: &Span) -> ScopeId {
      let mut children = Vec::new();
      Term::scope_of_lhs_impl(tlc, &mut children, lhs);
      let sid = tlc.push_scope(Scope {
         parent: scope,
         children: children,
      }, &span);
      sid
   }
   pub fn reduce_lhs(tlc: &TLC, scope_constants: &mut HashMap<String,Constant>, lhs: TermId, dc: &Constant) -> bool {
      match &tlc.rows[lhs.id].term {
         Term::Ident(n) => {
            if n != "_" {
               scope_constants.insert(n.clone(), dc.clone());
            };
            true
         },
         Term::Value(lv) => {
            if let Some(lc) = Constant::parse(tlc, lv) {
               &lc == dc
            } else { false }
         },
         _ => unimplemented!("Term::reduce_lhs({})", tlc.print_term(lhs))
      }
   }
   pub fn reduce(tlc: &TLC, scope: &Option<ScopeId>, scope_constants: &HashMap<String,Constant>, term: TermId) -> Option<Constant> {
      //scope is only used to look up functions
      //all other variables should already be converted to values
      match &tlc.rows[term.id].term {
         Term::Ident(n) => {
            if let Some(nv) = scope_constants.get(n) {
               Some(nv.clone())
            } else { panic!("Term::reduce free variable: {}", n) }
         },
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
                           if lb.parameters.len() != 1 { unimplemented!("Term::reduce, beta-reduce curried functions") }
                           let mut new_scope = scope_constants.clone();
                           let ref pars = lb.parameters[0];
                           let args = if pars.len()==1 { vec![xc] }
                                 else if let Constant::Tuple(xs) = xc { xs.clone() }
                                 else { vec![xc] };
                           if pars.len() != args.len() { panic!("Term::reduce, mismatched arity {}", tlc.print_term(term)) };
                           for ((pn,_pt,_pk),a) in std::iter::zip(pars,args) {
                              if let Some(pn) = pn {
                                 new_scope.insert(pn.clone(), a.clone());
                              }
                           }
                           if let Some(body) = lb.body {
                              Term::reduce(tlc, &Some(lb.scope), &new_scope, body)
                           } else { panic!("Term::reduce, applied function has no body: {}", gv) }
                        } else {
                           panic!("Term::reduce, unexpected lambda format in beta-reduction {}", tlc.print_term(binding))
                        }
                     } else { panic!("Term::reduce, failed to lookup function {}", gv) }
                  },
                  _ => unimplemented!("Term::reduce, implement Call-by-Value function call: {}({:?})", tlc.print_term(*g), xc)
               }
            } else { return None; }
         },
         Term::Match(dv,lrs) => {
            //These panics are OK, because the type-checker should disprove them
            if let Some(ref dc) = Constant::eval(tlc, scope_constants, *dv) {
               for (l,r) in lrs.iter() {
                  let mut sc = scope_constants.clone();
                  if Term::reduce_lhs(tlc, &mut sc, *l, dc) {
                     if tlc.fails(*r) {
                        panic!("Term::reduce match failed on {:?} at {:?}", tlc.print_term(*l), &tlc.rows[r.id].span)
                     }
                     return Term::reduce(tlc, scope, &sc, *r);
                  }
               }
               panic!("Term::reduce match failed at {:?}", &tlc.rows[term.id].span)
            } else { None }
         },
         _ => unimplemented!("Term::reduce, implement Call-by-Value term reduction: {}", tlc.print_term(term))
      }
   }
}
