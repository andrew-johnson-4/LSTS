use crate::typ::Type;
use crate::kind::Kind;
use crate::scope::{Scope,ScopeId};
use crate::tlc::TLC;
use crate::constant::Constant;
use crate::token::{Span};
use crate::debug::{Error};
use std::collections::HashMap;
use std::iter::FromIterator;

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

#[derive(Clone)]
pub enum Literal {
   Var(String),
   Char(char,String),
   String(String,String),
   Range(Vec<(char,char)>,String),
}
impl std::fmt::Debug for Literal {
   fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
      match self {
         Literal::Var(v)          => write!(f, "{}", v),
         Literal::Char(c,v)       => write!(f, "'{}'{}", c, v),
         Literal::String(s,v)     => write!(f, r#""{}"{}"#, s, v),
         Literal::Range(_r,v)     => write!(f, "[?]{}", v),
      }
   }
}

//does not implement Clone because terms are uniquely identified by their id
#[derive(Clone)] //clone seems to be needed to deconflict mutable borrows :(
pub enum Term {
   Ident(String),
   Value(String),
   Project(Constant),
   DynProject(TermId,TermId),
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
   Literal(Vec<Literal>),
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
               if let Some(prev) = scope_constants.get(n) {
                  return prev == dc;
               } else {
                  scope_constants.insert(n.clone(), dc.clone());
               }
            };
            true
         },
         Term::Tuple(ts) => {
            if let Constant::Tuple(cs) = dc {
            if ts.len() == cs.len() {
               for (lt,rc) in std::iter::zip(ts,cs) {
                  if !Term::reduce_lhs(tlc, scope_constants, *lt, rc) {
                     return false;
                  }
               }
               return true;
            }}
            false
         },
         Term::Value(lv) => {
            if let Some(lc) = Constant::parse(tlc, lv) {
               &lc == dc
            } else { false }
         },
         Term::Constructor(cname,cs) if cs.len()==0 => {
            if let Some(lc) = Constant::parse(tlc, cname) {
               &lc == dc
            } else { false }
         },
         Term::Literal(lps) => {
            if let Constant::Literal(dlp) = dc {
               let mut pre_lhs: Vec<Literal> = Vec::new();
               let mut v_lhs = "".to_string();
               let mut suf_lhs: Vec<Literal> = Vec::new();
               for li in 0..lps.len() {
                  match &lps[li] {
                     Literal::Char(_,_) | Literal::String(_,_) | Literal::Range(_,_) => {
                        if v_lhs=="" { pre_lhs.push(lps[li].clone()) }
                        else { suf_lhs.push(lps[li].clone()) }
                     },
                     Literal::Var(v) => {
                        if v_lhs=="" { v_lhs = v.clone(); }
                        else { panic!("Term::reduce_lhs({}) has two variables in the middle", tlc.print_term(lhs)) }
                     },
                  }
               }
               let mut dlp = dlp.chars().collect::<Vec<char>>();
               for p in pre_lhs.iter() {
               match p {
                  Literal::Char(pc,pv) => {
                     if dlp.len()==0 { return false; }
                     if &dlp[0] != pc { return false; }
                     let s = dlp.remove(0).to_string();
                     scope_constants.insert(pv.clone(), Constant::Literal(s));
                  },
                  Literal::String(ps,pv) => {
                     if dlp.len()<ps.len() { return false; }
                     for pc in ps.chars() {
                        if dlp.remove(0) != pc { return false; }
                     }
                     scope_constants.insert(pv.clone(), Constant::Literal(ps.clone()));
                  },
                  Literal::Range(pr,pv) => {
                     if dlp.len()==0 { return false; }
                     let mut matched = false;
                     for (low,high) in pr.iter() {
                     if *low<=dlp[0] && dlp[0]<=*high {
                        matched = true; break;
                     }}
                     if !matched { return false; }
                     let s = dlp.remove(0).to_string();
                     scope_constants.insert(pv.clone(), Constant::Literal(s));
                  },
                  Literal::Var(_) => panic!("Term::reduce prefix somehow got a Var: {}", tlc.print_term(lhs)),
               }}
               for p in suf_lhs.iter() {
               match p {
                  Literal::Char(pc,pv) => {
                     if dlp.len()==0 { return false; }
                     if &dlp[dlp.len()-1] != pc { return false; }
                     let s = dlp.remove(dlp.len()-1).to_string();
                     scope_constants.insert(pv.clone(), Constant::Literal(s));
                  },
                  Literal::String(ps,pv) => {
                     if dlp.len()<ps.len() { return false; }
                     for pc in ps.chars().rev() {
                        if dlp.remove(dlp.len()-1) != pc { return false; }
                     }
                     scope_constants.insert(pv.clone(), Constant::Literal(ps.clone()));
                  },
                  Literal::Range(pr,pv) => {
                     if dlp.len()==0 { return false; }
                     let mut matched = false;
                     for (low,high) in pr.iter() {
                     if *low<=dlp[dlp.len()-1] && dlp[dlp.len()-1]<=*high {
                        matched = true; break;
                     }}
                     if !matched { return false; }
                     let s = dlp.remove(dlp.len()-1).to_string();
                     scope_constants.insert(pv.clone(), Constant::Literal(s));
                  },
                  Literal::Var(_) => panic!("Term::reduce suffix somehow got a Var: {}", tlc.print_term(lhs)),
               }}
               if v_lhs != "" {
                  if dlp.len()==0 { return false; }
                  if v_lhs == "_" { return true; }
                  let vlv = Constant::Literal(String::from_iter(dlp));
                  if let Some(prev) = scope_constants.get(&v_lhs) {
                     return prev == &vlv;
                  } else {
                     scope_constants.insert(v_lhs.clone(), vlv);
                  }
               } else {
                  if dlp.len()!=0 { return false; }
               }
               true
            } else { panic!("Term::reduce_lhs({}) tried to match a literal of a non-literal: {:?}", tlc.print_term(lhs), dc) }
         }
         _ => unimplemented!("Term::reduce_lhs({})", tlc.print_term(lhs))
      }
   }
   pub fn check_hard_cast(tlc: &TLC, c: &Constant, ct: &Type, t: TermId) -> Result<(),Error> {
      if let Constant::Literal(cl) = c {
         let mut tried = false;
         for (rt, rgx) in tlc.regexes.iter() {
         if ct == rt {
            tried = true;
            if !rgx.is_match(&cl) {
               return Err(Error {
                  kind: "Runtime Error".to_string(),
                  rule: format!("Term::reduce Value {:?} did not match regex for Type: {:?}", cl, ct),
                  span: tlc.rows[t.id].span.clone(),
               })
            }
         }}
         if !tried {
            panic!("Term::reduce could not find regex for Type: {:?} at {:?}", ct, &tlc.rows[t.id].span);
         }
      } else {
         //TODO Term::reduce, dynamically check hard cast with gradual typing?
      }
      Ok(())
   }
   pub fn reduce(tlc: &TLC, scope: &Option<ScopeId>, scope_constants: &HashMap<String,Constant>, term: TermId) -> Result<Constant,Error> {
      //scope is only used to look up functions
      //all other variables should already be converted to values
      match &tlc.rows[term.id].term {
         Term::Ascript(t,tt) => {
            let c = Term::reduce(tlc, scope, scope_constants, *t)?;
            Term::check_hard_cast(tlc, &c, tt, term)?;
            Ok(c)
         },
         Term::As(t,tt) => {
            let c = Term::reduce(tlc, scope, scope_constants, *t)?;
            Term::check_hard_cast(tlc, &c, tt, term)?;
            Ok(c)
         },
         Term::Ident(n) => {
            if let Some(nv) = scope_constants.get(n) {
               Ok(nv.clone())
            } else { panic!("Term::reduce free variable: {} at {:?}", n, &tlc.rows[term.id].span) }
         },
         Term::Value(v) => {
            Ok(Constant::parse(tlc, &v).unwrap())
         },
         Term::Constructor(c,cps) if cps.len()==0 => {
            Ok(Constant::parse(tlc, &c).unwrap())
         },
         Term::Tuple(ts) => {
            let mut cs = Vec::new();
            for ct in ts.iter() {
               let cc = Term::reduce(tlc, scope, scope_constants, *ct)?;
               cs.push(cc);
            }
            Ok(Constant::Tuple(cs))
         },
         Term::Literal(lps) => {
            let mut v = "".to_string();
            for lp in lps.iter() {
            match lp {
               Literal::Char(lc,_) => { v += &lc.to_string(); },
               Literal::String(ls,_) => { v += ls; },
               Literal::Range(_,_) => { panic!("Term::Reduce(Term::Literal) is literal range at {:?}", tlc.rows[term.id].span) }, //not a Literal Value
               Literal::Var(lv) => {
                  if let Some(Constant::Literal(ls)) = scope_constants.get(lv) {
                     v += ls;
                  } else { panic!("Term::reduce free variable in literal {} at {:?}", lv, &tlc.rows[term.id].span) }
               },
            }}
            let cl = Constant::Literal(v);
            Term::check_hard_cast(tlc, &cl, &tlc.rows[term.id].typ, term)?;
            Ok(cl)
         },
         Term::DynProject(tb,ti) => {
            let tb = Term::reduce(tlc, scope, scope_constants, *tb)?;
            let ti = Term::reduce(tlc, scope, scope_constants, *ti)?;
            if let (Constant::Tuple(tb),Constant::Literal(ti)) = (tb,&ti) {
            if let Ok(ti) = str::parse::<usize>(ti) {
            if ti < tb.len() {
               return Ok(tb[ti].clone())
            }}}
            return Err(Error {
               kind: "Runtime Error".to_string(),
               rule: format!("Term::reduce index out of bounds: {:?}", ti),
               span: tlc.rows[term.id].span.clone(),
            })
         },
         Term::App(g,x) => {
            let xc = Term::reduce(tlc, scope, scope_constants, *x)?;
            if let Term::Project(Constant::Literal(pi)) = tlc.rows[g.id].term.clone() {
            if let Constant::Tuple(xct) = xc {
               let pi = str::parse::<usize>(&pi).unwrap();
               return Ok(xct[pi].clone());
            }}
            if let Term::Ident(gi) = tlc.rows[g.id].term.clone() {
            if gi == ".length" {
            if let Constant::Tuple(xct) = &xc {
               return Ok(Constant::Literal(format!("{}",xct.len())));
            }}}
            let sc = if let Some(sc) = scope { *sc } else { panic!("Term::reduce, function application has no scope at {:?}", &tlc.rows[term.id].span) };
            match &tlc.rows[g.id].term {
               Term::Ident(gv) => {
                  if let Some(binding) = Scope::lookup_term(tlc, sc, gv, &tlc.rows[g.id].typ) {
                     if let Term::Let(lb) = &tlc.rows[binding.id].term {
                        if lb.parameters.len() != 1 { unimplemented!("Term::reduce, beta-reduce curried functions") }
                        let mut new_scope = HashMap::new();
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
                        } else { panic!("Term::reduce, applied function has no body: {} at {:?}", gv, &tlc.rows[binding.id].span) }
                     } else {
                        panic!("Term::reduce, unexpected lambda format in beta-reduction {}", tlc.print_term(binding))
                     }
                  } else { panic!("Term::reduce, failed to lookup function {}: {:?}", gv, &tlc.rows[x.id].typ) }
               },
               _ => unimplemented!("Term::reduce, implement Call-by-Value function call: {}({:?})", tlc.print_term(*g), xc)
            }
         },
         Term::Match(dv,lrs) => {
            //These panics are OK, because the type-checker should disprove them
            let dc = Term::reduce(tlc, scope, scope_constants, *dv)?;
            for (l,r) in lrs.iter() {
               let mut sc = scope_constants.clone();
               if Term::reduce_lhs(tlc, &mut sc, *l, &dc) {
                  if tlc.fails(*r) {
                     return Err(Error {
                        kind: "Runtime Error".to_string(),
                        rule: format!("Term::reduce match failed on {:?}={:?}", tlc.print_term(*l), dc),
                        span: tlc.rows[r.id].span.clone(),
                     })
                  }
                  return Term::reduce(tlc, scope, &sc, *r);
               }
            }
            return Err(Error {
               kind: "Runtime Error".to_string(),
               rule: format!("Term::reduce match failed on default={:?}", dc),
               span: tlc.rows[term.id].span.clone(),
            })
         },
         _ => unimplemented!("Term::reduce, implement Call-by-Value term reduction: {}", tlc.print_term(term))
      }
   }
}
