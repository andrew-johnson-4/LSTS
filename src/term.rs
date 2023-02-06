use crate::typ::Type;
use crate::kind::Kind;
use crate::scope::{Scope,ScopeId};
use crate::tlc::TLC;
use crate::constant::Constant;
use crate::debug::{Error};
use crate::token::{Span};
use std::collections::HashMap;
use std::iter::FromIterator;
use l1_ir::value::Value;
use l1_ir::opt::{JProgram};
use l1_ir::ast::{Expression,Program};

#[derive(Clone,Copy,Eq,PartialEq,Ord,PartialOrd,Hash)]
pub struct TermId {
   pub id: usize,
}

#[derive(Clone)]
pub struct LetTerm {
   pub is_extern: bool,
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
      Vec<(ScopeId,TermId,TermId)>, //lhs's here don't need scopes because these bindings can't be polymorphic
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
   pub fn scope_of_lhs(tlc: &mut TLC, scope: Option<ScopeId>, lhs: TermId) -> ScopeId {
      let mut children = Vec::new();
      Term::scope_of_lhs_impl(tlc, &mut children, lhs);
      let sid = tlc.push_scope(Scope {
         parent: scope,
         children: children,
      });
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
         Term::App(g,x) => {
            if let Constant::Tuple(cts) = dc {
            if let Term::Ident(gn) = &tlc.rows[g.id].term {
            if gn == "pos" {
            if let Term::Tuple(x) = &tlc.rows[x.id].term {
            if x.len()==1 {
            if let Term::Tuple(vx) = &tlc.rows[x[0].id].term {
               let mut prefix = None;
               let mut midfix = None;
               let mut suffix = None;
               let mut accept = true;
               for vxt in vx.iter() {
               match tlc.rows[vxt.id].term.clone() {
                  Term::Tuple(fix) => {
                     if midfix.is_none() && prefix.is_none() { prefix = Some((vxt, fix.clone())); }
                     else if suffix.is_none() { suffix = Some((vxt, fix.clone())); }
                     else { accept = false; }
                  },
                  Term::Ident(fix) => {
                     if midfix.is_none() { midfix = Some((vxt, fix)); }
                     else { accept = false; }
                  },
                  _ => { accept = false; }
               }}
               if accept && (prefix.is_some() || midfix.is_some() || suffix.is_some()) {
                  let mut cts = cts.clone();
                  if let Some((pret,prevs)) = prefix {
                  if prevs.len() <= cts.len() {
                     let prects = cts[..prevs.len()].to_vec();
                     cts = cts[prevs.len()..].to_vec();
                     if !Term::reduce_lhs(tlc, scope_constants, *pret, &Constant::Tuple(prects.clone())) {
                        return false;
                     }
                  }}
                  if let Some((suft,sufvs)) = suffix {
                  if sufvs.len() <= cts.len() {
                     let sufcts = cts[(cts.len()-sufvs.len())..].to_vec();
                     cts = cts[..(cts.len()-sufvs.len())].to_vec();
                     if !Term::reduce_lhs(tlc, scope_constants, *suft, &Constant::Tuple(sufcts.clone())) {
                        return false;
                     }
                  }}
                  if let Some((midt,_midvs)) = midfix {
                     if !Term::reduce_lhs(tlc, scope_constants, *midt, &Constant::Tuple(cts.clone())) {
                        return false;
                     }
                  }
                  return true;
               }
            }}}}}}
            false
         }
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
         let cts = ct.all_named(); 
         for ct in cts.iter() {
            for (rt, rgx) in tlc.regexes.iter() {
            if ct == rt {
               if !rgx.is_match(&cl) {
                  return Err(Error {
                     kind: "Runtime Error".to_string(),
                     rule: format!("Term::reduce Value {:?} did not match regex for Type: {:?}", cl, ct),
                     span: tlc.rows[t.id].span.clone(),
                  })
               }
            }}
         }
      } else {
         //TODO Term::reduce, dynamically check hard cast with gradual typing?
      }
      Ok(())
   }
   pub fn compile_expr(tlc: &TLC, scope: &Option<ScopeId>, preamble: &mut Vec<Expression<Span>>, term: TermId) -> Result<Expression<Span>,Error> {
      match &tlc.rows[term.id].term {
         Term::Let(_) => {
            Ok(Expression::unit(tlc.rows[term.id].span.clone()))
         },
         Term::Tuple(ts) if ts.len()==0 => {
            Ok(Expression::unit(tlc.rows[term.id].span.clone()))
         },
         Term::Value(v) => {
            Ok(Expression::literal(&v, tlc.rows[term.id].span.clone()))
         },
         Term::Ascript(t,tt) => {
            let e = Term::compile_expr(tlc, scope, preamble, *t)?;
            let dt = tt.datatype();
            println!("nominal type {:?} => {}", tt, dt);
            Ok(e.typed( &tt.datatype() ))
         },
         Term::Block(sc,es) => {
            if es.len()==0 {
               Ok(Expression::unit(tlc.rows[term.id].span.clone()))
            } else {
               for ei in 0..(es.len()-1) {
                  let pe = Term::compile_expr(tlc, &Some(*sc), preamble, es[ei])?;
                  preamble.push(pe);
               }
               Term::compile_expr(tlc, &Some(*sc), preamble, es[es.len()-1])
            }
         },
         Term::App(g,x) => {
            let sc = if let Some(sc) = scope { *sc } else { panic!("Term::reduce, function application has no scope at {:?}", &tlc.rows[term.id].span) };
            match (&tlc.rows[g.id].term,&tlc.rows[x.id].term) {
               (Term::Ident(gv),Term::Tuple(ps)) => {
                  let mut args = Vec::new();
                  for p in ps.iter() {
                     args.push(Term::compile_expr(tlc, scope, preamble, *p)?);
                  }
                  if let Some(binding) = Scope::lookup_term(tlc, sc, gv, &tlc.rows[g.id].typ) {
                     if let Term::Let(lb) = &tlc.rows[binding.id].term {
                        if lb.parameters.len() > 1 { unimplemented!("Term::reduce, beta-reduce curried functions") }
                        if lb.is_extern {
                           let body = lb.body.expect(&format!("extern function body must be a mangled symbol: {}", gv));
                           if let Term::Ident(mangled) = &tlc.rows[body.id].term {
                              Ok(Expression::apply(&mangled, args, tlc.rows[term.id].span.clone()))
                           } else { unreachable!("extern function body must be a mangled symbol: {}", gv) }
                        } else {
                           unimplemented!("Term::reduce apply referenced function: {}", gv)
                        }
                     } else {
                        panic!("Term::reduce, unexpected lambda format in beta-reduction {}", tlc.print_term(binding))
                     }
                  } else { panic!("Term::reduce, failed to lookup function {}: {:?}", gv, &tlc.rows[x.id].typ) }
               },
               _ => unimplemented!("Term::reduce, implement Call-by-Value function call: {}({})", tlc.print_term(*g), tlc.print_term(*x))
            }
         },
         _ => unimplemented!("Term::compile_expr {}", tlc.print_term(term)),
      }
   }
   pub fn reduce(tlc: &TLC, scope: &Option<ScopeId>, term: TermId) -> Result<Constant,Error> {
      let mut preamble = Vec::new();
      let pe = Term::compile_expr(tlc, scope, &mut preamble, term)?;
      preamble.push(pe);

      let nojit = Program::program(
         vec![],
         preamble,
      );
      println!("debug program");
      let jit = JProgram::compile(&nojit);
      let jval = jit.eval(&[Value::u64(321,"U64")]);

      Ok(Constant::from_value(
         jval
      ))
      /*
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
         Term::Constructor(c,cps) if cps.len()==0 => {
            Ok(Constant::parse(tlc, &c).unwrap())
         },
         Term::Tuple(ts) => {
            let mut cs = Vec::new();
            for ct in ts.iter() {
               let cc = Term::reduce(tlc, scope, scope_constants, *ct)?;
               Term::check_hard_cast(tlc, &cc, &tlc.rows[ct.id].typ, *ct)?;
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
         Term::Match(dv,lrs) => {
            //These panics are OK, because the type-checker should disprove them
            let dc = Term::reduce(tlc, scope, scope_constants, *dv)?;
            for (lrc,l,r) in lrs.iter() {
               let mut sc = scope_constants.clone();
               if Term::reduce_lhs(tlc, &mut sc, *l, &dc) {
                  if tlc.fails(*r) {
                     return Err(Error {
                        kind: "Runtime Error".to_string(),
                        rule: format!("Term::reduce match failed on {:?}={:?}", tlc.print_term(*l), dc),
                        span: tlc.rows[r.id].span.clone(),
                     })
                  }
                  return Term::reduce(tlc, &Some(*lrc), &sc, *r);
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
      */
   }
}
