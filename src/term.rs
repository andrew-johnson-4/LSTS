use crate::typ::Type;
use crate::kind::Kind;
use crate::scope::{Scope,ScopeId};
use crate::tlc::TLC;
use crate::constant::Constant;
use crate::debug::{Error};
use crate::token::{Span};
use std::collections::HashMap;
use lambda_mountain::*;

#[derive(Clone,Copy,Eq,PartialEq,Ord,PartialOrd,Hash)]
pub struct TermId {
   pub id: usize,
}

#[derive(Clone)]
pub struct LetTerm {
   pub is_extern: bool,
   pub scope: ScopeId,
   pub name: String,
   pub parameters: Vec<Vec<(String,Type,Kind)>>,
   pub body: Option<TermId>,
   pub rtype: Type,
   pub rkind: Kind,
}
impl LetTerm {
   pub fn typeof_binding(&self) -> Type {
      let mut rtype = self.rtype.clone();
      for curr in self.parameters.iter().rev() {
         let mut ps = Vec::new();
         for (_,p,_) in curr.iter() {
            ps.push(p.clone());
         }
         rtype = Type::Arrow(
            Box::new(Type::Tuple(ps)),
            Box::new(rtype)
         );
      }
      rtype
   }
}

//does not implement Clone because terms are uniquely identified by their id
#[derive(Clone)] //clone seems to be needed to deconflict mutable borrows :(
pub enum Term {
   Ident(String),
   Value(String),
   Project(Constant),
   Arrow(ScopeId,TermId,Option<Type>,TermId),
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
   pub fn scope_of_lhs_impl(tlc: &mut TLC, children: &mut Vec<(String,HashMap<Type,Kind>,Type,Option<TermId>)>, lhs: TermId) {
      match &tlc.rows[lhs.id].term.clone() {
         Term::Ident(n) if n=="_" => {},
         Term::Ident(n) => {
            if tlc.rows[lhs.id].typ == Type::Any {
               tlc.rows[lhs.id].typ = Type::Named("I64".to_string(),vec![]);
            }
            children.push((n.clone(), HashMap::new(), tlc.rows[lhs.id].typ.clone(), Some(lhs)));
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
   pub fn compile_lhs(tlc: &TLC, scope: ScopeId, term: TermId) -> Result<Rhs,Error> {
      match &tlc.rows[term.id].term {
         Term::Value(lv) => {
            Ok(Rhs::Literal(lv.clone()))
         },
         Term::Ident(ln) => {
            Ok(Rhs::Variable(ln.clone()))
         },
         Term::Ascript(t,_tt) => {
            Term::compile_lhs(tlc, scope, *t)
         },
         Term::Constructor(cname,cts) if cname=="False" && cts.len()==0 => {
            Ok(Rhs::Literal("0".to_string()))
         },
         Term::Constructor(cname,cts) if cname=="True" && cts.len()==0 => {
            Ok(Rhs::Literal("1".to_string()))
         },
         Term::Constructor(cname,cts) => {
            if cts.len()==0 {
               return Ok(Rhs::Literal(cname.clone()));
            }
            let mut cas = vec![Rhs::Literal(cname.clone())];
            for (_fieldname,ct) in cts {
               cas.push(Term::compile_lhs(tlc, scope, *ct)?);
            }
            Ok(Rhs::App(cas))
         },
         _ => unimplemented!("compile_lhs: {}", tlc.print_term(term))
      }
   }
   pub fn compile_function(tlc: &TLC, _scope: &Option<ScopeId>, funcs: &mut Vec<(String,Rhs)>, term: TermId) -> Result<String,Error> {
      let mangled = if let Term::Let(ref lt) = tlc.rows[term.id].term {
         let mut name = lt.name.clone();
         name += ":";
         for ps in lt.parameters.iter() {
            name += "(";
            for (ai,args) in ps.iter().enumerate() {
               let at = args.1.clone();
               if ai > 0 { name += ","; }
               name += &format!("{:?}", at);
            }
            name += ")->";
         }
         name += &format!("{:?}", lt.rtype);
         name
      } else { panic!("Term::compile_function must be a Let binding") };
      for fd in funcs.iter() {
         if fd.0 == mangled { return Ok(mangled); }
      }
      let mut lhs = Vec::new();
      if let Term::Let(ref lt) = tlc.rows[term.id].term {
         if lt.parameters.len()==0 { unimplemented!("Term::compile_function valued let binding") }
         if lt.parameters.len()>1 { unimplemented!("Term::compile_function curried let binding") }
         for l in lt.parameters.iter() {
            for args in l.iter() {
               let name = args.0.clone();
               let typ = args.1.clone();
               let _term = Scope::lookup_term(tlc, lt.scope, &name, &typ).expect("Term::compile_function parameter not found in scope");
               lhs.push( Rhs::Variable(name) );
            }
         }
      }
      funcs.push((mangled.clone(), Rhs::App(Vec::new())));
      let mut rhs = Vec::new();
      if let Term::Let(ref lt) = tlc.rows[term.id].term {
      if let Some(body) = lt.body {
         let mut preamble = Vec::new();
         let ret = Term::compile_expr(tlc, &Some(lt.scope), funcs, &mut preamble, body)?;
         rhs.push(ret);
      }}
      for ref mut fd in funcs.iter_mut() {
      if fd.0 == mangled {
         fd.1 = Rhs::Lambda(lhs, rhs);
         break;
      }}
      Ok(mangled)
   }
   pub fn apply_fn(tlc: &TLC, scope: &Option<ScopeId>, funcs: &mut Vec<(String,Rhs)>,
                   preamble: &mut Vec<Rhs>, f: &str, ps: &Vec<TermId>,
                   ft: Type, _span: Span) -> Result<Rhs,Error> {
      let sc = if let Some(sc) = scope { *sc } else { panic!("Term::apply_fn, function application has no scope at {}", f) };
      let mut args = Vec::new();
      for p in ps.iter() {
         args.push(Term::compile_expr(tlc, scope, funcs, preamble, *p)?);
      }
      if let Some(binding) = Scope::lookup_term(tlc, sc, f, &ft) {
         if let Term::Let(lb) = &tlc.rows[binding.id].term {
            if lb.parameters.len() > 1 { unimplemented!("Term::reduce, beta-reduce curried functions") }
            let bt = lb.typeof_binding();
            if lb.is_extern {
               let body = lb.body.expect(&format!("extern function body must be a mangled symbol: {}", f));
               if let Term::Ident(mangled) = &tlc.rows[body.id].term {
                  args.insert(0, Rhs::Variable(mangled.clone()));
                  Ok(Rhs::App(args))
               } else { unreachable!("extern function body must be a mangled symbol: {}", f) }
            } else if bt.is_open() {
               let Some(lbt) = tlc.poly_bindings.get(&(lb.name.clone(),ft.clone()))
               else { unreachable!("could not find template function {}: {:?}", lb.name, bt) };
               let Term::Let(_lbb) = &tlc.rows[lbt.id].term
               else { unreachable!("template function must be let binding {}: {:?}", lb.name, bt) };
               let mangled = Term::compile_function(tlc, scope, funcs, *lbt)?;
               args.insert(0, Rhs::Variable(mangled.clone()));
               Ok(Rhs::App(args))
            } else {
               let mangled = Term::compile_function(tlc, scope, funcs, binding)?;
               args.insert(0, Rhs::Variable(mangled.clone()));
               Ok(Rhs::App(args))
            }
         } else {
            panic!("Term::reduce, unexpected lambda format in beta-reduction {}", tlc.print_term(binding))
         }
      } else { panic!("Term::reduce, failed to lookup function {}: {:?}", f, &ft) }
   }
   pub fn mangle_fn(tlc: &TLC, scope: &Option<ScopeId>, funcs: &mut Vec<(String,Rhs)>,
                    preamble: &mut Vec<Rhs>, g: TermId, x: TermId) -> Result<Rhs,Error> {
      let span = tlc.rows[g.id].span.clone();
      if let Term::Ident(gn) = &tlc.rows[g.id].term {
         let gt = tlc.rows[g.id].typ.clone();
         if let Term::Tuple(ts) = &tlc.rows[x.id].term {
            Term::apply_fn(tlc, scope, funcs, preamble, gn, ts, gt, span)
         } else {
            Term::apply_fn(tlc, scope, funcs, preamble, gn, &vec![x], gt, span)
         }
      } else {
         Term::compile_expr(tlc, scope, funcs, preamble, g)
      }
   }
   pub fn compile_expr(tlc: &TLC, scope: &Option<ScopeId>, funcs: &mut Vec<(String,Rhs)>,
                       preamble: &mut Vec<Rhs>, term: TermId) -> Result<Rhs,Error> {
      let span = tlc.rows[term.id].span.clone();
      match &tlc.rows[term.id].term {
         Term::Let(_) => {
            Ok(Rhs::App(Vec::new()))
         },
         Term::Tuple(ts) => {
            let mut tes = Vec::new();
            for te in ts.iter() {
               let te = Term::compile_expr(tlc, scope, funcs, preamble, *te)?;
               tes.push(te);
            }
            Ok(Rhs::App(tes))
         },
         Term::Constructor(c,cs) if c=="False" && cs.len()==0 => {
            Ok(Rhs::Literal("0".to_string()))
         },
         Term::Constructor(c,cs) if c=="True" && cs.len()==0 => {
            Ok(Rhs::Literal("1".to_string()))
         },
         Term::Constructor(c,cs) if cs.len()==0 => {
            Ok(Rhs::Literal(c.to_string()))
         },
         Term::Value(v) => {
            Ok(Rhs::Literal(v.to_string()))
         },
         Term::Ident(n) => {
            Ok(Rhs::Variable(n.clone()))
         },
         Term::Ascript(t,_tt) => {
            //TODO gradual type
            Term::compile_expr(tlc, scope, funcs, preamble, *t)
         },
         Term::As(t,tt) => {
            let bt = tlc.rows[t.id].typ.clone();
            if !Type::implies(tlc, &bt, tt).is_bottom() {
               Term::compile_expr(tlc, scope, funcs, preamble, *t)
            } else {
               let bts = Type::Tuple(vec![bt]);
               let gt = Type::Arrow(Box::new(bts), Box::new(tt.clone()));
               Term::apply_fn(tlc, scope, funcs, preamble, "as", &vec![*t], gt, span)
            }
         },
         Term::Block(sc,es) => {
            if es.len()==0 {
               Ok(Rhs::App(Vec::new()))
            } else {
               for ei in 0..(es.len()-1) {
                  let pe = Term::compile_expr(tlc, &Some(*sc), funcs, preamble, es[ei])?;
                  preamble.push(pe);
               }
               Term::compile_expr(tlc, &Some(*sc), funcs, preamble, es[es.len()-1])
            }
         },
         Term::Match(dv,lrs) => {
            //These panics are OK, because the type-checker should disprove them
            let pe = Term::compile_expr(tlc, scope, funcs, preamble, *dv)?;
            let mut plrs = Vec::new();
            for (lrc,l,r) in lrs.iter() {
               let lhs = Term::compile_lhs(tlc, *lrc, *l)?;
               let rhs = Term::compile_expr(tlc, &Some(*lrc), funcs, preamble, *r)?;
               plrs.push(Rhs::Lambda(vec![lhs],vec![rhs]));
            }
            Ok(Rhs::App(vec![
               Rhs::Variable("match".to_string()),
               pe,
               Rhs::App(plrs),
            ]))
         },
         Term::Arrow(sc,lhs,_lt,rhs) => {
            let lhs = Term::compile_lhs(tlc, *sc, *lhs)?;
            let rhs = Term::compile_expr(tlc, &Some(*sc), funcs, preamble, *rhs)?;
            Ok(Rhs::Lambda(vec![lhs], vec![rhs]))
         },
         Term::App(gt,xt) => {
            let x = Term::compile_expr(tlc, scope, funcs, preamble, *xt)?;

            if let Term::Project(Constant::Literal(cv)) = &tlc.rows[gt.id].term {
               return Ok(Rhs::App(vec![
                  Rhs::Variable("π".to_string()),
                  Rhs::Literal(cv.clone()),
                  x
               ]));
            }
            Term::mangle_fn(tlc, scope, funcs, preamble, *gt, *xt)
            /*
            match (&tlc.rows[g.id].term,&tlc.rows[x.id].term) {
               (Term::Ident(gv),Term::Tuple(ps)) if gv==".flatmap" && ps.len()==2 => {
                  let Term::Arrow(asc,lhs,_att,rhs) = tlc.rows[ps[1].id].term.clone()
                  else { panic!(".flatmap second argument must be an arrow: {}", tlc.print_term(ps[1])) };
                  if let Term::Match(mcond,mlrs) = &tlc.rows[rhs.id].term {
                  if mlrs.len()==2 {
                  if let Term::Constructor(tname,_) = &tlc.rows[mlrs[0].1.id].term {
                  if tname == "True" {
                  if let Term::Constructor(fname,_) = &tlc.rows[mlrs[1].1.id].term {
                  if fname == "False" {
                  if let Term::Tuple(fvalue) = &tlc.rows[mlrs[1].2.id].term {
                  if fvalue.len()==0 {
                     let (_true_scope,_true_lhs,true_rhs) = mlrs[0];
                     let map_iterable = Term::compile_expr(tlc, &Some(sc), funcs, preamble, ps[0])?;
                     let map_lhs = Term::compile_lhs(tlc, asc, lhs)?;
                     let map_yield = Term::compile_expr(tlc, &Some(asc), funcs, preamble, true_rhs)?;
                     let map_guard = Term::compile_expr(tlc, &Some(asc), funcs, preamble, *mcond)?;
                     let map_ti = TIPart::expression(Expression::pattern(
                        map_guard, vec![(
                           LHSPart::literal("1"),
                           map_yield.typed("Value")
                        )]
                     ,span.clone()).typed("Value"));
                     let map = Expression::map(
                        map_lhs,
                        map_iterable.typed("Value"),
                        map_ti,
                        span.clone(),
                     ).typed("Value");
                     let flatmap = Expression::apply(".flatten:(Tuple)->Tuple", vec![map], span).typed("Value");
                     return Ok(flatmap)
                  }} }} }} }}
                  let map_iterable = Term::compile_expr(tlc, &Some(sc), funcs, preamble, ps[0])?;
                  let map_lhs = Term::compile_lhs(tlc, asc, lhs)?;
                  let map_yield = Term::compile_expr(tlc, &Some(asc), funcs, preamble, rhs)?;
                  let map = Expression::map(
                     map_lhs,
                     map_iterable.typed("Value"),
                     TIPart::expression(map_yield.typed("Value")),
                     span.clone(),
                  ).typed("Value");
                  let flatmap = Expression::apply(".flatten:(Tuple)->Tuple", vec![map], span).typed("Value");
                  Ok(flatmap)
               },
               _ => unimplemented!("Term::reduce, implement Call-by-Value function call: {}({})", tlc.print_term(*g), tlc.print_term(*x))
            }
            */
         },
         _ => unimplemented!("Term::compile_expr {}", tlc.print_term(term)),
      }
   }
   pub fn reduce(tlc: &TLC, scope: &Option<ScopeId>, term: TermId) -> Result<Constant,Error> {
      let span = tlc.rows[term.id].span.clone();

      let mut policy = Policy::new();
      policy.bind_extern("π", &pi);
      policy.bind_extern("[]:(Tuple,U64)->Value", &get_index);
      policy.bind_extern(".length:(Tuple)->U64", &dot_length);

      policy.bind_extern("range:(I64,I64,I64)->I64[]", &range);

      policy.bind_extern(".join:(String[])->String", &string_join);
      policy.bind_extern(".join:(String[],String)->String", &string_join2);

      policy.bind_extern("not:(U8)->U8", &not_u8);
      policy.bind_extern("&&:(U8,U8)->U8", &and_u8);
      policy.bind_extern("||:(U8,U8)->U8", &or_u8);

      policy.bind_extern("+:(U64,U64)->U64", &add_u64);
      policy.bind_extern("-:(U64,U64)->U64", &sub_u64);
      policy.bind_extern("*:(U64,U64)->U64", &mul_u64);
      policy.bind_extern("/:(U64,U64)->U64", &div_u64);
      policy.bind_extern("%:(U64,U64)->U64", &mod_u64);
      policy.bind_extern("==:(U64,U64)->U8", &eq_u64);
      policy.bind_extern("!=:(U64,U64)->U8", &ne_u64);
      policy.bind_extern("<:(U64,U64)->U8", &lt_u64);
      policy.bind_extern("<=:(U64,U64)->U8", &lte_u64);
      policy.bind_extern(">:(U64,U64)->U8", &gt_u64);
      policy.bind_extern(">=:(U64,U64)->U8", &gte_u64);

      policy.bind_extern("+:(I64,I64)->I64", &add_i64);
      policy.bind_extern("-:(I64,I64)->I64", &sub_i64);
      policy.bind_extern("*:(I64,I64)->I64", &mul_i64);
      policy.bind_extern("/:(I64,I64)->I64", &div_i64);
      policy.bind_extern("%:(I64,I64)->I64", &mod_i64);
      policy.bind_extern("==:(I64,I64)->U8", &eq_i64);
      policy.bind_extern("!=:(I64,I64)->U8", &ne_i64);
      policy.bind_extern("<:(I64,I64)->U8", &lt_i64);
      policy.bind_extern("<=:(I64,I64)->U8", &lte_i64);
      policy.bind_extern(">:(I64,I64)->U8", &gt_i64);
      policy.bind_extern(">=:(I64,I64)->U8", &gte_i64);
      policy.bind_extern("pos:(I64)->I64", &pos_i64);
      policy.bind_extern("neg:(I64)->I64", &neg_i64);

      let mut preamble = Vec::new();
      let mut funcs = Vec::new();
      let pe = Term::compile_expr(tlc, scope, &mut funcs, &mut preamble, term)?;
      preamble.push(pe);

      for (k,v) in funcs.iter() {
         policy.bind(k, v.clone());
      }

      let context = Context::new(&policy);
      let mut last_e = Rhs::App(Vec::new());
      for pe in preamble {
         match eval_rhs(context.clone(), &[pe]) {
            Err(e) => {
               return Err(Error {
                  kind: "Runtime".to_string(),
                  rule: format!("reduce: {}", e),
                  span: span.clone(),
               });
            }, Ok(e) => {
               last_e = e.clone();
            },
         }
      }

      Ok(Constant::from_value(last_e))
   }
}

fn pi(args: &[Rhs]) -> Rhs {
   if let [Rhs::Literal(i), Rhs::App(ts)] = args {
      let i = i.parse::<usize>().unwrap();
      return ts[i].clone();
   }
   let mut args = args.to_vec();
   args.insert(0, Rhs::Literal("π".to_string()));
   Rhs::App(args)
}

fn range(args: &[Rhs]) -> Rhs {
   if let [Rhs::Literal(x),Rhs::Literal(y),Rhs::Literal(z)] = args {
      let x = x.parse::<i64>().unwrap();
      let y = y.parse::<i64>().unwrap();
      let z = z.parse::<usize>().unwrap();
      let mut cs = Vec::new();
      for i in (x..y).step_by(z) {
         cs.push(Rhs::Literal(format!("{}",i)))
      }
      return Rhs::App(cs);
   }
   let mut args = args.to_vec();
   args.insert(0, Rhs::Literal("range:(I64,I64,I64)->I64[]".to_string()));
   Rhs::App(args)
}

fn string_join(args: &[Rhs]) -> Rhs {
   if let [Rhs::App(ts)] = args {
      let mut s = String::new();
      for t in ts {
         s.push_str(&t.to_string());
      }
      return Rhs::Literal(s)
   }
   let mut args = args.to_vec();
   args.insert(0, Rhs::Literal(".join:(String[])->String".to_string()));
   Rhs::App(args)
}
fn string_join2(args: &[Rhs]) -> Rhs {
   if let [Rhs::App(ts),Rhs::Literal(sep)] = args {
      let mut s = String::new();
      for (ti,t) in ts.iter().enumerate() {
         if ti>0 {
            s.push_str(sep);
         }
         s.push_str(&t.to_string());
      }
      return Rhs::Literal(s)
   }
   let mut args = args.to_vec();
   args.insert(0, Rhs::Literal(".join:(String[],String)->String".to_string()));
   Rhs::App(args)
}

fn get_index(args: &[Rhs]) -> Rhs {
   if let [Rhs::App(ts), Rhs::Literal(i)] = args {
      let i = i.parse::<usize>().unwrap();
      return ts[i].clone();
   }
   let mut args = args.to_vec();
   args.insert(0, Rhs::Literal("[]:(Tuple,U64)->Value".to_string()));
   Rhs::App(args)
}

fn dot_length(args: &[Rhs]) -> Rhs {
   if let [Rhs::App(ts)] = args {
      return Rhs::Literal(format!("{}",ts.len()));
   }
   let mut args = args.to_vec();
   args.insert(0, Rhs::Literal(".length:(Tuple)->U64".to_string()));
   Rhs::App(args)
}

fn add_u64(args: &[Rhs]) -> Rhs {
   if let [Rhs::Literal(x),Rhs::Literal(y)] = args {
      let x = x.parse::<u64>().unwrap();
      let y = y.parse::<u64>().unwrap();
      return Rhs::Literal(format!("{}",x+y));
   }
   let mut args = args.to_vec();
   args.insert(0, Rhs::Literal("+:(U64,U64)->U64".to_string()));
   Rhs::App(args)
}
fn sub_u64(args: &[Rhs]) -> Rhs {
   if let [Rhs::Literal(x),Rhs::Literal(y)] = args {
      let x = x.parse::<u64>().unwrap();
      let y = y.parse::<u64>().unwrap();
      return Rhs::Literal(format!("{}",x-y));
   }
   let mut args = args.to_vec();
   args.insert(0, Rhs::Literal("-:(U64,U64)->U64".to_string()));
   Rhs::App(args)
}
fn mul_u64(args: &[Rhs]) -> Rhs {
   if let [Rhs::Literal(x),Rhs::Literal(y)] = args {
      let x = x.parse::<u64>().unwrap();
      let y = y.parse::<u64>().unwrap();
      return Rhs::Literal(format!("{}",x*y));
   }
   let mut args = args.to_vec();
   args.insert(0, Rhs::Literal("*:(U64,U64)->U64".to_string()));
   Rhs::App(args)
}
fn div_u64(args: &[Rhs]) -> Rhs {
   if let [Rhs::Literal(x),Rhs::Literal(y)] = args {
      let x = x.parse::<u64>().unwrap();
      let y = y.parse::<u64>().unwrap();
      return Rhs::Literal(format!("{}",x/y));
   }
   let mut args = args.to_vec();
   args.insert(0, Rhs::Literal("/:(U64,U64)->U64".to_string()));
   Rhs::App(args)
}
fn mod_u64(args: &[Rhs]) -> Rhs {
   if let [Rhs::Literal(x),Rhs::Literal(y)] = args {
      let x = x.parse::<u64>().unwrap();
      let y = y.parse::<u64>().unwrap();
      return Rhs::Literal(format!("{}",x%y));
   }
   let mut args = args.to_vec();
   args.insert(0, Rhs::Literal("%:(U64,U64)->U64".to_string()));
   Rhs::App(args)
}
fn eq_u64(args: &[Rhs]) -> Rhs {
   if let [Rhs::Literal(x),Rhs::Literal(y)] = args {
      let x = x.parse::<u64>().unwrap();
      let y = y.parse::<u64>().unwrap();
      return Rhs::Literal(format!("{}",(x==y) as u8));
   }
   let mut args = args.to_vec();
   args.insert(0, Rhs::Literal("==:(U64,U64)->U8".to_string()));
   Rhs::App(args)
}
fn ne_u64(args: &[Rhs]) -> Rhs {
   if let [Rhs::Literal(x),Rhs::Literal(y)] = args {
      let x = x.parse::<u64>().unwrap();
      let y = y.parse::<u64>().unwrap();
      return Rhs::Literal(format!("{}",(x != y) as u8));
   }
   let mut args = args.to_vec();
   args.insert(0, Rhs::Literal("!=:(U64,U64)->U8".to_string()));
   Rhs::App(args)
}
fn lt_u64(args: &[Rhs]) -> Rhs {
   if let [Rhs::Literal(x),Rhs::Literal(y)] = args {
      let x = x.parse::<u64>().unwrap();
      let y = y.parse::<u64>().unwrap();
      return Rhs::Literal(format!("{}",(x<y) as u8));
   }
   let mut args = args.to_vec();
   args.insert(0, Rhs::Literal("<:(U64,U64)->U8".to_string()));
   Rhs::App(args)
}
fn lte_u64(args: &[Rhs]) -> Rhs {
   if let [Rhs::Literal(x),Rhs::Literal(y)] = args {
      let x = x.parse::<u64>().unwrap();
      let y = y.parse::<u64>().unwrap();
      return Rhs::Literal(format!("{}",(x<=y) as u8));
   }
   let mut args = args.to_vec();
   args.insert(0, Rhs::Literal("<=:(U64,U64)->U8".to_string()));
   Rhs::App(args)
}
fn gt_u64(args: &[Rhs]) -> Rhs {
   if let [Rhs::Literal(x),Rhs::Literal(y)] = args {
      let x = x.parse::<u64>().unwrap();
      let y = y.parse::<u64>().unwrap();
      return Rhs::Literal(format!("{}",(x>y) as u8));
   }
   let mut args = args.to_vec();
   args.insert(0, Rhs::Literal(">:(U64,U64)->U8".to_string()));
   Rhs::App(args)
}
fn gte_u64(args: &[Rhs]) -> Rhs {
   if let [Rhs::Literal(x),Rhs::Literal(y)] = args {
      let x = x.parse::<u64>().unwrap();
      let y = y.parse::<u64>().unwrap();
      return Rhs::Literal(format!("{}",(x>=y) as u8));
   }
   let mut args = args.to_vec();
   args.insert(0, Rhs::Literal(">=:(U64,U64)->U8".to_string()));
   Rhs::App(args)
}

fn add_i64(args: &[Rhs]) -> Rhs {
   if let [Rhs::Literal(x),Rhs::Literal(y)] = args {
      let x = x.parse::<i64>().unwrap();
      let y = y.parse::<i64>().unwrap();
      return Rhs::Literal(format!("{}",x+y));
   }
   let mut args = args.to_vec();
   args.insert(0, Rhs::Literal("+:(i64,i64)->i64".to_string()));
   Rhs::App(args)
}
fn sub_i64(args: &[Rhs]) -> Rhs {
   if let [Rhs::Literal(x),Rhs::Literal(y)] = args {
      let x = x.parse::<i64>().unwrap();
      let y = y.parse::<i64>().unwrap();
      return Rhs::Literal(format!("{}",x-y));
   }
   let mut args = args.to_vec();
   args.insert(0, Rhs::Literal("-:(i64,i64)->i64".to_string()));
   Rhs::App(args)
}
fn mul_i64(args: &[Rhs]) -> Rhs {
   if let [Rhs::Literal(x),Rhs::Literal(y)] = args {
      let x = x.parse::<i64>().unwrap();
      let y = y.parse::<i64>().unwrap();
      return Rhs::Literal(format!("{}",x*y));
   }
   let mut args = args.to_vec();
   args.insert(0, Rhs::Literal("*:(i64,i64)->i64".to_string()));
   Rhs::App(args)
}
fn div_i64(args: &[Rhs]) -> Rhs {
   if let [Rhs::Literal(x),Rhs::Literal(y)] = args {
      let x = x.parse::<i64>().unwrap();
      let y = y.parse::<i64>().unwrap();
      return Rhs::Literal(format!("{}",x/y));
   }
   let mut args = args.to_vec();
   args.insert(0, Rhs::Literal("/:(i64,i64)->i64".to_string()));
   Rhs::App(args)
}
fn mod_i64(args: &[Rhs]) -> Rhs {
   if let [Rhs::Literal(x),Rhs::Literal(y)] = args {
      let x = x.parse::<i64>().unwrap();
      let y = y.parse::<i64>().unwrap();
      return Rhs::Literal(format!("{}",x%y));
   }
   let mut args = args.to_vec();
   args.insert(0, Rhs::Literal("%:(i64,i64)->i64".to_string()));
   Rhs::App(args)
}
fn eq_i64(args: &[Rhs]) -> Rhs {
   if let [Rhs::Literal(x),Rhs::Literal(y)] = args {
      let x = x.parse::<i64>().unwrap();
      let y = y.parse::<i64>().unwrap();
      return Rhs::Literal(format!("{}",(x==y) as u8));
   }
   let mut args = args.to_vec();
   args.insert(0, Rhs::Literal("==:(i64,i64)->U8".to_string()));
   Rhs::App(args)
}
fn ne_i64(args: &[Rhs]) -> Rhs {
   if let [Rhs::Literal(x),Rhs::Literal(y)] = args {
      let x = x.parse::<i64>().unwrap();
      let y = y.parse::<i64>().unwrap();
      return Rhs::Literal(format!("{}",(x != y) as u8));
   }
   let mut args = args.to_vec();
   args.insert(0, Rhs::Literal("!=:(i64,i64)->U8".to_string()));
   Rhs::App(args)
}
fn lt_i64(args: &[Rhs]) -> Rhs {
   if let [Rhs::Literal(x),Rhs::Literal(y)] = args {
      let x = x.parse::<i64>().unwrap();
      let y = y.parse::<i64>().unwrap();
      return Rhs::Literal(format!("{}",(x<y) as u8));
   }
   let mut args = args.to_vec();
   args.insert(0, Rhs::Literal("<:(i64,i64)->U8".to_string()));
   Rhs::App(args)
}
fn lte_i64(args: &[Rhs]) -> Rhs {
   if let [Rhs::Literal(x),Rhs::Literal(y)] = args {
      let x = x.parse::<i64>().unwrap();
      let y = y.parse::<i64>().unwrap();
      return Rhs::Literal(format!("{}",(x<=y) as u8));
   }
   let mut args = args.to_vec();
   args.insert(0, Rhs::Literal("<=:(i64,i64)->U8".to_string()));
   Rhs::App(args)
}
fn gt_i64(args: &[Rhs]) -> Rhs {
   if let [Rhs::Literal(x),Rhs::Literal(y)] = args {
      let x = x.parse::<i64>().unwrap();
      let y = y.parse::<i64>().unwrap();
      return Rhs::Literal(format!("{}",(x>y) as u8));
   }
   let mut args = args.to_vec();
   args.insert(0, Rhs::Literal(">:(i64,i64)->U8".to_string()));
   Rhs::App(args)
}
fn gte_i64(args: &[Rhs]) -> Rhs {
   if let [Rhs::Literal(x),Rhs::Literal(y)] = args {
      let x = x.parse::<i64>().unwrap();
      let y = y.parse::<i64>().unwrap();
      return Rhs::Literal(format!("{}",(x>=y) as u8));
   }
   let mut args = args.to_vec();
   args.insert(0, Rhs::Literal(">=:(i64,i64)->U8".to_string()));
   Rhs::App(args)
}
fn pos_i64(args: &[Rhs]) -> Rhs {
   if let [Rhs::Literal(x)] = args {
      let x = x.parse::<i64>().unwrap();
      return Rhs::Literal(format!("{}",x));
   }
   let mut args = args.to_vec();
   args.insert(0, Rhs::Literal("pos:(I64)->I64".to_string()));
   Rhs::App(args)
}
fn neg_i64(args: &[Rhs]) -> Rhs {
   if let [Rhs::Literal(x)] = args {
      let x = x.parse::<i64>().unwrap();
      return Rhs::Literal(format!("{}",-x));
   }
   let mut args = args.to_vec();
   args.insert(0, Rhs::Literal("neg:(I64)->I64".to_string()));
   Rhs::App(args)
}

fn bool_as_u8(x: &str) -> u8 {
   if x == "True" {
      1
   } else if x == "False" {
      0
   } else {
      x.parse::<u8>().unwrap()
   }
}
fn not_u8(args: &[Rhs]) -> Rhs {
   if let [Rhs::Literal(x)] = args {
      let x = bool_as_u8(x);
      return Rhs::Literal(format!("{}",if x==0 {1} else {0}));
   }
   let mut args = args.to_vec();
   args.insert(0, Rhs::Literal("not:(U8)->U8".to_string()));
   Rhs::App(args)
}
fn and_u8(args: &[Rhs]) -> Rhs {
   if let [Rhs::Literal(x),Rhs::Literal(y)] = args {
      let x = bool_as_u8(x);
      let y = bool_as_u8(y);
      return Rhs::Literal(format!("{}",x & y));
   }
   let mut args = args.to_vec();
   args.insert(0, Rhs::Literal("&&:(U8,U8)->U8".to_string()));
   Rhs::App(args)
}
fn or_u8(args: &[Rhs]) -> Rhs {
   if let [Rhs::Literal(x),Rhs::Literal(y)] = args {
      let x = bool_as_u8(x);
      let y = bool_as_u8(y);
      return Rhs::Literal(format!("{}",x | y));
   }
   let mut args = args.to_vec();
   args.insert(0, Rhs::Literal("||:(U8,U8)->U8".to_string()));
   Rhs::App(args)
}
