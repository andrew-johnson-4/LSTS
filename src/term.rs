use crate::typ::Type;
use crate::kind::Kind;
use crate::scope::{Scope,ScopeId};
use crate::tlc::TLC;
use crate::constant::Constant;
use crate::debug::{Error};
use crate::token::{Span};
use std::collections::HashMap;
use l1_ir::opt::{JProgram};
use l1_ir::ast::{self,Expression,Program,FunctionDefinition,LHSPart,TIPart};

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
   pub fn compile_lhs(tlc: &TLC, scope: ScopeId, term: TermId) -> Result<LHSPart,Error> {
      let tt = tlc.rows[term.id].typ.clone();
      match &tlc.rows[term.id].term {
         Term::Value(lv) => {
            Ok(LHSPart::literal(lv))
         },
         Term::Ident(ln) => {
            if ln == "_" {
               Ok(LHSPart::any())
            } else {
               let term = Scope::lookup_term(tlc, scope, ln, &tt).expect("Term::compile_lhs identifier not found in scope");
               Ok(LHSPart::variable(term.id))
            }
         },
         Term::Ascript(t,_tt) => {
            Term::compile_lhs(tlc, scope, *t)
         },
         Term::Constructor(cname,_) if cname=="False" => {
            Ok(LHSPart::literal("0"))
         },
         Term::Constructor(cname,_) if cname=="True" => {
            Ok(LHSPart::literal("1"))
         },
         _ => unimplemented!("compile_lhs: {}", tlc.print_term(term))
      }
   }
   pub fn compile_function(tlc: &TLC, scope: &Option<ScopeId>, funcs: &mut Vec<FunctionDefinition<Span>>, term: TermId) -> Result<String,Error> {
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
      println!("mangled: {}", mangled);
      for fd in funcs.iter() {
         if fd.name == mangled { return Ok(mangled); }
      }
      let mut l1_args = Vec::new();
      if let Term::Let(ref lt) = tlc.rows[term.id].term {
         if lt.parameters.len()==0 { unimplemented!("Term::compile_function valued let binding") }
         if lt.parameters.len()>1 { unimplemented!("Term::compile_function curried let binding") }
         for args in lt.parameters[0].iter() {
            let name = args.0.clone();
            let typ = args.1.clone();
            let dt = typ.datatype();
            let term = Scope::lookup_term(tlc, lt.scope, &name, &typ).expect("Term::compile_function parameter not found in scope");
            l1_args.push(( term.id, ast::Type::nominal(&dt) ));
         }
      }
      funcs.push(FunctionDefinition::define(
         &mangled,
         l1_args,
         vec![],
      ));
      let mut preamble = Vec::new();
      if let Term::Let(ref lt) = tlc.rows[term.id].term {
      if let Some(body) = lt.body {
         let ret = Term::compile_expr(tlc, &Some(lt.scope), funcs, &mut preamble, body)?;
         preamble.push(ret);
      }}
      for ref mut fd in funcs.iter_mut() {
      if fd.name == mangled {
         fd.body = preamble; break;
      }}
      Ok(mangled)
   }
   pub fn apply_fn(tlc: &TLC, scope: &Option<ScopeId>, funcs: &mut Vec<FunctionDefinition<Span>>,
                   preamble: &mut Vec<Expression<Span>>, f: &str, ps: &Vec<TermId>,
                   ft: Type, rt: Type, span: Span) -> Result<Expression<Span>,Error> {
      let sc = if let Some(sc) = scope { *sc } else { panic!("Term::apply_fn, function application has no scope at {}", f) };
      let mut args = Vec::new();
      for p in ps.iter() {
         args.push(Term::compile_expr(tlc, scope, funcs, preamble, *p)?);
      }
      if let Some(binding) = Scope::lookup_term(tlc, sc, f, &ft) {
         if let Term::Let(lb) = &tlc.rows[binding.id].term {
            if lb.parameters.len() > 1 { unimplemented!("Term::reduce, beta-reduce curried functions") }
            let bt = lb.typeof_binding();
            if bt.is_open() {
               let Some(lbt) = tlc.poly_bindings.get(&(lb.name.clone(),ft.clone()))
               else { unreachable!("could not find template function {}: {:?}", lb.name, bt) };
               let Term::Let(lbb) = &tlc.rows[lbt.id].term
               else { unreachable!("template function must be let binding {}: {:?}", lb.name, bt) };
               let mangled = Term::compile_function(tlc, scope, funcs, *lbt)?;
               let e = Expression::apply(&mangled, args, span);
               let e = e.typed(&rt.datatype());
               Ok(e)
            } else if lb.is_extern {
               let body = lb.body.expect(&format!("extern function body must be a mangled symbol: {}", f));
               if let Term::Ident(mangled) = &tlc.rows[body.id].term {
                  let e = Expression::apply(&mangled, args, span);
                  let e = e.typed(&rt.datatype());
                  Ok(e)
               } else { unreachable!("extern function body must be a mangled symbol: {}", f) }
            } else {
               let mangled = Term::compile_function(tlc, scope, funcs, binding)?;
               let e = Expression::apply(&mangled, args, span);
               let e = e.typed(&rt.datatype());
               Ok(e)
            }
         } else {
            panic!("Term::reduce, unexpected lambda format in beta-reduction {}", tlc.print_term(binding))
         }
      } else { panic!("Term::reduce, failed to lookup function {}: {:?}", f, &ft) }
   }
   pub fn compile_expr(tlc: &TLC, scope: &Option<ScopeId>, funcs: &mut Vec<FunctionDefinition<Span>>,
                       preamble: &mut Vec<Expression<Span>>, term: TermId) -> Result<Expression<Span>,Error> {
      let tt = tlc.rows[term.id].typ.clone();
      let span = tlc.rows[term.id].span.clone();
      match &tlc.rows[term.id].term {
         Term::Let(_) => {
            Ok(Expression::unit(span))
         },
         Term::Tuple(ts) if ts.len()==0 => {
            Ok(Expression::unit(span))
         },
         Term::Tuple(ts) => {
            let mut tes = Vec::new();
            for te in ts.iter() {
               let mut te = Term::compile_expr(tlc, scope, funcs, preamble, *te)?;
               let mut type_is_value = false;
               if let Some(tet) = te.typ().name {
               if tet == "String" {
                  type_is_value = true;
               }}
               if !type_is_value {
                  te = te.typed("Value");
               }
               tes.push(te);
            }
            Ok(Expression::tuple(tes,span).typed("Value"))
         },
         Term::Value(v) => {
            let e = Expression::literal(&v, span).typed(&tt.datatype());
            Ok(e)
         },
         Term::Ident(n) => {
            let tt = tlc.rows[term.id].typ.clone();
            let span = tlc.rows[term.id].span.clone();
            let sc = scope.expect("Term::compile_expr scope was None");
            let term = Scope::lookup_term(tlc, sc, &n, &tt).expect("Term::compile_expr variable not found in scope");
            let e = Expression::variable(term.id, span).typed(&tt.datatype());
            Ok(e)
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
               Term::apply_fn(tlc, scope, funcs, preamble, "as", &vec![*t], gt, tt.clone(), span)
            }
         },
         Term::Block(sc,es) => {
            if es.len()==0 {
               Ok(Expression::unit(tlc.rows[term.id].span.clone()))
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
               plrs.push((lhs,rhs));
            }
            Ok(Expression::pattern(pe, plrs, span).typed(&tt.datatype()))
         },
         Term::App(g,x) => {
            let sc = if let Some(sc) = scope { *sc } else { panic!("Term::reduce, function application has no scope at {:?}", &tlc.rows[term.id].span) };
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
               (Term::Ident(gv),Term::Tuple(ps)) => {
                  Term::apply_fn(tlc, scope, funcs, preamble, gv, ps, tlc.rows[g.id].typ.clone(), tt, span)
               },
               _ => unimplemented!("Term::reduce, implement Call-by-Value function call: {}({})", tlc.print_term(*g), tlc.print_term(*x))
            }
         },
         _ => unimplemented!("Term::compile_expr {}", tlc.print_term(term)),
      }
   }
   pub fn reduce(tlc: &TLC, scope: &Option<ScopeId>, term: TermId) -> Result<Constant,Error> {
      let mut preamble = Vec::new();
      let mut funcs = Vec::new();
      let pe = Term::compile_expr(tlc, scope, &mut funcs, &mut preamble, term)?;
      preamble.push(pe);

      let nojit = Program::program(
         funcs,
         preamble,
      );
      println!("compile program");
      let jit = JProgram::compile(&nojit);
      println!("eval program");
      let jval = jit.eval(&[]);

      Ok(Constant::from_value(
         jval
      ))
   }
}
