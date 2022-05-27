use std::path::Path;
use std::collections::HashMap;
use pest::Parser;
use pest::iterators::{Pair,Pairs};
use pest::error::{ErrorVariant,InputLocation,LineColLocation};

#[derive(Parser)]
#[grammar = "grammar_tlc.pest"]
struct TlcParser;

pub struct TLC {
   uuid: usize,
   debug: bool,
   debug_symbols: HashMap<usize,String>,
   locations: HashMap<usize,(String,(usize,usize),(usize,usize))>,
   typeof_exprs: HashMap<usize,TlcTyp>,
   types: HashMap<String,TlcTypedef>, //Canonical type names are n-ary like Tuple#2 or Tuple#9
   traits: HashMap<String,TlcTypedef>, //Traits unify and work just like types but are associated, optional, and plural
   kinds: HashMap<String,TlcKind>,
   scopes: HashMap<usize,TlcScope>,
}

pub struct TlcError {
   error_type: String,
   rule: String,
   filename: String,
   start: (usize,usize),
   end: (usize,usize),
   snippet: String
}
impl std::fmt::Debug for TlcError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "\n{}, expected {}, in {} --> {},{}\n{}\n", self.error_type, self.rule, self.filename,
               self.start.0, self.start.1, self.snippet)
    }
}

//does not implement Clone because scopes are uniquely identified by their id
pub struct TlcScope {
   id: usize,
   parent: Option<usize>,
   rules: Vec<TlcExpr>,
   children: HashMap<String,Vec<TlcExpr>>,
   statements: Vec<TlcExpr>,
}

#[derive(Clone)]
pub enum TlcKind {
   Simple(usize,String,Vec<TlcKind>),
}

#[derive(Clone)]
pub enum TlcTypedef {
   Assume(usize),
}

#[derive(Clone)]
pub enum TlcTyp {
   Nil(usize),
   Any(usize),
   Ident(usize,String),
   Or(usize,Vec<TlcTyp>),
   And(usize,Vec<TlcTyp>),
   Arrow(usize,Box<TlcTyp>,Box<TlcTyp>),
   Alias(usize,Box<TlcTyp>,Box<TlcTyp>),
   Compound(usize,Box<TlcTyp>,Vec<TlcTyp>),
   Tuple(usize,Vec<TlcTyp>),
   Angle(usize,Vec<TlcTyp>),
   Brack(usize,Vec<TlcTyp>),
}
impl std::fmt::Debug for TlcTyp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
           TlcTyp::Nil(_) => write!(f, "()"),
           TlcTyp::Any(_) => write!(f, "?"),
           TlcTyp::Ident(_,x) => write!(f, "{}", x),
           TlcTyp::Or(_,_) => write!(f, "||"),
           TlcTyp::And(_,_) => write!(f, "&&"),
           TlcTyp::Arrow(_,p,b) => write!(f, "({:?})=>({:?})", p, b),
           TlcTyp::Tuple(_,xs) => write!(f, "(#?#)"),
           _ => write!(f, "??::type"),
        }
    }
}

#[derive(Clone)]
pub enum TlcExpr {
   Nil(usize),
   Ident(usize,String),
   App(usize,Box<TlcExpr>,Box<TlcExpr>),
   Let(usize,String,Box<TlcExpr>,Box<TlcTyp>),
   Tuple(usize,Vec<TlcExpr>),
   Block(usize,Vec<TlcExpr>),
   Ascript(usize,Box<TlcExpr>,Box<TlcTyp>),
   Forall(usize,Vec<(String,Option<TlcTyp>)>,Box<Option<TlcTyp>>,Box<Option<TlcKind>>),
   Typedef(usize,String,Vec<(String,Option<TlcTyp>)>),
}
impl TlcExpr {
   pub fn id(&self) -> usize {
      match self {
         TlcExpr::Nil(id) => *id,
         TlcExpr::Ident(id,_) => *id,
         TlcExpr::App(id,_,_) => *id,
         TlcExpr::Let(id,_,_,_) => *id,
         TlcExpr::Tuple(id,_) => *id,
         TlcExpr::Block(id,_) => *id,
         TlcExpr::Ascript(id,_,_) => *id,
         TlcExpr::Forall(id,_,_,_) => *id,
         TlcExpr::Typedef(id,_,_) => *id,
      }
   }
}
impl std::fmt::Debug for TlcExpr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
           TlcExpr::Nil(_) => write!(f, "()"),
           TlcExpr::Ident(_,x) => write!(f, "{}", x),
           TlcExpr::App(_,g,x) => write!(f, "{:?}({:?})", g, x),
           TlcExpr::Let(id,v,x,t) => write!(f, "let {}: {:?} = {:?}", v, t, x),
           TlcExpr::Typedef(_,t,ps) => write!(f, "type {}", t),
           TlcExpr::Block(_,es) => {
              write!(f, "{{")?;
              for e in es.iter() {
                 write!(f, "{:?};", e)?;
              }
              write!(f, "}}")
           }
           _ => write!(f, "??::expr"),
	}
    }
}

impl TLC {
   pub fn new() -> TLC {
      TLC {
         uuid: 1, //0 is NULL-ary
         debug: true, //debug is always on for now until this stabilizes more
         debug_symbols: HashMap::new(),
         locations: HashMap::new(),
         typeof_exprs: HashMap::new(),
         types: HashMap::new(),
         traits: HashMap::new(),
         kinds: HashMap::new(),
         scopes: HashMap::new(),
      }
   }
   pub fn uuid(&mut self) -> usize {
      let n = self.uuid;
      self.uuid += 1;
      n
   }
   pub fn desugar(&mut self, parent_scope: Option<usize>, x: &TlcExpr) -> Result<usize,TlcError> {
      match x {
         TlcExpr::Block(scid,sts) => {
            //blocks can have multiple bindings of the same symbol
            //non-block bindings shadow each other
            self.scopes.insert(*scid, TlcScope {
               id: *scid,
               parent: parent_scope,
               rules: Vec::new(),
               children: HashMap::new(),
               statements: Vec::new(),
            });
            for stmt in sts.iter() {
               self.desugar(Some(*scid), stmt);
            }
            Ok(*scid)
         },
         TlcExpr::Forall(fid,qs,typ,kind) => {
            let scid = parent_scope.unwrap_or(0);
            if let Some(mut sc) = self.scopes.get_mut(&scid) {
               sc.rules.push(x.clone());
            }
            Ok(scid)
         },
         TlcExpr::Let(lid,pat,val,typ) => {
            let scid = parent_scope.unwrap_or(0);
            if let Some(mut sc) = self.scopes.get_mut(&scid) {
               if !sc.children.contains_key(pat) {
                  sc.children.insert(pat.clone(), Vec::new());
               }
               sc.children.get_mut(pat).unwrap().push(x.clone());
            }
            Ok(scid)
         },
         TlcExpr::Typedef(tid,tname,tpars) => {
            let scid = parent_scope.unwrap_or(0);
            self.types.insert(
               format!("{}#{}", tname, tpars.len()),
               TlcTypedef::Assume(*tid)
            );
            Ok(scid)
         },
         _ => {
            let scid = parent_scope.unwrap_or(0);
            if let Some(mut sc) = self.scopes.get_mut(&scid) {
               sc.statements.push(x.clone());
            }
            Ok(scid)
         }
      }
   }
   pub fn load_file(&mut self, parent_scope: Option<usize>, filename: &str) -> Result<usize,TlcError> {
      //symbol import/export rules are marginally beyond the scope of this project
      let stmts = self.parse_file(filename)?;
      let scope = self.desugar(parent_scope,&stmts)?;
      Ok(scope)
   }
   pub fn normalize_file(&mut self, fp:&str, ps: Pairs<crate::tlc::Rule>) -> Result<TlcExpr,TlcError> {
      self.normalize_ast(fp, ps.peek().unwrap())
   }
   pub fn normalize_ast_kind(&mut self, p: Pair<crate::tlc::Rule>) -> Result<TlcKind,TlcError> {
      match p.as_rule() {
         Rule::kind => {
            let mut ps = p.into_inner();
            let kg = ps.next().expect("TLC Grammar Error in rule [kind]").into_inner().concat();
            let ks = ps.map(|k|self.normalize_ast_kind(k).expect("TLC Grammar Error in rule [kind.2]"))
                       .collect::<Vec<TlcKind>>();
            Ok(TlcKind::Simple(self.uuid(),kg,ks))
         }
         rule => panic!("unexpected kind rule: {:?}", rule)
      }
   }
   pub fn normalize_ast_typ(&mut self, p: Pair<crate::tlc::Rule>) -> Result<TlcTyp,TlcError> {
      match p.as_rule() {
         Rule::ident => Ok(TlcTyp::Ident(self.uuid(),p.into_inner().concat())),
         Rule::typ => self.normalize_ast_typ(p.into_inner().next().expect("TLC Grammar Error in rule [typ]")),
         Rule::ident_typ => self.normalize_ast_typ(p.into_inner().next().expect("TLC Grammar Error in rule [ident_typ]")),
         Rule::atom_typ => self.normalize_ast_typ(p.into_inner().next().expect("TLC Grammar Error in rule [atom_typ]")),
         Rule::ident_typ => Ok(TlcTyp::Ident(self.uuid(),p.into_inner().concat())),
         Rule::any_typ => Ok(TlcTyp::Any(self.uuid())),
         Rule::paren_typ => {
            let ts = p.into_inner().map(|e|self.normalize_ast_typ(e).expect("TLC Grammar Error in rule [paren_typ]"))
                      .collect::<Vec<TlcTyp>>();
            if ts.len()==0 {
               Ok(TlcTyp::Nil(self.uuid()))
            } else if ts.len()==1 {
               Ok(ts[0].clone())
            } else {
               Ok(TlcTyp::Tuple(self.uuid(),ts))
            }
         },
         Rule::arrow_typ => {
            let mut ts = p.into_inner();
            let mut t = self.normalize_ast_typ(ts.next().expect("TLC Grammar Error in rule [arrow_typ]"))?;
            for tr in ts {
               t = TlcTyp::Arrow(
                  self.uuid(),
                  Box::new(t),
                  Box::new(self.normalize_ast_typ(tr)?)
               );
            }
            Ok(t)
         },
         Rule::alias_typ => {
            let mut ts = p.into_inner();
            let mut t = self.normalize_ast_typ(ts.next().expect("TLC Grammar Error in rule [arrow_typ]"))?;
            for tr in ts {
               t = TlcTyp::Alias(
                  self.uuid(),
                  Box::new(t),
                  Box::new(self.normalize_ast_typ(tr)?)
               );
            }
            Ok(t)
         },
         Rule::suffix_typ => {
            let mut ts = p.into_inner();
            let mut t = self.normalize_ast_typ(ts.next().expect("TLC Grammar Error in rule [suffix_typ]"))?;
            let ts = ts.map(|e|self.normalize_ast_typ(e).expect("TLC Grammar Error in rule [suffix_typ]"))
                       .collect::<Vec<TlcTyp>>();
            if ts.len()==0 {
               Ok(t)
            } else {
               Ok(TlcTyp::Compound(
                  self.uuid(),
                  Box::new(t),
                  ts
               ))
            }
         },
         Rule::or_typ => {
            let ts = p.into_inner().map(|e|self.normalize_ast_typ(e).expect("TLC Grammar Error in rule [or_typ]"))
                      .collect::<Vec<TlcTyp>>();
            if ts.len()==1 {
               Ok(ts[0].clone())
            } else {
               Ok(TlcTyp::Or(self.uuid(),ts))
            }
         },
         Rule::and_typ => {
            let ts = p.into_inner().map(|e|self.normalize_ast_typ(e).expect("TLC Grammar Error in rule [and_typ]"))
                      .collect::<Vec<TlcTyp>>();
            if ts.len()==1 {
               Ok(ts[0].clone())
            } else {
               Ok(TlcTyp::And(self.uuid(),ts))
            }
         },
         Rule::angle_typ => {
            let ts = p.into_inner().map(|e|self.normalize_ast_typ(e).expect("TLC Grammar Error in rule [angle_typ]"))
                      .collect::<Vec<TlcTyp>>();
            Ok(TlcTyp::Angle(self.uuid(),ts))
         },
         Rule::brack_typ => {
            let ts = p.into_inner().map(|e|self.normalize_ast_typ(e).expect("TLC Grammar Error in rule [brack_typ]"))
                      .collect::<Vec<TlcTyp>>();
            Ok(TlcTyp::Brack(self.uuid(),ts))
         },
         Rule::let_stmt_typ => {
            match p.into_inner().next() {
               None => Ok(TlcTyp::Nil(self.uuid())),
               Some(e) => self.normalize_ast_typ(e)
            }
         },
         rule => panic!("unexpected typ rule: {:?}", rule)
      }
   }
   pub fn normalize_ast(&mut self, fp:&str, p: Pair<crate::tlc::Rule>) -> Result<TlcExpr,TlcError> {
      let start = p.as_span().start_pos().line_col();
      let end = p.as_span().end_pos().line_col();
      let pe = match p.as_rule() {
         //entry point rule
         Rule::file => {
            let mut es = Vec::new();
            for e in p.into_inner() { match e.as_rule() {
               Rule::EOI => (),
               _ => es.push(self.normalize_ast(fp,e).expect("TLC Grammar Error in rule [file]"))
            }}
            Ok(TlcExpr::Block(self.uuid(),es))
         },

         //passthrough rules
         Rule::stmt => self.normalize_ast(fp,p.into_inner().next().expect("TLC Grammar Error in rule [stmt]")),
         Rule::term => self.normalize_ast(fp,p.into_inner().next().expect("TLC Grammar Error in rule [term]")),
         Rule::ident_term => self.normalize_ast(fp,p.into_inner().next().expect("TLC Grammar Error in rule [ident_term]")),
         Rule::tuple_term => self.normalize_ast(fp,p.into_inner().next().expect("TLC Grammar Error in rule [tuple_term]")),

         //literal value rules
         Rule::ident => Ok(TlcExpr::Ident(self.uuid(),p.into_inner().concat())),

         //complex rules
         Rule::forall_stmt => {
            let mut ps = Vec::new();
            let mut tt = None;
            let mut k  = None;
            for e in p.into_inner() {
               match e.as_rule() {
                  Rule::ascript_ident => {
                     let mut es = e.into_inner();
                     ps.push((
                        es.next().expect("TLC Grammar Error in rule [forall_stmt]").into_inner().concat(),
                        match es.next() {
                           Some(et) => Some(self.normalize_ast_typ(et)?),
                           None => None
                        }
                     ));
                  },
                  Rule::typ => tt = Some(self.normalize_ast_typ(e)?),
                  Rule::kind => k = Some(self.normalize_ast_kind(e)?),
                  rule => panic!("unexpected forall_stmt rule: {:?}", rule)
               }
            }
            Ok(TlcExpr::Forall(
               self.uuid(),
               ps,
               Box::new(tt),
               Box::new(k),
            ))
         },
         Rule::typ_stmt => {
            let mut ps = p.into_inner();
            let t = ps.next().expect("TLC Grammar Error in rule [typ_stmt.1]").into_inner().concat();
            let mut ts = Vec::new();
            for e in ps {
               match e.as_rule() {
                  Rule::ascript_ident => {
                     let mut es = e.into_inner();
                     ts.push((
                        es.next().expect("TLC Grammar Error in rule [typ_stmt.2]").into_inner().concat(),
                        match es.next() {
                           Some(et) => Some(self.normalize_ast_typ(et)?),
                           None => None
                        }
                     ));
                  },
                  rule => panic!("unexpected typ_stmt rule: {:?}", rule)
               }
            }
            Ok(TlcExpr::Typedef(
               self.uuid(),
               t,
               ts
            ))
         },
         Rule::let_stmt => {
            let mut es = p.into_inner();
            Ok(TlcExpr::Let(
               self.uuid(),
               es.next().expect("TLC Grammar Error in rule [let_stmt.1]").into_inner().concat(),
               Box::new(self.normalize_ast(fp,es.next().expect("TLC Grammar Error in rule [let_stmt.2]"))?),
               Box::new(self.normalize_ast_typ(es.next().expect("TLC Grammar Error in rule [let_stmt.3]"))?),
            ))
         },
         Rule::let_stmt_val => {
            match p.into_inner().next() {
               None => Ok(TlcExpr::Nil(self.uuid())),
               Some(e) => self.normalize_ast(fp,e)
            }
         },
         Rule::ascript_term => {
            let mut es = p.into_inner();
            let e = es.next().expect("TLC Grammar Error in rule [ascript_term]");
            match es.next() {
               None => self.normalize_ast(fp,e),
               Some(tt) => Ok(TlcExpr::Ascript(
                  self.uuid(),
                  Box::new(self.normalize_ast(fp,e)?), //term
                  Box::new(self.normalize_ast_typ(tt)?) //type
               )),
            }
         },
         Rule::term_atom => {
            let mut es = p.into_inner();
            let mut e = self.normalize_ast(fp,es.next().expect("TLC Grammar Error in rule [term_atom]"))?;
            for args in es {
               e = TlcExpr::App(
                  self.uuid(),
                  Box::new(e),
                  Box::new(self.normalize_ast(fp,args)?)
               );
            }
            Ok(e)
         },
         Rule::paren_atom => {
            let es = p.into_inner().map(|e|self.normalize_ast(fp,e).expect("TLC Grammar Error in rule [paren_atom]"))
                      .collect::<Vec<TlcExpr>>();
            if es.len()==0 {
               Ok(TlcExpr::Nil(self.uuid()))
            } else if es.len()==1 {
               Ok(es[0].clone())
            } else {
               Ok(TlcExpr::Tuple(self.uuid(),es))
            }
         },
         rule => panic!("unexpected expr rule: {:?}", rule)
      };
      if let Ok(ref pe) = pe {
         self.locations.insert(pe.id(), (fp.to_string(),start,end));
         if self.debug {
            self.debug_symbols.insert(pe.id(), format!("{:?}",pe));
         }
      };
      pe
   }
   pub fn typof(&mut self, tid: usize) -> TlcTyp {
      match self.typeof_exprs.get(&tid) {
         Some(tt) => tt.clone(),
         None => TlcTyp::Any(tid)
      }
   }
   pub fn locof(&mut self, tid: usize) -> (String,(usize,usize),(usize,usize)) {
      if let Some(loc) = self.locations.get(&tid) {
         loc.clone()
      } else {
         ("unknown file".to_string(), (0,0), (0,0))
      }
   }
   pub fn unify(&mut self, tid: usize, lt: &TlcTyp, rt: &TlcTyp) -> Result<TlcTyp,TlcError> {
      match (lt,rt) {
         (TlcTyp::Any(_),r) => Ok(r.clone()),
         (l,TlcTyp::Any(_)) => Ok(l.clone()),
         (TlcTyp::Nil(_),TlcTyp::Nil(_)) => Ok(lt.clone()),
         (l,r) => panic!("TODO: unify {:?} x {:?}", l, r)
      }
   }
   pub fn typecheck_concrete(&mut self, tid: usize) -> Result<(),TlcError> {
      let tt = self.typof(tid);
      self.typecheck_concrete_rec(tid, &tt)
   }
   pub fn estring(&self, tid: usize) -> String {
      self.debug_symbols.get(&tid).unwrap_or(&format!("??#{}::expr", tid)).to_string()
   }
   pub fn typecheck_concrete_rec(&mut self, tid: usize, tt: &TlcTyp) -> Result<(),TlcError> {
      match tt {
         TlcTyp::Nil(_) => Ok(()),
         TlcTyp::Or(_,_) => {
            let (filename,start,end) = self.locof(tid);
            Err(TlcError {
               error_type: "Type Error".to_string(),
               rule: "type is ambigious".to_string(),
               filename: filename,
               start: start,
               end: end,
               snippet: format!("{:?}",tt),
            })
         },
         TlcTyp::And(_,ts) => {
            for tc in ts.iter() {
               self.typecheck_concrete_rec(tid, tc)?;
            }
            Ok(())
         },
         TlcTyp::Arrow(_,tp,tb) => {
            self.typecheck_concrete_rec(tid, tp)?;
            self.typecheck_concrete_rec(tid, tb)
         },
         TlcTyp::Any(_) => {
            let (filename,start,end) = self.locof(tid);
            Err(TlcError {
               error_type: "Type Error".to_string(),
               rule: "type is not concrete".to_string(),
               filename: filename,
               start: start,
               end: end,
               snippet: format!("typeof({}#{})={:?}",self.estring(tid),tid,tt),
            })
         }, TlcTyp::Ident(id,tname) => {
            if self.types.contains_key(&format!("{}#0", tname)) { return Ok(()) }
            let (filename,start,end) = self.locof(tid);
            Err(TlcError {
               error_type: "Type Error".to_string(),
               rule: "type is not defined".to_string(),
               filename: filename,
               start: start,
               end: end,
               snippet: format!("{}#0",tname),
            })
         }, _ => Ok(())
      }
   }
   pub fn typecheck(&mut self, scope: Option<usize>, e: &TlcExpr) -> Result<(),TlcError> {
      match e {
         TlcExpr::Forall(_,_,_,_) => { Ok(()) },
         TlcExpr::Typedef(_,_,_) => { Ok(()) },
         TlcExpr::Nil(id) => { self.typeof_exprs.insert(*id, TlcTyp::Nil(*id)); Ok(()) },
         TlcExpr::Ident(id,_) => { panic!("TODO typecheck.1 {:?}", e) },
         TlcExpr::App(id,f,x) => { panic!("TODO typecheck.2 {:?}", e) },
         TlcExpr::Let(id,x,v,t) => {
            //variable has already been added to scope by desugar method
            self.typeof_exprs.insert(*id, TlcTyp::Nil(*id));
            self.typeof_exprs.insert(v.id(), *t.clone());
            self.typecheck(scope, v)
         },
         TlcExpr::Tuple(id,es) => { panic!("TODO typecheck.4 {:?}", e) },
         TlcExpr::Block(id,cs) => {
            let (stmts,children) = if let Some(sc) = self.scopes.get(id) {(
               sc.statements.clone(),
               sc.children.clone(),
            )} else { panic!("typecheck could not find block#{}", id) };

            let mut last_stmt_typ = TlcTyp::Nil(*id);
            for stmt in stmts.iter() {
               self.typecheck(Some(*id), stmt)?;
               last_stmt_typ = self.typeof_exprs.get(&stmt.id()).expect("typecheck Block.1").clone();
            }
            self.typeof_exprs.insert(*id, last_stmt_typ);

            for (cn,cs) in children.iter() {
               for ch in cs.iter() {
                  self.typecheck(Some(*id), ch)?;
               }
            }

            Ok(())
         },
         TlcExpr::Ascript(id,e,t) => { panic!("TODO typecheck.6 {:?}", e) },
      }
   }
   pub fn sanitycheck(&mut self, scope: Option<usize>, e: &TlcExpr) -> Result<(),TlcError> {
      match e {
         //ignore
         TlcExpr::Forall(_,_,_,_) => { Ok(()) },
         TlcExpr::Typedef(_,_,_) => { Ok(()) },

         //check that all expression types are concrete
         TlcExpr::Nil(id) => { let tt = self.typof(*id); self.unify(*id,&tt,&TlcTyp::Nil(*id))?; Ok(()) },
         TlcExpr::Block(id,cs) => {
            for c in cs.iter() {
               self.sanitycheck(scope, c)?;
            }
            self.typecheck_concrete(*id)
         },
         TlcExpr::Let(id,x,v,t) => {
            self.typecheck_concrete(*id)?;
            self.typecheck_concrete_rec(*id, t)?;
            self.sanitycheck(scope, v)?;
            let tt = self.typof(*id);
            self.unify(*id, &tt, &TlcTyp::Nil(*id))?;
            Ok(())
         },
         _ => panic!("TODO sanitycheck {:?}", e)
         /*
         TlcExpr::Ident(id,_) => { self.typecheck_concrete(*id) },
         TlcExpr::App(id,f,x) => { self.typecheck_concrete(*id) },
         TlcExpr::Tuple(id,es) => { self.typecheck_concrete(*id) },
         TlcExpr::Ascript(id,e,t) => { self.typecheck_concrete(*id) },
         */
      }
   }
   pub fn check(&mut self, globals: Option<usize>, src:&str) -> Result<(),TlcError> {
      let ast = self.parse(src)?;
      let locals = self.desugar(globals, &ast)?;
      self.typecheck(Some(locals), &ast)?;
      self.sanitycheck(Some(locals), &ast)
   }
   pub fn parse(&mut self, src:&str) -> Result<TlcExpr,TlcError> {
      self.parse_doc("[string]", src)
   }
   pub fn parse_file(&mut self, filename:&str) -> Result<TlcExpr,TlcError> {
      if !Path::new(filename).exists() {
         panic!("parse_file could not find file: '{}'", filename)
      }
      let src = std::fs::read_to_string(filename)
                   .expect("parse_file: Something went wrong reading the file");
      self.parse_doc(filename,&src)
   }
   pub fn parse_doc(&mut self, docname:&str, src:&str) -> Result<TlcExpr,TlcError> {
      let parse_result = TlcParser::parse(Rule::file, src);
      match parse_result {
        Ok(parse_ast) => self.normalize_file(docname, parse_ast),
        Err(pe) => {
          let (start,end) = match pe.line_col {
             LineColLocation::Pos(s) => (s,s),
             LineColLocation::Span(s,e) => (s,e),
          };
          let (istart,iend) = match pe.location {
             InputLocation::Pos(s) => (s,s),
             InputLocation::Span((s,e)) => (s,e),
          };
          let rule = match pe.variant {
             ErrorVariant::ParsingError {
                positives:p,
                negatives:n
             } => {
                p.iter().map(|r|{format!("{:?}",r)}).collect::<Vec<String>>().join(" or ")
             }, _ => {format!("")}
          };
          Err(TlcError { 
             error_type: "Parse Error".to_string(),
             rule: rule,
             filename:docname.to_string(),
             start:start, end:end,
             snippet: if iend>istart { format!("\n{}", &src[istart..iend]) }
                      else { format!(" {:?}", &src[istart..std::cmp::min(src.len(),istart+1)])}
          })
        }
      } 
   }
}
