use std::path::Path;
use std::collections::HashMap;
use pest::Parser;
use pest::iterators::{Pair,Pairs};
use pest::error::{ErrorVariant,InputLocation,LineColLocation};

#[derive(Parser)]
#[grammar = "tlc.pest"]
struct TlcParser;

pub struct TLC {
   uuid: usize,
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
           _ => write!(f, "??"),
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
impl std::fmt::Debug for TlcExpr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
           TlcExpr::Nil(_) => write!(f, "()"),
           TlcExpr::Ident(_,x) => write!(f, "{}", x),
           _ => write!(f, "??"),
	}
    }
}

impl TLC {
   pub fn new() -> TLC {
      TLC {
         uuid: 0,
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
   pub fn desugar(&mut self, parent_scope: Option<usize>, x: TlcExpr) -> Result<usize,TlcError> {
      match x {
        TlcExpr::Block(id,sts) => {
           //blocks can have multiple bindings of the same symbol
           //non-block bindings shadow each other
           self.scopes.insert(id, TlcScope {
              id: id,
              parent: parent_scope,
              rules: Vec::new(),
              children: HashMap::new(),
              statements: Vec::new(),
           });
           for stmt in sts.iter() {
              match stmt {
                 TlcExpr::Forall(id,qs,typ,kind) => {
                    if let Some(mut sc) = self.scopes.get_mut(&id) {
                       sc.rules.push(stmt.clone());
                    }
                 },
                 TlcExpr::Let(id,pat,val,typ) => {
                    if let Some(mut sc) = self.scopes.get_mut(&id) {
                       if !sc.children.contains_key(pat) {
                          sc.children.insert(pat.clone(), Vec::new());
                       }
                       sc.children.get_mut(pat).unwrap().push(stmt.clone());
                    }
                 },
                 _ => {
                    if let Some(mut sc) = self.scopes.get_mut(&id) {
                       sc.statements.push(stmt.clone());
                    }
                 }
              }
           }
           Ok(id)
        },
        _ => panic!("TLC::desugar: expected block")
      }
   }
   pub fn load_file(&mut self, parent_scope: Option<usize>, filename: &str) -> Result<usize,TlcError> {
      //symbol import/export rules are marginally beyond the scope of this project
      let stmts = self.parse_file(filename)?;
      let scope = self.desugar(parent_scope,stmts)?;
      Ok(scope)
   }
   pub fn normalize_file(&mut self, ps: Pairs<crate::syntax::tlc::Rule>) -> Result<TlcExpr,TlcError> {
      self.normalize_ast(ps.peek().unwrap())
   }
   pub fn normalize_ast_kind(&mut self, p: Pair<crate::syntax::tlc::Rule>) -> Result<TlcKind,TlcError> {
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
   pub fn normalize_ast_typ(&mut self, p: Pair<crate::syntax::tlc::Rule>) -> Result<TlcTyp,TlcError> {
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
   pub fn normalize_ast(&mut self, p: Pair<crate::syntax::tlc::Rule>) -> Result<TlcExpr,TlcError> {
      match p.as_rule() {
         //entry point rule
         Rule::file => {
            let mut es = Vec::new();
            for e in p.into_inner() { match e.as_rule() {
               Rule::EOI => (),
               _ => es.push(self.normalize_ast(e).expect("TLC Grammar Error in rule [file]"))
            }}
            if es.len()==1 {
               Ok(es[0].clone())
            } else {
               Ok(TlcExpr::Block(self.uuid(),es))
            }
         },

         //passthrough rules
         Rule::stmt => self.normalize_ast(p.into_inner().next().expect("TLC Grammar Error in rule [stmt]")),
         Rule::term => self.normalize_ast(p.into_inner().next().expect("TLC Grammar Error in rule [term]")),
         Rule::ident_term => self.normalize_ast(p.into_inner().next().expect("TLC Grammar Error in rule [ident_term]")),
         Rule::tuple_term => self.normalize_ast(p.into_inner().next().expect("TLC Grammar Error in rule [tuple_term]")),

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
               Box::new(self.normalize_ast(es.next().expect("TLC Grammar Error in rule [let_stmt.2]"))?),
               Box::new(self.normalize_ast_typ(es.next().expect("TLC Grammar Error in rule [let_stmt.3]"))?),
            ))
         },
         Rule::let_stmt_val => {
            match p.into_inner().next() {
               None => Ok(TlcExpr::Nil(self.uuid())),
               Some(e) => self.normalize_ast(e)
            }
         },
         Rule::ascript_term => {
            let mut es = p.into_inner();
            let e = es.next().expect("TLC Grammar Error in rule [ascript_term]");
            match es.next() {
               None => self.normalize_ast(e),
               Some(tt) => Ok(TlcExpr::Ascript(
                  self.uuid(),
                  Box::new(self.normalize_ast(e)?), //term
                  Box::new(self.normalize_ast_typ(tt)?) //type
               )),
            }
         },
         Rule::term_atom => {
            let mut es = p.into_inner();
            let mut e = self.normalize_ast(es.next().expect("TLC Grammar Error in rule [term_atom]"))?;
            for args in es {
               e = TlcExpr::App(
                  self.uuid(),
                  Box::new(e),
                  Box::new(self.normalize_ast(args)?)
               );
            }
            Ok(e)
         },
         Rule::paren_atom => {
            let es = p.into_inner().map(|e|self.normalize_ast(e).expect("TLC Grammar Error in rule [paren_atom]"))
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
      }
   }
   pub fn typof(&mut self, tid: usize) -> TlcTyp {
      match self.typeof_exprs.get(&tid) {
         Some(tt) => tt.clone(),
         None => TlcTyp::Any(tid)
      }
   }
   pub fn unify(&mut self, lt: TlcTyp, rt: TlcTyp) -> Result<TlcTyp,TlcError> {
      Ok(lt)
   }
   pub fn typecheck_concrete(&mut self, tid: usize) -> Result<(),TlcError> {
      Err(TlcError {
         error_type: "Type Error".to_string(),
         rule: "type is not concrete".to_string(),
         filename: "unknown file".to_string(),
         start: (0,0),
         end: (0,0),
         snippet: format!("typeof(expr#{})={:?}",tid,self.typof(tid)),
      })
   }
   pub fn typecheck(&mut self, scope: Option<usize>, e: TlcExpr) -> Result<(),TlcError> {
      match e {
         //ignore
         TlcExpr::Forall(_,_,_,_) => { Ok(()) },
         TlcExpr::Typedef(_,_,_) => { Ok(()) },

         //check that all expression types are concrete
         TlcExpr::Nil(id) => { let tt = self.typof(id); self.unify(tt,TlcTyp::Nil(id))?; Ok(()) },
         TlcExpr::Ident(id,_) => { self.typecheck_concrete(id) },
         TlcExpr::App(id,f,x) => { self.typecheck_concrete(id) },
         TlcExpr::Let(id,x,v,t) => { self.typecheck_concrete(id) },
         TlcExpr::Tuple(id,es) => { self.typecheck_concrete(id) },
         TlcExpr::Block(id,es) => { self.typecheck_concrete(id) },
         TlcExpr::Ascript(id,e,t) => { self.typecheck_concrete(id) },
      }
/*
pub enum TlcTyp {
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
*/
   }
   pub fn check(&mut self, scope: Option<usize>, src:&str) -> Result<(),TlcError> {
      let ast = self.parse(src)?;
      self.typecheck(scope, ast)
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
        Ok(parse_ast) => self.normalize_file(parse_ast),
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
