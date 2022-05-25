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
   exprs: HashMap<usize,TlcExpr>,
   types: HashMap<usize,TlcTyp>,
   traits: HashMap<usize,TlcTyp>, //Traits unify and work just like types but are associated, optional, and plural
   kinds: HashMap<usize,TlcKind>,
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
        write!(f, "{}, expected {}, in {} --> {},{}{}", self.error_type, self.rule, self.filename,
               self.start.0, self.start.1, self.snippet)
    }
}

#[derive(Clone)]
pub enum TlcKind {
   Simple(String,Vec<TlcKind>),
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

#[derive(Clone)]
pub enum TlcExpr {
   Nil(usize),
   Ident(usize,String),
   App(usize,Box<TlcExpr>,Box<TlcExpr>),
   Let(usize,Box<TlcExpr>,Box<TlcExpr>,Box<TlcTyp>),
   Tuple(usize,Vec<TlcExpr>),
   Block(usize,Vec<TlcExpr>),
   Ascript(usize,Box<TlcExpr>,Box<TlcTyp>),
}

impl TLC {
   pub fn new() -> TLC {
      TLC {
         uuid: 0,
         exprs: HashMap::new(),
         types: HashMap::new(),
         traits: HashMap::new(),
         kinds: HashMap::new(),
      }
   }
   pub fn uuid(&mut self) -> usize {
      let n = self.uuid;
      self.uuid += 1;
      n
   }
   pub fn load_file(&mut self, filename: &str) -> Result<(),TlcError> {
      self.parse_file(filename)?;
      panic!("TODO load_file: interpret file");
      Ok(())
   }
   pub fn normalize_file(&mut self, ps: Pairs<crate::syntax::tlc::Rule>) -> Result<TlcExpr,TlcError> {
      self.normalize_ast(ps.peek().unwrap())
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
         Rule::let_stmt => {
            let mut es = p.into_inner();
            Ok(TlcExpr::Let(
               self.uuid(),
               Box::new(self.normalize_ast(es.next().expect("TLC Grammar Error in rule [let_stmt]"))?),
               Box::new(self.normalize_ast(es.next().expect("TLC Grammar Error in rule [let_stmt]"))?),
               Box::new(self.normalize_ast_typ(es.next().expect("TLC Grammar Error in rule [let_stmt]"))?),
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
   pub fn typecheck(&mut self, e: TlcExpr) -> Result<(),TlcError> {
      Ok(())
   }
   pub fn check(&mut self, src:&str) -> Result<(),TlcError> {
      let ast = self.parse(src)?;
      self.typecheck(ast)
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
