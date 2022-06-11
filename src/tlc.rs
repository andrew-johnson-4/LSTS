use std::path::Path;
use pest::Parser;
use pest::iterators::{Pair,Pairs};
use pest::error::{ErrorVariant,InputLocation,LineColLocation};

#[derive(Parser)]
#[grammar = "grammar_tlc.pest"]
struct TlcParser;

pub struct TLC {
   pub rows: Vec<Row>,
   pub rules: Vec<TypeRule>,
   pub scopes: Vec<Scope>,
}

pub struct Row {
   pub term: Term,
   pub typ: Typ,
   pub kind: Kind,
   pub span: Span,
}

#[derive(Clone)]
pub struct Span {
   pub filename: String,
   pub offset_start: usize,
   pub offset_end: usize,
   pub linecol_start: (usize,usize),
   pub linecol_end: (usize,usize),
}
impl Span {
   pub fn snippet(&self) -> String {
      format!("")
   }
}

pub struct Error {
   pub kind: String,
   pub rule: String,
   pub span: Span,
   pub snippet: String,
}
impl std::fmt::Debug for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "\n{}, expected {}, in {} --> {},{}\n{}\n", self.kind, self.rule, self.span.filename,
               self.span.linecol_start.0, self.span.linecol_start.1, self.span.snippet())
    }
}

#[derive(Clone, Copy)]
pub struct ScopeId {
   pub id: usize,
}
//does not implement Clone because scopes are uniquely identified by their id
pub struct Scope {
   pub parent: Option<ScopeId>,
   pub children: Vec<(String,Term)>,
   pub statements: Vec<Term>,
}

#[derive(Clone,Eq,PartialEq,Ord,PartialOrd)]
pub enum Kind {
   Nil,
   Simple(String,Vec<Kind>),
}
impl std::fmt::Debug for Kind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
           Kind::Nil => write!(f, "Nil"),
           Kind::Simple(k,ps) => {
              if ps.len()==0 { write!(f, "{}", k) }
              else { write!(f, "{}<{:?}>", k, ps.iter().map(|p|format!("{:?}",p)).collect::<Vec<String>>().join(",")) }
           }
        }
    }
}

#[derive(Clone,Eq,PartialEq,Ord,PartialOrd)]
pub enum Typ {
   Nil,
   Any,
   Ident(String,Vec<Typ>),
   Or(Vec<Typ>),
   And(Vec<Typ>),
   Arrow(Box<Typ>,Box<Typ>),
   Tuple(Vec<Typ>),   //Tuple is order-sensitive
   Product(Vec<Typ>), //Product is order-insensitive
   Ratio(Box<Typ>,Box<Typ>),
}
impl std::fmt::Debug for Typ {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
           Typ::Nil => write!(f, "()"),
           Typ::Any => write!(f, "?"),
           Typ::Ident(t,ts) => {
              if ts.len()==0 { write!(f, "{}", t) }
              else { write!(f, "{}<{}>", t, ts.iter().map(|t|format!("{:?}",t)).collect::<Vec<String>>().join(",") ) }
           }
           Typ::Or(ts) => write!(f, "({})", ts.iter().map(|t|format!("{:?}",t)).collect::<Vec<String>>().join("|") ),
           Typ::And(ts) => write!(f, "({})", ts.iter().map(|t|format!("{:?}",t)).collect::<Vec<String>>().join("+") ),
           Typ::Tuple(ts) => write!(f, "({})", ts.iter().map(|t|format!("{:?}",t)).collect::<Vec<String>>().join(",") ),
           Typ::Product(ts) => write!(f, "({})", ts.iter().map(|t|format!("{:?}",t)).collect::<Vec<String>>().join("*") ),
           Typ::Arrow(p,b) => write!(f, "({:?})=>({:?})", p, b),
           Typ::Ratio(n,d) => write!(f, "({:?})/({:?})", n, d),
        }
    }
}

#[derive(Clone)]
pub enum Inference {
   Typ(Typ),
   Imply(Typ,Typ),
}
impl std::fmt::Debug for Inference {
   fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
      match self {
        Inference::Typ(t) => write!(f, "{:?}", t),
        Inference::Imply(a,b) => write!(f, "{:?} => {:?}", a, b),
      }
   }
}
impl Inference {
   pub fn types(&self) -> Vec<Typ> {
      match self {
         Inference::Typ(t) => vec![t.clone()],
         Inference::Imply(a,b) => vec![a.clone(),b.clone()],
      }
   }
}

#[derive(Clone)]
pub enum TypeRule {
   //type Deca<U::Unit> :: Unit;
   Typedef(String,Vec<(Option<String>,Option<Typ>,Option<Kind>)>,Option<Kind>),

   //forall A,B,1,2. (A,B,1,2) => (B,1,2) :: A<B,1,2>;
   //forall U,u:Milli<U>. Milli<U> => U = 1000 * u;
   Forall(Vec<(Option<String>,Option<Typ>,Option<Kind>)>, Inference, Option<TermId>, Option<Kind>),
}
impl std::fmt::Debug for TypeRule {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
           TypeRule::Typedef(tn,itks,tk) => write!(f, "{}{}{}{}::{:?}",
              tn,
              if itks.len()==0 { "" } else { "<" },
              itks.iter().map(|(i,t,k)| format!("{:?}:{:?}::{:?}",
                    i.clone().unwrap_or("_".to_string()),
                    t.clone().unwrap_or(Typ::Nil),
                    k.clone().unwrap_or(Kind::Nil),
              )).collect::<Vec<String>>().join(","),
              if itks.len()==0 { "" } else { ">" },
              tk
           ),
           TypeRule::Forall(itks,inf,_t,tk) => write!(f, "forall {}. {:?} :: {:?}", 
              itks.iter().map(|(i,t,k)| format!("{:?}:{:?}::{:?}",
                    i.clone().unwrap_or("_".to_string()),
                    t.clone().unwrap_or(Typ::Nil),
                    k.clone().unwrap_or(Kind::Nil),
              )).collect::<Vec<String>>().join(","),
              inf,
              tk.clone().unwrap_or(Kind::Nil),
           ),
        }
    }
}

#[derive(Clone, Copy)]
pub struct TermId {
   pub id: usize,
}
//does not implement Clone because terms are uniquely identified by their id
pub enum Term {
   Assume, //used to introduce sentinel values
   Nil,
   Ident(String),
   App(TermId,TermId),
   Let(String,TermId,Typ),
   Tuple(Vec<TermId>),
   Block(ScopeId,Vec<TermId>),
   Ascript(TermId,Typ),
}
impl std::fmt::Debug for Term {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
           Term::Assume => write!(f, "$"),
           Term::Nil => write!(f, "()"),
           Term::Ident(x) => write!(f, "{}", x),
           Term::App(g,x) => write!(f, "{:?}({:?})", g.id, x.id),
           Term::Let(v,x,t) => write!(f, "let {}: {:?} = {:?}", v, t, x.id),
           Term::Ascript(t,tt) => write!(f, "{:?}:{:?}", t.id, tt),
           Term::Tuple(es) => {
              write!(f, "(")?;
              for e in es.iter() {
                 write!(f, "{:?},", e.id)?;
              }
              write!(f, ")")
           },
           Term::Block(_,es) => {
              write!(f, "{{")?;
              for e in es.iter() {
                 write!(f, "{:?};", e.id)?;
              }
              write!(f, "}}")
           },
	}
    }
}

impl TLC {
   pub fn new() -> TLC {
      TLC {
         //the first row, index 0, is nullary
         rows: vec![Row {
            term: Term::Nil,
            typ: Typ::Nil,
            kind: Kind::Nil,
            span: Span {
               filename:"".to_string(),
               offset_start: 0,
               offset_end: 0,
               linecol_start: (0,0),
               linecol_end: (0,0),
            },
         }],
         rules: Vec::new(),
         scopes: Vec::new(),
      }
   }

   pub fn compile_str(&mut self, globals: Option<ScopeId>, src:&str) -> Result<TermId,Error> {
      self.compile_doc(globals, "[string]", src)
   }
   pub fn compile_file(&mut self, globals: Option<ScopeId>, filename:&str) -> Result<ScopeId,Error> {
      if !Path::new(filename).exists() {
         panic!("parse_file could not find file: '{}'", filename)
      }
      let src = std::fs::read_to_string(filename)
                   .expect("parse_file: Something went wrong reading the file");
      self.compile_doc(globals, filename,&src)?;
      Ok(ScopeId {id:0})
   }
   pub fn compile_doc(&mut self, _globals: Option<ScopeId>, docname:&str, src:&str) -> Result<TermId,Error> {
      let ast = self.parse_doc(docname, src)?;
      self.compile_rules(docname)?;
      Ok(ast)
   }
   pub fn kind_of(&self, t: &Typ) -> Kind {
      for rule in self.rules.iter() { match rule {
         TypeRule::Typedef(tt,_tps,k) => { if &format!("{:?}",t)==tt {
            return k.clone().unwrap_or(Kind::Simple("Term".to_string(),Vec::new()));
         }},
         _ => ()
      }}
      Kind::Nil //undefined types have Nil kind
   }
   pub fn compile_rules(&mut self, docname:&str) -> Result<(),Error> {

      //check logical consistency of foralls
      for rule in self.rules.iter() { match rule {
         TypeRule::Forall(qs,inf,_t,k) => {
            //check if domain is explicit
            if k.clone().unwrap_or(Kind::Nil) != Kind::Nil { continue; }

            //otherwise check that all variables share a domain
            let mut domains: Vec<(Typ,Kind)> = Vec::new();
            for (_i,t,k) in qs.iter() {
               match (t,k) {
                  (Some(tt),Some(kk)) => domains.push((tt.clone(),kk.clone())),
                  (Some(tt),None) => domains.push((tt.clone(),self.kind_of(tt))),
                  _ => domains.push((Typ::Nil,Kind::Nil))
               }
            }
            for it in inf.types().iter() {
               if !qs.iter().any(|(_i,t,_k)| &Some(it.clone())==t) {
                  domains.push((it.clone(),self.kind_of(it)));
               }
            }
            domains.sort();
            domains.dedup();
            let firstkind = if domains.len()==0 { Kind::Nil } else { domains[0].1.clone() };
            let kind = domains.iter().fold(firstkind,|l,(_,r)| if l==*r {l} else {Kind::Nil});
            if kind==Kind::Nil {
               return Err(Error { 
                  kind: "Type Error".to_string(),
                  rule: format!("({}) do not share a domain ({})", 
                     domains.iter().map(|(t,_k)|format!("{:?}",t)).collect::<Vec<String>>().join(","),
                     domains.iter().map(|(_t,k)|format!("{:?}",k)).collect::<Vec<String>>().join(",")
                  ),
                  span: Span {
                     filename:docname.to_string(),
                     offset_start: 0,
                     offset_end: 0,
                     linecol_start: (0,0),
                     linecol_end: (0,0),
                  },
                  snippet: "".to_string()
               })
            }
         },
         _ => ()
      }}

      Ok(())
   }
   pub fn parse(&mut self, src:&str) -> Result<TermId,Error> {
      self.parse_doc("[string]", src)
   }
   pub fn parse_doc(&mut self, docname:&str, src:&str) -> Result<TermId,Error> {
      let parse_result = TlcParser::parse(Rule::file, src);
      match parse_result {
        Ok(parse_ast) => { self.unparse_file(docname, parse_ast) }
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
                negatives:_
             } => {
                p.iter().map(|r|{format!("{:?}",r)}).collect::<Vec<String>>().join(" or ")
             }, _ => {format!("")}
          };
          Err(Error { 
             kind: "Parse Error".to_string(),
             rule: rule,
             span: Span {
                filename:docname.to_string(),
                offset_start: istart,
                offset_end: iend,
                linecol_start: start,
                linecol_end: end,
             },
             snippet: if iend>istart { format!("\n{}", &src[istart..iend]) }
                      else { format!(" {:?}", &src[istart..std::cmp::min(src.len(),istart+1)])}
          })
        }
      } 
   }
   pub fn unparse_file(&mut self, fp:&str, ps: Pairs<crate::tlc::Rule>) -> Result<TermId,Error> {
      self.unparse_ast(&None, fp, ps.peek().unwrap())
   }
   pub fn push_term(&mut self, term: Term, span: &Span) -> TermId {
      let index = self.rows.len();
      self.rows.push(Row {
         term: term,
         typ: Typ::Any,
         kind: Kind::Simple("Term".to_string(),Vec::new()),
         span: span.clone(),
      });
      TermId { id: index }
   }
   pub fn push_scope(&mut self, scope: Scope, _span: &Span) -> ScopeId {
      let index = self.scopes.len();
      self.scopes.push(scope);
      ScopeId { id: index }
   }
   pub fn unparse_ast(&mut self, scope:&Option<ScopeId>, fp:&str, p: Pair<crate::tlc::Rule>) -> Result<TermId,Error> {
      let span = Span {
         filename: fp.to_string(),
         linecol_start: p.as_span().start_pos().line_col(),
         linecol_end: p.as_span().end_pos().line_col(),
         offset_start: p.as_span().start(),
         offset_end: p.as_span().end(),
      };
      match p.as_rule() {
         //entry point rule
         Rule::file => {
            let sid = self.push_scope(Scope {
               parent: *scope,
               children: Vec::new(),
               statements: Vec::new(),
            }, &span);
            let mut es = Vec::new();
            for e in p.into_inner() { match e.as_rule() {
               Rule::EOI => (),
               _ => es.push(self.unparse_ast(&Some(sid),fp,e).expect("TLC Grammar Error in rule [file]"))
            }}
            Ok(self.push_term(Term::Block(sid,es), &span))
         },

         //block statement rule
         Rule::block => {
            let sid = self.push_scope(Scope {
               parent: *scope,
               children: Vec::new(),
               statements: Vec::new(),
            }, &span);
            let mut es = Vec::new();
            for e in p.into_inner() { match e.as_rule() {
               _ => es.push(self.unparse_ast(&Some(sid),fp,e).expect("TLC Grammar Error in rule [file]"))
            }}
            Ok(self.push_term(Term::Block(sid, es), &span))
         },

         //passthrough rules
         Rule::stmt => self.unparse_ast(scope,fp,p.into_inner().next().expect("TLC Grammar Error in rule [stmt]")),
         Rule::term => self.unparse_ast(scope,fp,p.into_inner().next().expect("TLC Grammar Error in rule [term]")),
         Rule::value_term => self.unparse_ast(scope,fp,p.into_inner().next().expect("TLC Grammar Error in rule [value_term]")),

         //literal value rules
         Rule::ident => Ok(self.push_term(Term::Ident(p.into_inner().concat()), &span)),

         //complex rules
         Rule::let_stmt => {
            //TODO parse let stmt parameters
            Ok(self.push_term(Term::Assume, &span))
         },
         Rule::let_stmt_par => {
            //TODO parse let stmt parameters
            Ok(self.push_term(Term::Assume, &span))
         },
         Rule::ascript_term => {
            let mut es = p.into_inner();
            let e = es.next().expect("TLC Grammar Error in rule [ascript_term]");
            match es.next() {
               None => self.unparse_ast(scope,fp,e),
               Some(tt) => Ok({let t = Term::Ascript(
                  self.unparse_ast(scope,fp,e)?, //term
                  self.unparse_ast_typ(tt)? //type
               ); self.push_term(t, &span)}),
            }
         },
         Rule::atom_term => {
            let mut es = p.into_inner();
            let mut e = self.unparse_ast(scope,fp,es.next().expect("TLC Grammar Error in rule [atom_term]"))?;
            for args in es {
               e = {let t = Term::App(
                  e,
                  self.unparse_ast(scope,fp,args)?
               ); self.push_term(t,&span)};
            }
            Ok(e)
         },
         Rule::tuple_term => {
            let es = p.into_inner().map(|e|self.unparse_ast(scope,fp,e).expect("TLC Grammar Error in rule [tuple_term]"))
                      .collect::<Vec<TermId>>();
            if es.len()==0 {
               Ok(self.push_term(Term::Nil, &span))
            } else if es.len()==1 {
               Ok(es[0].clone())
            } else {
               Ok(self.push_term(Term::Tuple(es), &span))
            }
         },

         //inference rules
         Rule::typ_stmt => {
            let mut ps = p.into_inner();
            let t = ps.next().expect("TLC Grammar Error in rule [typ_stmt.1]").into_inner().concat();
            let ts = Vec::new();
            let kind = None;
            for e in ps { match e.as_rule() {
               Rule::ident_typ_kind => {
                  let mut _ident = None;
                  let mut _typ = None;
                  let mut _kind = None;
                  for itk in e.into_inner() { match itk.as_rule() {
                     Rule::ident => { _ident = Some(itk.into_inner().concat()); },
                     Rule::typ   => { _typ   = Some(self.unparse_ast_typ(itk)); },
                     Rule::kind   => { _kind   = Some(self.unparse_ast_kind(itk)); },
                     rule => panic!("unexpected ident_typ_kind rule: {:?}", rule)
                  }}
               },
               rule => panic!("unexpected typ_stmt rule: {:?}", rule)
            }}
            self.rules.push(TypeRule::Typedef(
               t,
               ts,
               kind
            ));
            Ok(TermId { id:0 })
         },

         
         Rule::forall_stmt => {
            let mut quants = Vec::new();
            let mut inference  = None;
            let mut term = None;
            let mut kind = None;
            for e in p.into_inner() { match e.as_rule() {
               Rule::ident_typ_kind => {
                  let mut ident = None;
                  let mut typ = None;
                  let mut kind = None;
                  for itk in e.into_inner() { match itk.as_rule() {
                     Rule::ident => { ident = Some(itk.into_inner().concat()); },
                     Rule::typ   => { typ   = Some(self.unparse_ast_typ(itk)?); },
                     Rule::kind   => { kind   = Some(self.unparse_ast_kind(itk)?); },
                     rule => panic!("unexpected ident_typ_kind rule: {:?}", rule)
                  }}
                  quants.push((ident, typ, kind));
               },
               Rule::inference => { inference = Some(self.unparse_ast_inference(e)?); }
               Rule::term => { term = Some(self.unparse_ast(scope,fp,e)?); }
               Rule::kind => { kind = Some(self.unparse_ast_kind(e)?); }
               rule => panic!("unexpected typ_stmt rule: {:?}", rule)
            }}
            self.rules.push(TypeRule::Forall(
               quants,
               inference.expect("TLC Grammar Error in rule [forall_stmt], expected inference"),
               term,
               kind
            ));
            Ok(TermId { id:0 })
         },

         rule => panic!("unexpected expr rule: {:?}", rule)
      }
   }
   pub fn unparse_ast_inference(&mut self, p: Pair<crate::tlc::Rule>) -> Result<Inference,Error> {
      let mut a = None;
      let mut b = None;
      for e in p.into_inner() { match e.as_rule() {
         Rule::typ => {
            if a.is_none() { a = Some(self.unparse_ast_typ(e)?); }
            else { b = Some(self.unparse_ast_typ(e)?); }
         },
         rule => panic!("unexpected inference rule: {:?}", rule)
      }}
      if a.is_none() { panic!("TLC Grammar Error in rule [inference]") }
      else if b.is_none() { Ok(Inference::Typ(a.unwrap())) }
      else { Ok(Inference::Imply(a.unwrap(), b.unwrap())) }
   }
   pub fn unparse_ast_typ(&mut self, p: Pair<crate::tlc::Rule>) -> Result<Typ,Error> {
      match p.as_rule() {
         Rule::typname => Ok(Typ::Ident(p.into_inner().concat(),Vec::new())),
         Rule::typ => self.unparse_ast_typ(p.into_inner().next().expect("TLC Grammar Error in rule [typ]")),
         Rule::ident_typ => self.unparse_ast_typ(p.into_inner().next().expect("TLC Grammar Error in rule [ident_typ]")),
         Rule::atom_typ => self.unparse_ast_typ(p.into_inner().next().expect("TLC Grammar Error in rule [atom_typ]")),
         Rule::any_typ => Ok(Typ::Any),
         Rule::paren_typ => {
            let ts = p.into_inner().map(|e|self.unparse_ast_typ(e).expect("TLC Grammar Error in rule [paren_typ]"))
                      .collect::<Vec<Typ>>();
            if ts.len()==0 {
               Ok(Typ::Nil)
            } else if ts.len()==1 {
               Ok(ts[0].clone())
            } else {
               Ok(Typ::Tuple(ts))
            }
         },
         Rule::arrow_typ => {
            let mut ts = p.into_inner();
            let mut t = self.unparse_ast_typ(ts.next().expect("TLC Grammar Error in rule [arrow_typ]"))?;
            for tr in ts {
               t = Typ::Arrow(
                  Box::new(t),
                  Box::new(self.unparse_ast_typ(tr)?)
               );
            }
            Ok(t)
         },
         Rule::suffix_typ => {
            let mut ts = p.into_inner();
            let t = self.unparse_ast_typ(ts.next().expect("TLC Grammar Error in rule [suffix_typ]"))?;
            //for t in ts {
               //TODO parameterized types and bracketed types
            //}
            Ok(t)
         },
         Rule::ratio_typ => {
            let ts = p.into_inner().map(|e|self.unparse_ast_typ(e).expect("TLC Grammar Error in rule [ratio_typ.1]"))
                      .collect::<Vec<Typ>>();
            if ts.len()==1 {
               Ok(ts[0].clone())
            } else if ts.len()==2 {
               Ok(Typ::Ratio(Box::new(ts[0].clone()),Box::new(ts[1].clone())))
            } else {
               panic!("TLC Grammar Error in rule [ratio_typ.2]")
            }
         },
         Rule::product_typ => {
            let ts = p.into_inner().map(|e|self.unparse_ast_typ(e).expect("TLC Grammar Error in rule [or_typ]"))
                      .collect::<Vec<Typ>>();
            if ts.len()==1 {
               Ok(ts[0].clone())
            } else {
               Ok(Typ::Product(ts))
            }
         },
         Rule::or_typ => {
            let ts = p.into_inner().map(|e|self.unparse_ast_typ(e).expect("TLC Grammar Error in rule [or_typ]"))
                      .collect::<Vec<Typ>>();
            if ts.len()==1 {
               Ok(ts[0].clone())
            } else {
               Ok(Typ::Or(ts))
            }
         },
         Rule::and_typ => {
            let ts = p.into_inner().map(|e|self.unparse_ast_typ(e).expect("TLC Grammar Error in rule [and_typ]"))
                      .collect::<Vec<Typ>>();
            if ts.len()==1 {
               Ok(ts[0].clone())
            } else {
               Ok(Typ::And(ts))
            }
         },
         rule => panic!("unexpected typ rule: {:?}", rule)
      }
   }
   pub fn unparse_ast_kind(&mut self, p: Pair<crate::tlc::Rule>) -> Result<Kind,Error> {
      match p.as_rule() {
         Rule::kind => {
            let mut name = "Nil".to_string();
            let mut kinds = Vec::new();
            for e in p.into_inner() { match e.as_rule() {
               Rule::kindname => { name = e.into_inner().concat(); },
               Rule::kind  => { kinds.push(self.unparse_ast_kind(e)?); },
               rule => panic!("unexpected ident_typ_kind rule: {:?}", rule)
            }}
            if name=="Nil" { Ok(Kind::Nil) }
            else { Ok(Kind::Simple(name, kinds)) }
         },
         rule => panic!("unexpected kind rule: {:?}", rule)
      }
   }
   pub fn check(&mut self, globals: Option<ScopeId>, src:&str) -> Result<(),Error> {
      let _ast = self.compile_str(globals, src)?;
      Ok(())
   }
}

/*
   pub fn desugar(&mut self, parent_scope: Option<usize>, x: &Term) -> Result<usize,Error> {
      match x {
         Term::Block(scid,sts) => {
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

         Term::Forall(fid,qs,typ,kind) => {
            let scid = parent_scope.unwrap_or(0);
            if let Some(mut sc) = self.scopes.get_mut(&scid) {
               sc.rules.push(x.clone());
            }
            Ok(scid)
         },
         Term::Let(lid,pat,val,typ) => {
            let scid = parent_scope.unwrap_or(0);
            if let Some(mut sc) = self.scopes.get_mut(&scid) {
               if !sc.children.contains_key(pat) {
                  sc.children.insert(pat.clone(), Vec::new());
               }
               sc.children.get_mut(pat).unwrap().push(x.clone());
            }
            Ok(scid)
         },
         Term::Typedef(tid,tname,tpars) => {
            let scid = parent_scope.unwrap_or(0);
            self.types.insert(
               format!("{}#{}", tname, tpars.len()),
               Typedef::Assume(*tid)
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
   pub fn load_file(&mut self, parent_scope: Option<usize>, filename: &str) -> Result<usize,Error> {
      //symbol import/export rules are marginally beyond the scope of this project
      let stmts = self.parse_file(filename)?;
      let scope = self.desugar(parent_scope,&stmts)?;
      Ok(scope)
   }
   pub fn unparse_ast_kind(&mut self, p: Pair<crate::tlc::Rule>) -> Result<TlcKind,Error> {
      match p.as_rule() {
         Rule::kind => {
            let mut ps = p.into_inner();
            let kg = ps.next().expect("TLC Grammar Error in rule [kind]").into_inner().concat();
            let ks = ps.map(|k|self.unparse_ast_kind(k).expect("TLC Grammar Error in rule [kind.2]"))
                       .collect::<Vec<TlcKind>>();
            Ok(TlcKind::Simple(self.uuid(),kg,ks))
         }
         rule => panic!("unexpected kind rule: {:?}", rule)
      }
   }
   pub fn typof_var(&self, scope: Option<usize>, v: &str, vid: usize) -> Typ {
      if let Some(sc) = self.scopes.get(&scope.unwrap_or(0)) {
         if let Some(cs) = sc.children.get(v) {
            let mut ts = Vec::new();
            for c in cs.iter() {
               ts.push(self.typof(c.id()));
            }
            if ts.len()==0 { Typ::Nil(vid) }
            else if ts.len()==1 { ts[0].clone() }
            else { Typ::Or(vid,ts) }
         } else {
            panic!("could not find variable {} in scope#{}", v, scope.unwrap_or(0))
         }
      } else {
         panic!("could not find scope#{}", scope.unwrap_or(0))
      }
   }
   pub fn typof(&self, tid: usize) -> Typ {
      match self.typeof_exprs.get(&tid) {
         Some(tt) => tt.clone(),
         None => {
            if self.debug { panic!("Could not find typeof expressions {}#{}", self.estring(tid), tid) }
            else { panic!("Could not find typeof expression #{}", tid) }
         }
      }
   }
   pub fn locof(&mut self, tid: usize) -> (String,(usize,usize),(usize,usize)) {
      if let Some(loc) = self.locations.get(&tid) {
         loc.clone()
      } else {
         ("unknown file".to_string(), (0,0), (0,0))
      }
   }
   pub fn unify(&mut self, tid: usize, lt: &Typ, rt: &Typ) -> Result<Typ,Error> {
      match (lt,rt) {
         (Typ::Any(_),r) => Ok(r.clone()),
         (l,Typ::Any(_)) => Ok(l.clone()),
         (Typ::Nil(_),Typ::Nil(_)) => Ok(lt.clone()),
         (Typ::Ident(lid,lv),Typ::Ident(_,rv)) if lv==rv => Ok(lt.clone()),
         (Typ::Arrow(lid,pl,bl),Typ::Arrow(_,pr,br)) => {
            let pt = self.unify(tid,pl,pr)?;
            let bt = self.unify(tid,bl,br)?;
            Ok(Typ::Arrow(*lid,Box::new(pt),Box::new(bt)))
         },
         (l,r) => {
            let (filename,start,end) = self.locof(tid);
            Err(Error {
               error_type: "Type Error".to_string(),
               rule: "failed unification".to_string(),
               filename: filename,
               start: start,
               end: end,
               snippet: format!("{:?} (x) {:?}",l,r),
            })
         }
      }
   }
   pub fn typecheck_concrete(&mut self, tid: usize) -> Result<(),Error> {
      let tt = self.typof(tid);
      self.typecheck_concrete_rec(tid, &tt)
   }
   pub fn estring(&self, tid: usize) -> String {
      self.debug_symbols.get(&tid).unwrap_or(&format!("??#{}::expr", tid)).to_string()
   }
   pub fn typecheck_concrete_rec(&mut self, tid: usize, tt: &Typ) -> Result<(),Error> {
      match tt {
         Typ::Nil(_) => Ok(()),
         Typ::Or(_,_) => {
            let (filename,start,end) = self.locof(tid);
            Err(Error {
               error_type: "Type Error".to_string(),
               rule: "type is ambigious".to_string(),
               filename: filename,
               start: start,
               end: end,
               snippet: format!("{:?}",tt),
            })
         },
         Typ::And(_,ts) => {
            for tc in ts.iter() {
               self.typecheck_concrete_rec(tid, tc)?;
            }
            Ok(())
         },
         Typ::Arrow(_,tp,tb) => {
            self.typecheck_concrete_rec(tid, tp)?;
            self.typecheck_concrete_rec(tid, tb)
         },
         Typ::Any(_) => {
            let (filename,start,end) = self.locof(tid);
            Err(Error {
               error_type: "Type Error".to_string(),
               rule: "type is not concrete".to_string(),
               filename: filename,
               start: start,
               end: end,
               snippet: format!("typeof({}#{})={:?}",self.estring(tid),tid,tt),
            })
         }, Typ::Ident(id,tname) => {
            if self.types.contains_key(&format!("{}#0", tname)) { return Ok(()) }
            let (filename,start,end) = self.locof(tid);
            Err(Error {
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
   pub fn typecheck(&mut self, scope: Option<usize>, e: &Term) -> Result<(),Error> {
      match e {
         Term::Assume(_) => { Ok(()) },
         Term::Forall(_,_,_,_) => { Ok(()) },
         Term::Typedef(_,_,_) => { Ok(()) },
         Term::Nil(id) => { self.typeof_exprs.insert(*id, Typ::Nil(*id)); Ok(()) },
         Term::Ident(id,v) => {
            let vt = self.typof_var(scope, v, *id);
            self.typecheck_concrete_rec(*id, &vt)?;
            self.typeof_exprs.insert(*id, vt);
            Ok(())
         },
         Term::App(id,f,x) => {
            self.typecheck(scope,f)?;
            self.typecheck(scope,x)?;
            let ft = self.typof(f.id());
            let xt = self.typof(x.id());
            let rt = self.unify(*id, &Typ::Arrow(*id,Box::new(xt),Box::new(Typ::Any(*id))), &ft)?;
            self.typeof_exprs.insert(*id, rt);
            Ok(())
         },
         Term::Let(id,x,v,t) => {
            //variable has already been added to scope by desugar method
            self.typeof_exprs.insert(*id, *t.clone());
            self.typeof_exprs.insert(v.id(), *t.clone());
            self.typecheck(scope, v)
         },
         Term::Tuple(id,es) => { panic!("TODO typecheck.4 {:?}", e) },
         Term::Block(id,cs) => {
            let (stmts,children) = if let Some(sc) = self.scopes.get(id) {(
               sc.statements.clone(),
               sc.children.clone(),
            )} else { panic!("typecheck could not find block#{}", id) };

            //step 1, typecheck variable declarations
            for (cn,cs) in children.iter() {
               for ch in cs.iter() {
                  self.typecheck(Some(*id), ch)?;
               }
            }

            //step 2, typecheck block statements
            let mut last_stmt_typ = Typ::Nil(*id);
            for stmt in stmts.iter() {
               self.typecheck(Some(*id), stmt)?;
               if let Some(stmt_typ) = self.typeof_exprs.get(&stmt.id()) {
                  last_stmt_typ = stmt_typ.clone(); 
               } else {
                  panic!("typecheck did not set a type for {:?}::expr", stmt);
               }
            }
            self.typeof_exprs.insert(*id, last_stmt_typ);

            Ok(())
         },
         Term::Ascript(id,e,t) => { panic!("TODO typecheck.6 {:?}", e) },
      }
   }
   pub fn sanitycheck(&mut self, scope: Option<usize>, e: &Term) -> Result<(),Error> {
      match e {
         //ignore
         Term::Forall(_,_,_,_) => { Ok(()) },
         Term::Typedef(_,_,_) => { Ok(()) },

         //check that all expression types are concrete
         Term::Assume(id) => { Ok(()) },
         Term::Nil(id) => { let tt = self.typof(*id); self.unify(*id,&tt,&Typ::Nil(*id))?; Ok(()) },
         Term::Block(id,cs) => {
            for c in cs.iter() {
               self.sanitycheck(scope, c)?;
            }
            self.typecheck_concrete(*id)
         },
         Term::Let(id,x,v,t) => {
            self.typecheck_concrete(*id)?;
            self.typecheck_concrete_rec(*id, t)?;
            self.sanitycheck(scope, v)?;
            Ok(())
         },
         Term::Ident(id,_) => { self.typecheck_concrete(*id) },
         Term::App(id,f,x) => {
            self.typecheck_concrete(f.id())?;
            self.typecheck_concrete(x.id())?;
            self.typecheck_concrete(*id)
         },
         _ => panic!("TODO sanitycheck {:?}", e)
         Term::Tuple(id,es) => { self.typecheck_concrete(*id) },
         Term::Ascript(id,e,t) => { self.typecheck_concrete(*id) },
      }
   }
}
*/
