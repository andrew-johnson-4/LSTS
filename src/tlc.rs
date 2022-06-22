use std::collections::{HashSet,HashMap};
use std::path::Path;
use pest::Parser;
use pest::iterators::{Pair,Pairs};
use pest::error::{ErrorVariant,InputLocation,LineColLocation};
use regex::Regex;
use crate::term::{Term,TermId};
use crate::scope::{Scope,ScopeId};
use crate::typ::Type;
use crate::kind::Kind;

#[derive(Parser)]
#[grammar = "grammar_tlc.pest"]
struct TlcParser;

pub struct TLC {
   pub rows: Vec<Row>,
   pub rules: Vec<TypeRule>,
   pub scopes: Vec<Scope>,
   pub regexes: Vec<(Type,Regex)>,
   pub constructors: HashMap<String,(Type,Vec<Type>,Vec<(String,Type)>)>,
   pub type_is_normal: HashSet<Type>,
   pub kind_is_normal: HashSet<Kind>,
   pub typedef_index: HashMap<String,usize>,
   pub foralls_index: HashMap<Type,Vec<usize>>,
   pub foralls_rev_index: HashMap<Type,Vec<usize>>,
   pub term_kind: Kind,
   pub nil_type: Type,
   pub bottom_type: Type,
}

pub enum Constant {
   Integer(i64)
}

pub struct Row {
   pub term: Term,
   pub typ: Type,
   pub kind: Kind,
   pub span: Span,
   pub constant: Option<Constant>,
}

#[derive(Clone)]
pub struct Span {
   pub filename: String,
   pub offset_start: usize,
   pub offset_end: usize,
   pub linecol_start: (usize,usize),
   pub linecol_end: (usize,usize),
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
               self.span.linecol_start.0, self.span.linecol_start.1, self.snippet)
    }
}

#[derive(Clone)]
pub enum Inference {
   Type(Type),
   Imply(Type,Type),
}
impl std::fmt::Debug for Inference {
   fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
      match self {
        Inference::Type(t) => write!(f, "{:?}", t),
        Inference::Imply(a,b) => write!(f, "{:?} => {:?}", a, b),
      }
   }
}
impl Inference {
   pub fn types(&self) -> Vec<Type> {
      match self {
         Inference::Type(t) => vec![t.clone()],
         Inference::Imply(a,b) => vec![a.clone(),b.clone()],
      }
   }
}

#[derive(Clone)]
pub enum Typedef {
   Regex(String),
   Constructor(String,Vec<(String,Type)>),
}

#[derive(Clone)]
pub struct Invariant {
   pub itks: Vec<(Option<String>,Option<Type>,Kind)>,
   pub assm: Option<TermId>,
   pub prop: TermId,
}

#[derive(Clone)]
pub enum TypeRule {
   Typedef(String,bool,Vec<(String,Option<Type>,Kind)>,Option<Type>,Vec<Typedef>,Kind,Vec<Invariant>,Span),

   Forall(Vec<(Option<String>,Option<Type>,Kind)>, Inference, Option<TermId>, Kind, Span),
}
impl TypeRule {
   pub fn span(&self) -> Span {
      match self {
         TypeRule::Typedef(_,_,_,_,_,_,_,sp) => sp.clone(),
         TypeRule::Forall(_,_,_,_,sp) => sp.clone(),
      }
   }
}
impl std::fmt::Debug for TypeRule {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
           TypeRule::Typedef(tn,_norm,itks,implies,_td,tk,_props,_) => write!(f, "{}{}{}{}{}::{:?}",
              tn,
              if itks.len()==0 { "" } else { "<" },
              itks.iter().map(|(t,i,k)| format!("{:?}:{:?}::{:?}",
                    t.clone(),
                    i.clone().unwrap_or(Type::Any),
                    k
              )).collect::<Vec<String>>().join(","),
              if itks.len()==0 { "" } else { ">" },
              if let Some(ti) = implies { format!(":{:?}",ti) } else { format!("") },
              tk
           ),
           TypeRule::Forall(itks,inf,_t,tk,_) => write!(f, "forall {}. {:?} :: {:?}", 
              itks.iter().map(|(i,t,k)| format!("{:?}:{:?}::{:?}",
                    i.clone().unwrap_or("_".to_string()),
                    t.clone().unwrap_or(Type::And(Vec::new())),
                    k,
              )).collect::<Vec<String>>().join(","),
              inf,
              tk,
           ),
        }
    }
}

impl TLC {
   pub fn new() -> TLC {
      TLC {
         //the first row, index 0, is nullary
         rows: vec![Row {
            term: Term::Tuple(Vec::new()),
            typ: Type::Tuple(Vec::new()), //typeof(NULL) is () not []
            kind: Kind::Nil,
            span: Span {
               filename:"".to_string(),
               offset_start: 0,
               offset_end: 0,
               linecol_start: (0,0),
               linecol_end: (0,0),
            },
            constant: None,
         }],
         rules: Vec::new(),
         scopes: Vec::new(),
         regexes: Vec::new(),
         constructors: HashMap::new(),
         foralls_index: HashMap::new(),
         foralls_rev_index: HashMap::new(),
         typedef_index: HashMap::new(),
         type_is_normal: HashSet::new(),
         kind_is_normal: HashSet::new(),
         term_kind: Kind::Simple("Term".to_string(),Vec::new()),
         nil_type: Type::Tuple(Vec::new()),
         bottom_type: Type::And(Vec::new()),
      }
   }
   pub fn print_scope(&self, s: ScopeId) -> String {
      let mut buf:String = format!("#{}{{\n", s.id);
      for (cn,pks,ct) in self.scopes[s.id].children.iter() {
         buf += &format!("\t{}: {:?} with {}\n", cn, ct,
            pks.iter().map(|(p,k)|format!("{:?}::{:?}",p,k)).collect::<Vec<String>>().join(";"));
      }
      buf += "}\n";
      buf
   }
   pub fn print_term(&self, t: TermId) -> String {
      match &self.rows[t.id].term {
         Term::Ident(x) => format!("{}", x),
         Term::Value(x) => format!("'{}'", x),
         Term::App(g,x) => format!("{}({})", self.print_term(*g), self.print_term(*x)),
         Term::Let(_sc,v,_ps,_b,_rt,_rk) => format!("let {}", v),
         Term::Ascript(t,tt) => format!("{}:{:?}", self.print_term(*t), tt),
         Term::As(t,tt) => format!("{} as {:?}", self.print_term(*t), tt),
         Term::Tuple(es) => {
            format!("({})", es.iter().filter(|e|e.id!=0).map(|e| self.print_term(*e)).collect::<Vec<String>>().join(","))
         },
         Term::Block(_,es) => {
            format!("{{{}}}", es.iter().filter(|e|e.id!=0).map(|e| self.print_term(*e)).collect::<Vec<String>>().join(";"))
         },
         Term::Constructor(cn,kvs) => {
            format!("{}{{{}}}", cn, kvs.iter().map(|(k,v)|format!("{}={}",k,self.print_term(*v))).collect::<Vec<String>>().join(","))
         },
      }
   }
   pub fn push_forall(&mut self, quants: Vec<(Option<String>,Option<Type>,Kind)>,
                             inference: Inference, term: Option<TermId>, kind: Kind, span: Span) {
      let fi = self.rules.len();
      self.rules.push(TypeRule::Forall(
         quants, inference.clone(), term, kind, span
      ));
      match &inference {
         Inference::Imply(lt,rt) => {
            let lmt = lt.mask();
            if lmt == Type::Any {
            } else if let Some(fs) = self.foralls_index.get_mut(&lmt) {
               fs.push(fi);
            } else {
               self.foralls_index.insert(lmt, vec![fi]);
            }

            let rmt = rt.mask();
            if rmt == Type::Any {
            } else if let Some(fs) = self.foralls_rev_index.get_mut(&rmt) {
               fs.push(fi);
            } else {
               self.foralls_rev_index.insert(rmt, vec![fi]);
            }
         }, _ => ()
      }
   }
   pub fn query_foralls(&self, tt: &Type) -> Vec<usize> {
      if let Some(fs) = self.foralls_index.get(&tt.mask()) {
         fs.clone()
      } else { Vec::new() }
   }
   pub fn project_kinded(&self, k: &Kind, t: &Type) -> Type {
      let ts = match t {
         Type::And(ts) => ts.clone(),
         tt => vec![tt.clone()],
      };
      let mut ats = Vec::new();
      for t in ts.iter() {
         if self.kindof(t).has(k) {
            ats.push(t.clone());
         }
      }
      if ats.len()==1 {
         ats[0].clone()
      } else {
         Type::And(ats)
      }
   }
   pub fn remove_kinded(&self, k: &Kind, t: &Type) -> Type {
      let ts = match t {
         Type::And(ts) => ts.clone(),
         tt => vec![tt.clone()],
      };
      //remove T :: K
      let ts = ts.into_iter().filter(|ct|!self.kindof(ct).has(k)).collect::<Vec<Type>>();
      Type::And(ts)
   }

   pub fn compile_str(&mut self, globals: Option<ScopeId>, src:&str) -> Result<TermId,Error> {
      self.compile_doc(globals, "[string]", src)
   }
   pub fn import_str(&mut self, globals: Option<ScopeId>, src:&str) -> Result<ScopeId,Error> {
      self.compile_doc(globals, "[string]", src)?;
      Ok(ScopeId {id:0})
   }
   pub fn import_file(&mut self, globals: Option<ScopeId>, filename:&str) -> Result<ScopeId,Error> {
      if !Path::new(filename).exists() {
         panic!("parse_file could not find file: '{}'", filename)
      }
      let src = std::fs::read_to_string(filename)
                   .expect("parse_file: Something went wrong reading the file");
      self.compile_doc(globals, filename,&src)?;
      Ok(ScopeId {id:0})
   }
   pub fn compile_doc(&mut self, globals: Option<ScopeId>, docname:&str, src:&str) -> Result<TermId,Error> {
      let ast = self.parse_doc(globals, docname, src)?;
      self.compile_rules(docname)?;
      self.typeck(globals, ast, None)?;
      self.sanityck()?;
      Ok(ast)
   }
   pub fn kind_of(&self, tt: &Type) -> Kind {
      let tn = match tt {
         Type::Ident(cn,_cts) => cn.clone(),
         _ => "".to_string(),
      };
      if let Some(ti) = self.typedef_index.get(&tn) {
      if let TypeRule::Typedef(_tn,_norm,_tps,_implies,_td,k,_props,_) = &self.rules[*ti] {
         return k.clone();
      }}
      Kind::Nil //undefined types have Nil kind
   }
   pub fn compile_rules(&mut self, _docname:&str) -> Result<(),Error> {

      //check logical consistency of foralls
      for rule in self.rules.clone().iter() { match rule {
         TypeRule::Forall(qs,inf,_t,k,sp) => {
            //check if domain is explicit
            if k != &Kind::Nil { continue; }

            //otherwise check that all variables share a domain
            let mut domains: Vec<(Type,Kind)> = Vec::new();
            for (_i,t,k) in qs.iter() {
               match t {
                  Some(tt) => domains.push((tt.clone(),k.clone())),
                  _ => domains.push((self.bottom_type.clone(),Kind::Nil))
               }
            }
            for it in inf.types().iter() {
               if !qs.iter().any(|(_i,t,_k)| &Some(it.clone())==t) {
                  domains.push((it.clone(),self.kindof(it)));
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
                  span: sp.clone(),
                  snippet: format!("{:?}", rule),
               })
            }
         },
         TypeRule::Typedef(tn,_norm,_itks,_implies,tds,_tks,props,_span) => {
            for td in tds.iter() { match td {
               Typedef::Regex(pat) => {
                  if let Ok(r) = Regex::new(&pat[1..pat.len()-1]) {
                     self.regexes.push((Type::Ident(tn.clone(),Vec::new()),r));
                  } else {
                     panic!("typedef regex rejected: {}", pat);
                  }
               },
               Typedef::Constructor(cname,kts) => {
                  self.constructors.insert(cname.clone(), (Type::Ident(tn.clone(),Vec::new()),Vec::new(),kts.clone()));
               }
            }}
            for p in props.iter() {
               if let Some(assm) = p.assm {
                  self.untyped(assm);
               }
               self.untyped(p.prop);
            }
         },
      }}

      Ok(())
   }
   pub fn parse(&mut self, src:&str) -> Result<TermId,Error> {
      //used mainly in tests
      self.parse_doc(None, "[string]", src)
   }
   pub fn parse_doc(&mut self, scope:Option<ScopeId>, docname:&str, src:&str) -> Result<TermId,Error> {
      let parse_result = TlcParser::parse(Rule::file, src);
      match parse_result {
        Ok(parse_ast) => { self.unparse_file(scope, docname, parse_ast) }
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
   pub fn unparse_file(&mut self, scope:Option<ScopeId>, fp:&str, ps: Pairs<crate::tlc::Rule>) -> Result<TermId,Error> {
      let p = ps.peek().unwrap();
      let span = Span {
         filename: fp.to_string(),
         linecol_start: p.as_span().start_pos().line_col(),
         linecol_end: p.as_span().end_pos().line_col(),
         offset_start: p.as_span().start(),
         offset_end: p.as_span().end(),
      };
      let file_scope = scope.unwrap_or(self.push_scope(Scope {
         parent: None,
         children: Vec::new(),
      }, &span));
      self.unparse_ast(file_scope, fp, p, &span)
   }
   pub fn push_term(&mut self, term: Term, span: &Span) -> TermId {
      let index = self.rows.len();
      self.rows.push(Row {
         term: term,
         typ: Type::Any,
         kind: self.term_kind.clone(),
         span: span.clone(),
         constant: None,
      });
      TermId { id: index }
   }
   pub fn push_scope(&mut self, scope: Scope, _span: &Span) -> ScopeId {
      let index = self.scopes.len();
      self.scopes.push(scope);
      ScopeId { id: index }
   }
   pub fn into_ident(&self, n: String) -> String {
      if n.starts_with("$") { n[2..n.len()-1].to_string() }
      else { n }
   }
   pub fn unparse_ast(&mut self, scope:ScopeId, fp:&str, p: Pair<crate::tlc::Rule>, span:&Span) -> Result<TermId,Error> {
      match p.as_rule() {
         //entry point rule
         Rule::file => {
            let mut es = Vec::new();
            for e in p.into_inner() { match e.as_rule() {
               Rule::EOI => (),
               _ => es.push(self.unparse_ast(scope,fp,e,span).expect("TLC Grammar Error in rule [file]"))
            }}
            Ok(self.push_term(Term::Block(scope,es), &span))
         },

         //block statement rule
         Rule::block => {
            let sid = self.push_scope(Scope {
               parent: Some(scope),
               children: Vec::new(),
            }, &span);
            let mut es = Vec::new();
            for e in p.into_inner() { match e.as_rule() {
               _ => es.push(self.unparse_ast(sid,fp,e,span).expect("TLC Grammar Error in rule [file]"))
            }}
            Ok(self.push_term(Term::Block(sid, es), &span))
         },

         //passthrough rules
         Rule::stmt => self.unparse_ast(scope,fp,p.into_inner().next().expect("TLC Grammar Error in rule [stmt]"),span),
         Rule::term => self.unparse_ast(scope,fp,p.into_inner().next().expect("TLC Grammar Error in rule [term]"),span),
         Rule::assume => self.unparse_ast(scope,fp,p.into_inner().next().expect("TLC Grammar Error in rule [assume]"),span),
         Rule::value_term => self.unparse_ast(scope,fp,p.into_inner().next().expect("TLC Grammar Error in rule [value_term]"),span),
         Rule::atom_term => self.unparse_ast(scope,fp,p.into_inner().next().expect("TLC Grammar Error in rule [atom_term]"),span),
         Rule::prefix_term => {
            let mut prefix = None;
            let mut term = None;
            for pe in p.into_inner() { match pe.as_rule() {
               Rule::prefix_op => { match pe.into_inner().concat() {
                  s if s=="+" => { prefix=Some("pos".to_string()); },
                  s if s=="-" => { prefix=Some("neg".to_string()); },
                  s => panic!("TLC Grammar Error in rule [prefix_term.0] '{}'", s),
               }},
               Rule::atom_term => { term = Some(self.unparse_ast(scope,fp,pe,span)?); },
               _ => panic!("TLC Grammar Error in rule [prefix_term.1]")
            }}
            match (prefix,term) {
               (Some(prefix),Some(term)) => {
                  let it = self.push_term(Term::Ident(prefix),&span);
                  Ok(self.push_term(Term::App(it,term),&span))
               },
               (None,Some(term)) => Ok(term),
               _ => panic!("TLC Grammar Error in rule [prefix_term.2]")
            }
         },
         Rule::infix_term => self.unparse_ast(scope,fp,p.into_inner().next().expect("TLC Grammar Error in rule [infix_term]"),span),

         //literal value rules
         Rule::ident => Ok(self.push_term(Term::Ident(self.into_ident(p.into_inner().concat())), &span)),
         Rule::constant => Ok(self.push_term(Term::Value(p.into_inner().concat()), &span)),

         //complex rules
         Rule::let_stmt => {
            let mut ps = p.into_inner();
            let ident  = self.into_ident(ps.next().expect("TLC Grammar Error in rule [let_stmt.1]").into_inner().concat());
            let mut pars: Vec<Vec<(Option<String>,Option<Type>,Kind)>> = Vec::new();
            let mut rt = self.bottom_type.clone();
            let mut rk = Kind::Nil;
            let mut t  = None;
            for e in ps { match e.as_rule() {
               Rule::let_stmt_par => {
                  let mut itks = Vec::new();
                  for itkse in e.into_inner() {
                     let mut ident = None;
                     let mut typ   = None;
                     let mut kind  = Kind::Nil;
                     for itk in itkse.into_inner() { match itk.as_rule() {
                        Rule::ident => { ident = Some(itk.into_inner().concat()); },
                        Rule::typ   => { typ   = Some(self.unparse_ast_type(scope,fp,itk,span)?); },
                        Rule::kind   => { kind = self.unparse_ast_kind(scope,fp,itk,span)?; },
                        rule => panic!("unexpected ident_typ_kind rule: {:?}", rule)
                     }}
                     itks.push((ident,typ,kind));
                  }
                  pars.push(itks);
               },
               Rule::typ => { rt = self.unparse_ast_type(scope,fp,e,span)?; },
               Rule::kind => { rk = self.unparse_ast_kind(scope,fp,e,span)?; },
               Rule::term => { t = Some(self.unparse_ast(scope,fp,e,span)?); },
               rule => panic!("unexpected let_stmt rule: {:?}", rule),
            }}
            let mut children = Vec::new();
            for itks in pars.iter() {
               for (i,t,k) in itks.iter() {
                  let t = t.clone().unwrap_or(self.bottom_type.clone());
                  let ks = vec![(t.clone(),k.clone())];
                  children.push((i.clone().unwrap_or("_".to_string()), ks, t.clone()));
               }
            }
            let mut ft = rt.clone();
            let mut fkts = Vec::new();
            for itks in pars.iter().rev() {
               let mut ps = Vec::new();
               for (_i,t,k) in itks.iter() {
                  let t = t.clone().unwrap_or(self.bottom_type.clone());
                  fkts.push((t.clone(),k.clone()));
                  ps.push(t.clone());
               }
               let pt = if ps.len()==1 {
                  ps[0].clone()
               } else {
                  Type::Tuple(ps.clone())
               };
               ft = Type::Arrow(Box::new(pt),Box::new(ft));
            }
            self.scopes[scope.id].children.push((ident.clone(), fkts, ft));
            let inner_scope = self.push_scope(Scope {
               parent: Some(scope),
               children: children,
            }, span);
            Ok(self.push_term(Term::Let(inner_scope,ident,pars,t,rt,rk), &span))
         },
         Rule::ascript_term => {
            let mut es = p.into_inner();
            let e = es.next().expect("TLC Grammar Error in rule [ascript_term]");
            match es.next() {
               None => self.unparse_ast(scope,fp,e,span),
               Some(tt) => Ok({let t = Term::Ascript(
                  self.unparse_ast(scope,fp,e,span)?, //term
                  self.unparse_ast_type(scope,fp,tt,span)? //type
               ); self.push_term(t, &span)}),
            }
         },
         Rule::as_term => {
            let mut es = p.into_inner();
            let e = es.next().expect("TLC Grammar Error in rule [as_term]");
            match es.next() {
               None => self.unparse_ast(scope,fp,e,span),
               Some(tt) => Ok({let t = Term::As(
                  self.unparse_ast(scope,fp,e,span)?, //term
                  self.unparse_ast_type(scope,fp,tt,span)? //type
               ); self.push_term(t, &span)}),
            }
         },
         Rule::app_term => {
            let mut es = p.into_inner();
            let mut g = self.unparse_ast(scope,fp,es.next().expect("TLC Grammar Error in rule [atom_term]"),span)?;
            for x in es { match x.as_rule() {
               Rule::tuple_term => {
                  g = { let f = self.unparse_ast(scope,fp,x,span)?; let t = Term::App(
                     g,
                     f,
                  ); self.push_term(t,&span)};
               },
               Rule::field_term => {
                  g = {let t = Term::App(
                     self.push_term(Term::Ident(format!(".{}", x.into_inner().concat())),&span),
                     g,
                  ); self.push_term(t,&span)};
               },
               rule => panic!("unexpected app_term rule: {:?}", rule),
            }}
            Ok(g)
         },
         Rule::divmul_term => {
            let mut es = p.into_inner();
            let mut e = self.unparse_ast(scope,fp,es.next().expect("TLC Grammar Error in rule [divmul_term.1]"),span)?;
            while let Some(op) = es.next() {
               let op = op.into_inner().concat();
               let d = self.unparse_ast(scope,fp,es.next().expect("TLC Grammar Error in rule [divmul_term.2]"),span)?;
               e = {let t = Term::App(
                  self.push_term(Term::Ident(op),span),
                  self.push_term(Term::Tuple(vec![e,d]),span),
               ); self.push_term(t,&span)};
            }
            Ok(e)
         },
         Rule::addsub_term => {
            let mut es = p.into_inner();
            let mut e = self.unparse_ast(scope,fp,es.next().expect("TLC Grammar Error in rule [addsub_term.1]"),span)?;
            while let Some(op) = es.next() {
               let op = op.into_inner().concat();
               let d = self.unparse_ast(scope,fp,es.next().expect("TLC Grammar Error in rule [addsub_term.2]"),span)?;
               e = {let t = Term::App(
                  self.push_term(Term::Ident(op),span),
                  self.push_term(Term::Tuple(vec![e,d]),span),
               ); self.push_term(t,&span)};
            }
            Ok(e)
         },
         Rule::compare_term => {
            let mut es = p.into_inner();
            let mut e = self.unparse_ast(scope,fp,es.next().expect("TLC Grammar Error in rule [compare_term.1]"),span)?;
            while let Some(op) = es.next() {
               let op = op.into_inner().concat();
               let d = self.unparse_ast(scope,fp,es.next().expect("TLC Grammar Error in rule [compare_term.2]"),span)?;
               e = {let t = Term::App(
                  self.push_term(Term::Ident(op),span),
                  self.push_term(Term::Tuple(vec![e,d]),span),
               ); self.push_term(t,&span)};
            }
            Ok(e)
         },
         Rule::logical_term => {
            let mut es = p.into_inner();
            let mut e = self.unparse_ast(scope,fp,es.next().expect("TLC Grammar Error in rule [logical_term.1]"),span)?;
            while let Some(op) = es.next() {
               let op = op.into_inner().concat();
               let d = self.unparse_ast(scope,fp,es.next().expect("TLC Grammar Error in rule [logical_term.2]"),span)?;
               e = {let t = Term::App(
                  self.push_term(Term::Ident(op),span),
                  self.push_term(Term::Tuple(vec![e,d]),span),
               ); self.push_term(t,&span)};
            }
            Ok(e)
         },
         Rule::tuple_term => {
            let es = p.into_inner().map(|e|self.unparse_ast(scope,fp,e,span).expect("TLC Grammar Error in rule [tuple_term]"))
                      .collect::<Vec<TermId>>();
            if es.len()==1 {
               Ok(es[0].clone())
            } else {
               Ok(self.push_term(Term::Tuple(es), &span))
            }
         },

         //inference rules
         Rule::typ_stmt => {
            let mut t = "".to_string();
            let mut normal = false;
            let mut implies = None;
            let mut tiks = Vec::new();
            let mut typedef = Vec::new();
            let mut kinds = Vec::new();
            let mut props = Vec::new();
            let mut constructors = Vec::new();
            for e in p.into_inner() { match e.as_rule() {
               Rule::typname => { t=e.into_inner().concat(); },
               Rule::normal => { normal=true; },
               Rule::typ_inf_kind => {
                  let mut typ = "".to_string();
                  let mut inf = None;
                  let mut kind = Kind::Nil;
                  for tik in e.into_inner() { match tik.as_rule() {
                     Rule::typvar => { typ = tik.into_inner().concat(); },
                     Rule::typ   => { inf   = Some(self.unparse_ast_type(scope,fp,tik,span)?); },
                     Rule::kind   => { kind   = self.unparse_ast_kind(scope,fp,tik,span)?; },
                     rule => panic!("unexpected ident_typ_kind rule: {:?}", rule)
                  }}
                  tiks.push((typ,inf,kind));
               },
               Rule::typ => { implies = Some(self.unparse_ast_type(scope,fp,e,span)?); },
               Rule::typedef => {
                  let struct_typ = Type::Ident(t.clone(), tiks.iter().map(|(t,_i,_k)|Type::Ident(t.clone(),Vec::new())).collect::<Vec<Type>>());
                  for tdb in e.into_inner() {
                     let mut tbs = tdb.into_inner();
                     let tbl = tbs.concat();
                     let tb = tbs.next().expect("TLC Grammar Error in rule [typedef.2]");
                     match tb.as_rule() {
                        Rule::regex => {
                           typedef.push( Typedef::Regex(tbl) );
                        },
                        Rule::constructor_typedef => {
                           let mut tcname = t.clone(); //if not provided, constructor name is same as of the type being defined
                           let mut tcrows = Vec::new();
                           for tc in tb.into_inner() { match tc.as_rule() {
                              Rule::typname => { tcname = tc.into_inner().concat(); },
                              Rule::key_typ => {
                                 let mut kts = tc.into_inner();
                                 let ki = kts.next().expect("TLC Grammar Error in rule [typedef.3]").into_inner().concat();
                                 let kt = self.unparse_ast_type(scope,fp,kts.next().expect("TLC Grammar Error in rule [typedef.4]"),span)?;
                                 self.scopes[scope.id].children.push((
                                    format!(".{}",ki.clone()),
                                    Vec::new(),
                                    Type::Arrow(Box::new(struct_typ.clone()),Box::new(kt.clone())),
                                 ));
                                 tcrows.push((ki,kt));
                              },
                              rule => panic!("unexpected constructor_typedef rule: {:?}", rule)
                           }}
                           constructors.push(tcname.clone());
                           typedef.push( Typedef::Constructor(tcname,tcrows) );
                        },
                        rule => panic!("unexpected typedef rule: {:?}", rule)
                     }
                  }
               },
               Rule::kind => { kinds.push(self.unparse_ast_kind(scope,fp,e,span)?); },
               Rule::typ_invariant => {
                  let mut itks = Vec::new();
                  let mut assm = None;
                  let mut prop = TermId { id:0 };
                  for tip in e.into_inner() { match tip.as_rule() {
                     Rule::ident_typ_kind => {
                        let mut idn = None;
                        let mut inf = None;
                        let mut kind = Kind::Nil;
                        for itk in tip.into_inner() { match itk.as_rule() {
                           Rule::ident => { idn = Some(itk.into_inner().concat()); },
                           Rule::typ   => { inf   = Some(self.unparse_ast_type(scope,fp,itk,span)?); },
                           Rule::kind   => { kind   = self.unparse_ast_kind(scope,fp,itk,span)?; },
                           rule => panic!("unexpected ident_typ_kind rule: {:?}", rule)
                        }}
                        itks.push((idn,inf,kind));
                     },
                     Rule::assume => { assm = Some(self.unparse_ast(scope,fp,tip,span)?); }
                     Rule::term => { prop = self.unparse_ast(scope,fp,tip,span)?; }
                     rule => panic!("unexpected typ_invariant rule: {:?}", rule)
                  }}
                  props.push(Invariant {
                     itks: itks,
                     assm: assm,
                     prop: prop,
                  });
               },
               rule => panic!("unexpected typ_stmt rule: {:?}", rule)
            }}
            let kinds = if kinds.len()==0 { Kind::Nil
            } else if kinds.len()==1 { kinds[0].clone()
            } else { Kind::And(kinds) };
            if normal {
               if constructors.len()==0 {
                  //constructors are preferred normal forms
                  self.type_is_normal.insert(Type::Ident(t.clone(),Vec::new()));
               }
               for k in kinds.flatten().iter() {
                  if k == &self.term_kind { continue; } //Term is never normal
                  self.kind_is_normal.insert(k.clone());
               }
            }
            self.typedef_index.insert(t.clone(), self.rules.len());
            for c in constructors.iter() {
               if normal {
                  self.type_is_normal.insert(Type::Ident(c.clone(),Vec::new()));
               }
               if &t==c { continue; } //constructor has same name as type
               self.typedef_index.insert(c.clone(), self.rules.len());
            }
            self.rules.push(TypeRule::Typedef(
               t,
               normal,
               tiks,
               implies,
               typedef,
               kinds,
               props,
               span.clone()
            ));
            Ok(TermId { id:0 })
         },

         Rule::constructor => {
            let mut ps = p.into_inner();
            let cname = ps.next().expect("TLC Grammar Error in rule [constructor], expected typname").into_inner().concat();
            let kvs = Vec::new();
            //key_value = { ident ~ "=" ~ term }
            //constructor = { typname ~ ("{" ~ (key_value ~ ("," ~ key_value)*)? ~ "}")? }
            Ok(self.push_term(Term::Constructor(
               cname,
               kvs
            ),&span))
         },
         
         Rule::forall_stmt => {
            let mut quants: Vec<(Option<String>,Option<Type>,Kind)> = Vec::new();
            let mut inference  = None;
            let mut term = None;
            let mut kind = self.term_kind.clone();
            for e in p.into_inner() { match e.as_rule() {
               Rule::ident_typ_kind => {
                  let mut ident = None;
                  let mut typ = None;
                  let mut kind = Kind::Nil;
                  for itk in e.into_inner() { match itk.as_rule() {
                     Rule::ident => { ident = Some(itk.into_inner().concat()); },
                     Rule::typ   => { typ   = Some(self.unparse_ast_type(scope,fp,itk,span)?); },
                     Rule::kind   => { kind   = self.unparse_ast_kind(scope,fp,itk,span)?; },
                     rule => panic!("unexpected ident_typ_kind rule: {:?}", rule)
                  }}
                  quants.push((ident, typ, kind));
               },
               Rule::inference => { inference = Some(self.unparse_ast_inference(scope,fp,e,span)?); }
               Rule::term => { term = Some(self.unparse_ast(scope,fp,e,span)?); }
               Rule::kind => { kind = self.unparse_ast_kind(scope,fp,e,span)?; }
               rule => panic!("unexpected typ_stmt rule: {:?}", rule)
            }}
            self.push_forall(
               quants.clone(),
               inference.expect("TLC Grammar Error in rule [forall_stmt], expected inference"),
               term,
               kind,
               span.clone(),
            );
            if let Some(t) = term {
               let mut children = Vec::new();
               for (i,t,_k) in quants.iter() {
                  children.push((i.clone().unwrap_or("_".to_string()), Vec::new(), t.clone().unwrap_or(self.bottom_type.clone())));
               }
               let sid = self.push_scope(Scope {
                  parent: Some(scope),
                  children: children,
               }, &span);
               Ok(self.push_term(Term::Let(
                 sid,
                 "".to_string(),
                 vec![quants.clone()],
                 Some(t),
                 Type::Any,
                 self.term_kind.clone(),
               ),&span))
            } else {
               Ok(TermId { id:0 })
            }
         },

         rule => panic!("unexpected expr rule: {:?}", rule)
      }
   }
   pub fn unparse_ast_inference(&mut self, scope:ScopeId, fp:&str, p: Pair<crate::tlc::Rule>, span: &Span) -> Result<Inference,Error> {
      let mut a = None;
      let mut b = None;
      for e in p.into_inner() { match e.as_rule() {
         Rule::typ => {
            if a.is_none() { a = Some(self.unparse_ast_type(scope,fp,e,span)?); }
            else { b = Some(self.unparse_ast_type(scope,fp,e,span)?); }
         },
         rule => panic!("unexpected inference rule: {:?}", rule)
      }}
      if a.is_none() { panic!("TLC Grammar Error in rule [inference]") }
      else if b.is_none() { Ok(Inference::Type(a.unwrap())) }
      else { Ok(Inference::Imply(a.unwrap(), b.unwrap())) }
   }
   pub fn unparse_ast_type(&mut self, scope:ScopeId, fp:&str, p: Pair<crate::tlc::Rule>, span: &Span) -> Result<Type,Error> {
      match p.as_rule() {
         Rule::typname => {
            let name = p.into_inner().concat();
            Ok(Type::Ident(name,Vec::new()))
         },
         Rule::typ => self.unparse_ast_type(scope,fp,p.into_inner().next().expect("TLC Grammar Error in rule [typ]"),span),
         Rule::ident_typ => {
            let mut ps = p.into_inner();
            let tn = ps.next().expect("TLC Grammar Error in rule [ident_typ.1]").into_inner().concat();
            let tps = ps.map(|e|self.unparse_ast_type(scope,fp,e,span).expect("TLC Grammar Error in rule [ident_typ.2]")).collect::<Vec<Type>>();
            Ok(Type::Ident(tn,tps))
         },
         Rule::atom_typ => self.unparse_ast_type(scope,fp,p.into_inner().next().expect("TLC Grammar Error in rule [atom_typ]"),span),
         Rule::any_typ => Ok(Type::Any),
         Rule::paren_typ => {
            let ts = p.into_inner().map(|e|self.unparse_ast_type(scope,fp,e,span).expect("TLC Grammar Error in rule [paren_typ]"))
                      .collect::<Vec<Type>>();
            if ts.len()==1 {
               Ok(ts[0].clone())
            } else {
               Ok(Type::Tuple(ts))
            }
         },
         Rule::arrow_typ => {
            let mut ts = p.into_inner();
            let mut t = self.unparse_ast_type(scope,fp,ts.next().expect("TLC Grammar Error in rule [arrow_typ]"),span)?;
            for tr in ts {
               t = Type::Arrow(
                  Box::new(t),
                  Box::new(self.unparse_ast_type(scope,fp,tr,span)?)
               );
            }
            Ok(t)
         },
         Rule::suffix_typ => {
            let mut ts = p.into_inner();
            let t = self.unparse_ast_type(scope,fp,ts.next().expect("TLC Grammar Error in rule [suffix_typ]"),span)?;
            //for t in ts {
               //TODO parameterized types and bracketed types
            //}
            Ok(t)
         },
         Rule::ratio_typ => {
            let ts = p.into_inner().map(|e|self.unparse_ast_type(scope,fp,e,span).expect("TLC Grammar Error in rule [ratio_typ.1]"))
                      .collect::<Vec<Type>>();
            if ts.len()==1 {
               Ok(ts[0].clone())
            } else if ts.len()==2 {
               Ok(Type::Ratio(Box::new(ts[0].clone()),Box::new(ts[1].clone())))
            } else {
               panic!("TLC Grammar Error in rule [ratio_typ.2]")
            }
         },
         Rule::product_typ => {
            let ts = p.into_inner().map(|e|self.unparse_ast_type(scope,fp,e,span).expect("TLC Grammar Error in rule [or_typ]"))
                      .collect::<Vec<Type>>();
            if ts.len()==1 {
               Ok(ts[0].clone())
            } else {
               Ok(Type::Product(ts))
            }
         },
         Rule::and_typ => {
            let ts = p.into_inner().map(|e|self.unparse_ast_type(scope,fp,e,span).expect("TLC Grammar Error in rule [and_typ]"))
                      .collect::<Vec<Type>>();
            if ts.len()==1 {
               Ok(ts[0].clone())
            } else {
               Ok(Type::And(ts))
            }
         },
         Rule::dep_typ => {
            let t = self.unparse_ast(scope,fp,p.into_inner().next().expect("TLC Grammar Error in rule [stmt]"),span)?;
            self.untyped(t);
            self.guard_varnames(t); //convert all varnames to "var#{term.id}"
            Ok(Type::Constant(t))
         }
         rule => panic!("unexpected typ rule: {:?}", rule)
      }
   }
   pub fn unparse_ast_kind(&mut self, scope:ScopeId, fp:&str, p: Pair<crate::tlc::Rule>, span: &Span) -> Result<Kind,Error> {
      match p.as_rule() {
         Rule::kind => {
            let mut name = "Nil".to_string();
            let mut kinds = Vec::new();
            for e in p.into_inner() { match e.as_rule() {
               Rule::kindname => { name = e.into_inner().concat(); },
               Rule::kind  => { kinds.push(self.unparse_ast_kind(scope,fp,e,span)?); },
               rule => panic!("unexpected ident_typ_kind rule: {:?}", rule)
            }}
            if name=="Nil" { Ok(Kind::Nil) }
            else { Ok(Kind::Simple(name, kinds)) }
         },
         rule => panic!("unexpected kind rule: {:?}", rule)
      }
   }
   pub fn sanityck(&mut self) -> Result<(),Error> {
      for (ri,r) in self.rows.iter().enumerate() {
         if ri==0 { continue; } //first row is nullary and not sane
         if !r.typ.is_concrete() {
            return Err(Error {
               kind: "Type Error".to_string(),
               rule: format!("inhabited type is not concrete: {:?}", r.typ),
               span: r.span.clone(),
               snippet: "".to_string()
            })
         }
         let mut rvars = r.typ.vars();
         match &r.term {
            Term::Let(_sid,_v,_pars,_t,rt,_rk) => {
               rvars.append(&mut rt.vars());
               //TODO append parameters vars
            },
            _ => (),
         }
         for tvar in rvars.iter() {
            if tvar.chars().all(char::is_uppercase) { continue; } //Type variables don't need to be defined
            if !self.typedef_index.contains_key(tvar) { return Err(Error {
               kind: "Type Error".to_string(),
               rule: format!("inhabited type is not defined: {}", tvar),
               span: r.span.clone(),
               snippet: "".to_string()
            })}
         }
      }
      Ok(())
   }
   pub fn soundck(&self, tt: &Type, span: &Span) -> Result<(),Error> {
      match tt {
         Type::Any => Ok(()),
         Type::Arrow(p,b) => { self.soundck(p,span)?; self.soundck(b,span)?; Ok(()) },
         Type::Ratio(p,b) => { self.soundck(p,span)?; self.soundck(b,span)?; Ok(()) },
         Type::And(ts) => {
            for tc in ts.iter() { self.soundck(tc,span)?; }

            //check that multiple constructors of the same type are not present
            let mut uq: HashSet<Type> = HashSet::new();
            let mut nuq: HashSet<String> = HashSet::new();
            for tc in ts.iter() {
            if let Type::Ident(tn,_ts) = tc {
               if let Some((bt,_,_)) = self.constructors.get(tn) {
                  if uq.contains(bt) && !nuq.contains(tn) { return Err(Error {
                     kind: "Type Error".to_string(),
                     rule: format!("multiple type constructors of type {:?} are present in type {:?}", bt, tt),
                     span: span.clone(),
                     snippet: "".to_string()
                  }) }
                  uq.insert(bt.clone());
                  nuq.insert(tn.clone());
               }
            }}

            Ok(())
         },
         Type::Tuple(ts) => { for tc in ts.iter() { self.soundck(tc,span)?; } Ok(()) },
         Type::Product(ts) => { for tc in ts.iter() { self.soundck(tc,span)?; } Ok(()) },
         Type::Ident(tn,ts) => {
            if ts.len()==0 { return Ok(()); }
            if !tt.is_concrete() { return Ok(()); }
            if let Some(ti) = self.typedef_index.get(tn) {
            if let TypeRule::Typedef(_tn,_norm,itks,_implies,_td,_tk,_props,_) = &self.rules[*ti] {
            if ts.len()==itks.len() {
               for (pt,(_bi,bt,_bk)) in std::iter::zip(ts,itks) {
                  if let Some(bt) = bt {
                     self.unify(pt, bt, span)?;
                  }
               }
               return Ok(());
            }}}
            Ok(())
         },
         Type::Constant(_) => Ok(()),
      }
   }
   pub fn extend_implied(&self, tt: &Type) -> Type {
      match tt {
         Type::Any => tt.clone(),
         Type::Arrow(p,b) => Type::Arrow(Box::new(self.extend_implied(p)),Box::new(self.extend_implied(b))),
         Type::Ratio(p,b) => Type::Ratio(Box::new(self.extend_implied(p)),Box::new(self.extend_implied(b))),
         Type::Ident(tn,ts) => {
            let mut implies = Vec::new();

            //lookup typedefs
            if let Some(ti) = self.typedef_index.get(tn) {
            if let TypeRule::Typedef(_tn,_norm,itks,imp,_td,_tk,_props,_) = &self.rules[*ti] {
            if ts.len()==itks.len() {
               if let Some(it) = imp {
                  match it.clone() {
                     Type::And(mut its) => { implies.append(&mut its); },
                     i => { implies.push(i); },
                  }
               }
            }}}

            //lookup constructors
            if let Some((bt,_ps,_kts)) = self.constructors.get(tn) {
               implies.push(bt.clone());
            }

            if implies.len()==0 { return tt.clone(); }
            let mut ats = Vec::new();
            ats.push(tt.clone());
            //TODO: substitute type variables in implied types
            ats.append(&mut implies.clone());
            Type::And(ats).normalize()
         },
         Type::And(ts) => {
            let mut ats = Vec::new();
            for tc in ts.iter() {
               let ct = self.extend_implied(tc);
               if let Type::And(mut cts) = ct {
                  ats.append(&mut cts);
               } else {
                  ats.push(ct);
               }
            }
            Type::And(ats)
         },
         Type::Tuple(ts) => Type::Tuple(ts.iter().map(|tc| self.extend_implied(tc)).collect::<Vec<Type>>()),
         Type::Product(ts) => Type::Product(ts.iter().map(|tc| self.extend_implied(tc)).collect::<Vec<Type>>()),
         Type::Constant(c) => Type::Constant(*c)
      }
   }
   pub fn kindsof(&self, kinds:&mut Vec<(Type,Kind)>, tt:&Type) {
      match tt {
         Type::Any => {},
         Type::Ident(_t,_ts) => {
            for (ot,_ok) in kinds.iter() {
               if ot==tt { return; } //type is already kinded
            }
            kinds.push((tt.clone(), self.kindof(tt)));
         },
         Type::And(ts) => {for t in ts.iter() { self.kindsof(kinds,t); }}
         Type::Tuple(ts) => {for t in ts.iter() { self.kindsof(kinds,t); }}
         Type::Product(ts) => {for t in ts.iter() { self.kindsof(kinds,t); }}
         Type::Arrow(p,b) => { self.kindsof(kinds,p); self.kindsof(kinds,b); }
         Type::Ratio(p,b) => { self.kindsof(kinds,p); self.kindsof(kinds,b); }
         Type::Constant(_) => {}
      }
   }
   pub fn kindof(&self, tt:&Type) -> Kind {
      match tt {
         Type::Any => Kind::Nil,
         Type::Ident(tn,ts) => {
            if let Some(ti) = self.typedef_index.get(tn) {
            if let TypeRule::Typedef(_tn,_norm,itks,_implies,_td,k,_props,_) = &self.rules[*ti] {
            if ts.len()==itks.len() {
               if k==&Kind::Nil { return self.term_kind.clone(); }
               else { return k.clone(); }
            }}}
            Kind::Nil
         },
         Type::And(ts) => {for t in ts.iter() { let k=self.kindof(t); if k!=Kind::Nil { return k; }}; Kind::Nil},
         Type::Tuple(ts) => {for t in ts.iter() { let k=self.kindof(t); if k!=Kind::Nil { return k; }}; Kind::Nil},
         Type::Product(ts) => {for t in ts.iter() { let k=self.kindof(t); if k!=Kind::Nil { return k; }}; Kind::Nil},
         Type::Arrow(p,b) => { let k=self.kindof(p); if k!=Kind::Nil { return k; } self.kindof(b) },
         Type::Ratio(p,b) => { let k=self.kindof(p); if k!=Kind::Nil { return k; } self.kindof(b) },
         Type::Constant(_) => Kind::Nil,
      }
   }
   pub fn is_knormal(&self, k:&Kind) -> bool {
      let ks = k.flatten();
      ks.iter().any(|kf| self.kind_is_normal.contains(kf))
   }
   pub fn is_normal(&self, tt:&Type) -> bool {
      match tt {
         Type::Any => false,
         Type::And(ts) => ts.iter().any(|ct|self.is_normal(ct)),
         Type::Ident(tn,ts) => self.type_is_normal.contains(&Type::Ident(tn.clone(),Vec::new())) &&
                              ts.iter().all(|ct|self.is_normal(ct)),
         Type::Tuple(ts) => ts.iter().all(|ct|self.is_normal(ct)),
         Type::Product(ts) => ts.iter().all(|ct|self.is_normal(ct)),
         Type::Arrow(p,b) => self.is_normal(p) && self.is_normal(b),
         Type::Ratio(p,b) => self.is_normal(p) && self.is_normal(b),
         Type::Constant(_) => true,
      }
   }
   pub fn typeof_var(&self, scope: &Option<ScopeId>, v: &str, implied: &Option<Type>, span: &Span) -> Result<Type,Error> {
      if let Some(scope) = scope {
         let mut candidates = Vec::new();
         let mut matches = Vec::new();
         let ref sc = self.scopes[scope.id];
         for (tn,_tkts,tt) in sc.children.iter() {
            if tn==v {
               candidates.push(tt.clone());
               if let Some(it) = implied {
                  match tt {
                     Type::Arrow(_p,_b) => {
                        //if it => tt
                        if let Ok(rt) = self.unify(&it,tt,span) {
                           matches.push(rt.clone());
                        }
                     },
                     _ => {
                        //if tt => it
                        if let Ok(rt) = self.unify(tt,&it,span) {
                           matches.push(rt.clone());
                        }
                     },
                  }
               } else {
                  matches.push(tt.clone());
               }
            }
         }
         if matches.len()>1 {
            //it is OK for multiple functions to match
            Ok(Type::And(matches).normalize())
         } else if matches.len()==1 {
            Ok(matches[0].clone())
         } else if candidates.len() > 0 { Err(Error {
            kind: "Type Error".to_string(),
            rule: format!("variable {}: {:?} did not match any candidate {}",
                     v,
                     implied.clone().unwrap_or(Type::Any),
                     candidates.iter().map(|t|format!("{:?}",t))
                               .collect::<Vec<String>>().join(" | ") ),
            span: span.clone(),
            snippet: "".to_string()
         }) } else {
            self.typeof_var(&sc.parent.clone(), v, implied, span)
         }
      } else { Err(Error {
         kind: "Type Error".to_string(),
         rule: format!("variable not found in scope: {}", v),
         span: span.clone(),
         snippet: "".to_string()
      }) }
   }
   pub fn untyped(&mut self, t: TermId) {
      self.rows[t.id].typ = self.bottom_type.clone();
      match self.rows[t.id].term.clone() {
         Term::Ident(_x) => (),
         Term::Value(_x) => (),
         Term::App(g,x) => { self.untyped(g); self.untyped(x); },
         Term::Block(_sid,es) => {
            for e in es.iter() {
               self.untyped(*e);
            }
         },
         Term::Tuple(es) => {
            for e in es.iter() {
               self.untyped(*e);
            }
         },
         Term::Let(_s,_v,_ps,b,_rt,_rk) => {
            if let Some(b) = b {
               self.untyped(b);
            }
         },
         Term::Ascript(x,_tt) => {
            self.untyped(x);
         },
         _ => panic!("TODO untype term: {}", self.print_term(t))
      }
   }
   pub fn cast_normal(&self, l_only: &Type, span: &Span) -> Result<Type,Error> {
      let mut num_collector = Vec::new();
      let mut den_collector = Vec::new();
      let (numerator,denominator) = l_only.project_ratio();

      for mut n in numerator.into_iter() {
         if self.is_normal(&n) {
            num_collector.push(n);
            continue;
         }

         if let Type::Ident(nn,nns) = &n {
            let mut nns = nns.clone();
            for ni in 0..nns.len() {
            if !self.is_normal(&nns[ni]) {
               nns[ni] = self.cast_normal(&nns[ni], span)?;
            }}
            n = Type::Ident(nn.clone(),nns);
         }

         if let Type::Ident(nn,_nns) = &n {
         if let Some(ti) = self.typedef_index.get(nn) {
         if let TypeRule::Typedef(_tn,_norm,_itks,implies,_td,_tk,_props,_span) = &self.rules[*ti] {
         let it = implies.clone().unwrap_or(Type::Any);
         if self.is_normal(&it) {
            let (inum, iden) = it.project_ratio();
            num_collector.append(&mut inum.clone());
            den_collector.append(&mut iden.clone());
            continue;
         }}}}

         let mnt = n.mask();
         let mut found = false;
         if let Some(tis) = self.foralls_index.get(&mnt) {
         for ti in tis.iter() { if !found {
         if let TypeRule::Forall(_itks,Inference::Imply(lt,rt),_term,_tk,_) = &self.rules[*ti] {
            let mut subs = Vec::new();
            if let Ok(_) = lt.unify_impl(&mut subs, &n) {
               let srt = rt.substitute(&subs);
               if self.is_normal(&srt) {
                  let (inum, iden) = srt.project_ratio();
                  num_collector.append(&mut inum.clone());
                  den_collector.append(&mut iden.clone());
                  found = true;
                  continue;
               }
            }
         }}}}
         if found { continue; }

         return Err(Error {
            kind: "Type Error".to_string(),
            rule: format!("could not normalize numerator type atom in cast {:?}", n),
            span: span.clone(),
            snippet: "".to_string()
         })
      }

      for mut d in denominator.into_iter() {
         if self.is_normal(&d) {
            den_collector.push(d.clone());
            continue;
         }

         if let Type::Ident(dn,dns) = &d {
            let mut dns = dns.clone();
            for ni in 0..dns.len() {
            if !self.is_normal(&dns[ni]) {
               dns[ni] = self.cast_normal(&dns[ni], span)?;
            }}
            d = Type::Ident(dn.clone(),dns);
         }

         if let Type::Ident(dn,_dns) = &d {
         if let Some(ti) = self.typedef_index.get(dn) {
         if let TypeRule::Typedef(_tn,_norm,_itks,implies,_td,_tk,_props,_span) = &self.rules[*ti] {
         let it = implies.clone().unwrap_or(Type::Any);
         if self.is_normal(&it) {
            let (inum, iden) = it.project_ratio();
            num_collector.append(&mut iden.clone());
            den_collector.append(&mut inum.clone());
            continue;
         }}}}

         let mdt = d.mask();
         let mut found = false;
         if let Some(tis) = self.foralls_index.get(&mdt) {
         for ti in tis.iter() { if !found {
         if let TypeRule::Forall(_itks,Inference::Imply(lt,rt),_term,_tk,_) = &self.rules[*ti] {
            let mut subs = Vec::new();
            if let Ok(_) = lt.unify_impl(&mut subs, &d) {
               let srt = rt.substitute(&subs);
               if self.is_normal(&srt) {
                  let (inum, iden) = srt.project_ratio();
                  num_collector.append(&mut iden.clone());
                  den_collector.append(&mut inum.clone());
                  found = true;
                  continue;
               }
            }
         }}}}
         if found { continue; }

         return Err(Error {
            kind: "Type Error".to_string(),
            rule: format!("could not normalize denominator type atom in cast {:?}", d),
            span: span.clone(),
            snippet: "".to_string()
         })
      }

      let num = if num_collector.len()==0 {
         Type::Tuple(Vec::new())
      } else if num_collector.len()==1 {
         num_collector[0].clone()
      } else {
         Type::Product(num_collector)
      };
      Ok(if den_collector.len()==0 {
         num
      } else if den_collector.len()==1 {
         Type::Ratio(Box::new(num),Box::new(den_collector[0].clone()))
      } else {
         let den = Type::Product(den_collector);
         Type::Ratio(Box::new(num),Box::new(den))
      })
   }
   pub fn cast_into_kind(&self, mut l_only: Type, into: &Type, span: &Span) -> Result<Type,Error> {

      while !self.is_normal(&l_only) { //kindof(Into) is normal, so all casts must go through normalization
         l_only = self.cast_normal(&l_only, span)?;
      }

      if !self.is_normal(&into) {
         let mut num_collector = Vec::new();
         let mut den_collector = Vec::new();
         let (numerator,denominator) = into.project_ratio();

         for n in numerator.iter() {
            //if Into part is normal, do nothing
            if self.is_normal(n) {
               num_collector.push(n.clone());
               continue;
            }

            //if Into part implies a normal, replace part with implied
            if let Type::Ident(nn,_nns) = n {
            if let Some(ti) = self.typedef_index.get(nn) {
            if let TypeRule::Typedef(_tn,_norm,_itks,implies,_td,_tk,_props,_span) = &self.rules[*ti] {
            let it = implies.clone().unwrap_or(Type::Any);
            if self.is_normal(&it) {
               let (inum, iden) = it.project_ratio();
               num_collector.append(&mut inum.clone());
               den_collector.append(&mut iden.clone());
               continue;
            }}}}

            //if Into part can cast into normal, replace part with cast
            let mnt = n.mask();
            let mut found = false;
            if let Some(tis) = self.foralls_index.get(&mnt) {
            for ti in tis.iter() { if !found {
            if let TypeRule::Forall(_itks,Inference::Imply(lt,rt),_term,_tk,_) = &self.rules[*ti] {
               let mut subs = Vec::new();
               if let Ok(_) = lt.unify_impl(&mut subs, &n) {
                  let srt = rt.substitute(&subs);
                  let (inum, iden) = srt.project_ratio();
                  num_collector.append(&mut inum.clone());
                  den_collector.append(&mut iden.clone());
                  found = true;
                  continue;
               }
            }}}}
            if found { continue; }

            return Err(Error {
               kind: "Type Error".to_string(),
               rule: format!("could not denormalize numerator type atom in cast {:?}", n),
               span: span.clone(),
               snippet: "".to_string()
            })
         }

         for d in denominator.iter() {
            //if Into part is normal, do nothing
            if self.is_normal(d) {
               den_collector.push(d.clone());
               continue;
            }

            //if Into part implies a normal, replace part with implied
            if let Type::Ident(dn,_dns) = d {
            if let Some(ti) = self.typedef_index.get(dn) {
            if let TypeRule::Typedef(_tn,_norm,_itks,implies,_td,_tk,_props,_span) = &self.rules[*ti] {
            let it = implies.clone().unwrap_or(Type::Any);
            if self.is_normal(&it) {
               let (inum, iden) = it.project_ratio();
               num_collector.append(&mut iden.clone());
               den_collector.append(&mut inum.clone());
               continue;
            }}}}

            //if Into part can cast into normal, replace part with cast
            let mnt = d.mask();
            let mut found = false;
            if let Some(tis) = self.foralls_index.get(&mnt) {
            for ti in tis.iter() { if !found {
            if let TypeRule::Forall(_itks,Inference::Imply(lt,rt),_term,_tk,_) = &self.rules[*ti] {
               let mut subs = Vec::new();
               if let Ok(_) = lt.unify_impl(&mut subs, &d) {
                  let srt = rt.substitute(&subs);
                  let (inum, iden) = srt.project_ratio();
                  num_collector.append(&mut iden.clone());
                  den_collector.append(&mut inum.clone());
                  found = true;
                  continue;
               }
            }}}}
            if found { continue; }

            return Err(Error {
               kind: "Type Error".to_string(),
               rule: format!("could not denormalize denominator type atom in cast {:?}", d),
               span: span.clone(),
               snippet: "".to_string()
            })
         }

         let num = if num_collector.len()==0 {
            Type::Tuple(Vec::new())
         } else if num_collector.len()==1 {
            num_collector[0].clone()
         } else {
            Type::Product(num_collector)
         };
         let b_only = if den_collector.len()==0 {
            num
         } else if den_collector.len()==1 {
            Type::Ratio(Box::new(num),Box::new(den_collector[0].clone()))
         } else {
            let den = Type::Product(den_collector);
            Type::Ratio(Box::new(num),Box::new(den))
         };

         self.unify(&l_only, &b_only, span)?;
         l_only = into.clone();
      }

      Ok(l_only)
   }

   pub fn guard_varnames(&mut self, t: TermId) {
      match self.rows[t.id].term.clone() {
         Term::Ident(_) => {
            self.rows[t.id].term = Term::Ident(format!("var#{}", t.id));
         },
         Term::Value(_) => {},
         Term::Block(_sid,es) => {
            for e in es.into_iter() {
               self.guard_varnames(e);
            }
         },
         Term::Tuple(es) => {
            for e in es.into_iter() {
               self.guard_varnames(e);
            }
         },
         Term::Let(_,_,_,_,_,_) => {
            panic!("TODO: guard_varnames in Let term")
         },
         Term::App(g,x) => {
            self.guard_varnames(g);
            self.guard_varnames(x);
         },
         Term::Ascript(t,_tt) => {
            self.guard_varnames(t);
         },
         Term::As(t,_tt) => {
            self.guard_varnames(t);
         },
         Term::Constructor(_c,kts) => {
            for (_k,t) in kts.into_iter() {
               self.guard_varnames(t);
            }
         },
      }
   }

   pub fn typeck(&mut self, scope: Option<ScopeId>, t: TermId, implied: Option<Type>) -> Result<(),Error> {
      //clone is needed to avoid double mutable borrows?
      match self.rows[t.id].term.clone() {
         Term::Block(sid,es) => {
            let mut last_typ = self.nil_type.clone();
            for e in es.iter() {
               self.typeck(Some(sid), *e, None)?;
               last_typ = self.rows[e.id].typ.clone();
            }
            self.rows[t.id].typ = self.unify(&self.rows[t.id].typ, &last_typ, &self.rows[t.id].span)?;
         },
         Term::Tuple(es) => {
            let mut ts = Vec::new();
            for e in es.iter() {
               self.typeck(scope, *e, None)?;
               ts.push(self.rows[e.id].typ.clone());
            }
            self.rows[t.id].typ = self.unify(&self.rows[t.id].typ, &Type::Tuple(ts), &self.rows[t.id].span)?;
         },
         Term::Let(s,v,_ps,b,rt,_rk) => {
            if v=="" {
               //term is untyped
               self.untyped(t);
            } else if let Some(ref b) = b {
               self.typeck(Some(s), *b, Some(rt.clone()))?;
               self.rows[t.id].typ = self.bottom_type.clone();
            } else {
               self.rows[t.id].typ = self.bottom_type.clone();
            }
            self.soundck(&rt,&self.rows[t.id].span)?;
         },
         Term::Ascript(x,tt) => {
            self.typeck(scope.clone(), x, Some(tt.clone()))?;
            self.rows[t.id].typ = self.unify(&self.rows[x.id].typ, &tt, &self.rows[t.id].span)?;
         },
         Term::As(x,into) => {
            self.typeck(scope.clone(), x, None)?;
            let into_kind = self.kindof(&into).first();
            if let Ok(nt) = self.unify(&self.rows[x.id].typ, &into, &self.rows[t.id].span) {
               //if cast is already satisfied, do nothing
               self.rows[t.id].typ = self.unify(&nt, &self.rows[t.id].typ, &self.rows[t.id].span)?;
            } else {
               if self.is_knormal(&into_kind) {
                  let l_only = self.project_kinded(&into_kind, &self.rows[x.id].typ)
                                   .remove(&into_kind.as_type());
                  let l_alts = self.remove_kinded(&into_kind, &self.rows[x.id].typ);

                  let l_only = self.cast_into_kind(l_only, &into, &self.rows[t.id].span)?;

                  //quod erat demonstrandum
                  self.rows[t.id].typ = self.unify(&l_only,&into,&self.rows[t.id].span)?.and(&l_alts);
               } else {
                  let mut accept = false;
                  for tr in self.rules.iter() { match tr {
                     TypeRule::Forall(_itks,Inference::Imply(lt,rt),_term,k,_) if k==&into_kind => {
                        if let Ok(lt) = self.unify(&self.rows[x.id].typ, lt, &self.rows[t.id].span) {
                        if let Ok(rt) = self.unify(&rt, &into, &self.rows[t.id].span) {
                           //if conversion rule matches, (L=>R), typeof(x) => L, R => Into :: kindof(Into)
                           //eliminate typeof(x) :: kindof(Into)
                           let l_narrowed = self.remove_kinded(&into_kind, &lt);
                           //introduce typeof(x) (x) R
                           let l_widened = rt.and(&l_narrowed);
                           self.rows[t.id].typ = l_widened;
                           //TODO: substitute term t into macro body if exists
                           accept = true;
                           break;
                        }}
                     }, _ => {} 
                  }}
                  if !accept {
                     return Err(Error {
                        kind: "Type Error".to_string(),
                        rule: format!("could not cast {:?} into {:?}", &self.rows[x.id].typ, &into),
                        span: self.rows[t.id].span.clone(),
                        snippet: "".to_string()
                     })
                  }
               }
            }
         },
         Term::Ident(x) => {
            let xt = self.typeof_var(&scope, &x, &implied, &self.rows[t.id].span)?;
            //typeof(x:Implied) => t.typ
            self.rows[t.id].typ = self.unify(&xt, &self.rows[t.id].typ, &self.rows[t.id].span)?;
         },
         Term::Value(x) => {
            let i = if let Some(ref i) = implied { i.clone() } else { self.bottom_type.clone() };
            let ki = self.project_kinded(&self.term_kind, &i);
            let mut r = None;
            for (pat,re) in self.regexes.iter() {
               if pat==&ki { //Term kinded is not []
                  r = Some(re);
                  self.rows[t.id].typ = self.unify(pat, &self.rows[t.id].typ, &self.rows[t.id].span)?;
                  break;
               }
               else if self.bottom_type==ki && re.is_match(&x) { //Term kinded is []
                  r = Some(re);
                  self.rows[t.id].typ = self.unify(pat, &self.rows[t.id].typ, &self.rows[t.id].span)?;
                  break;
               }
            }
            if let Some(re) = r {
               if !re.is_match(&x) {
                  return Err(Error {
                     kind: "Type Error".to_string(),
                     rule: format!("type {:?} rejected the literal {}", i, x),
                     span: self.rows[t.id].span.clone(),
                     snippet: "".to_string()
                  })
               }
               //if any non Term typ is implied, introduce it here
               let ri = self.remove_kinded(&self.term_kind, &i);
               self.rows[t.id].typ = self.rows[t.id].typ.and(&ri);
            } else {
               return Err(Error {
                  kind: "Type Error".to_string(),
                  rule: format!("type {:?} is not literal: {}", i, x),
                  span: self.rows[t.id].span.clone(),
                  snippet: "".to_string()
               })
            }
	 },
         Term::App(g,x) => {
            //covariant, contravariant matters here
            //unify(lt,rt) means lt => rt, not always rt => lt
            self.typeck(scope.clone(), x, None)?;
            self.typeck(scope.clone(), g, Some(
               Type::Arrow(Box::new(self.rows[x.id].typ.clone()),
                          Box::new(Type::Any))
            ))?;
            self.rows[x.id].typ = self.unify(&self.rows[x.id].typ, &self.rows[g.id].typ.expects(), &self.rows[x.id].span)?;
            self.rows[t.id].typ = self.unify(&self.rows[t.id].typ, &self.rows[g.id].typ.returns(), &self.rows[t.id].span)?;
         },
         Term::Constructor(cname,kvs) => {
            for (_k,v) in kvs.iter() {
               self.typeck(scope.clone(), *v, None)?;
            }
            if let Some((tt,_tpars,_tkvs)) = self.constructors.get(&cname) {
               self.rows[t.id].typ = self.unify(
                  &self.rows[t.id].typ,
                  &tt,
                  &self.rows[t.id].span
               )?;
               self.rows[t.id].typ = self.rows[t.id].typ.and(&Type::Ident(cname.clone(),Vec::new())).normalize();
            } else { return Err(Error {
               kind: "Type Error".to_string(),
               rule: format!("type constructor, none found for: {}", self.print_term(t)),
               span: self.rows[t.id].span.clone(),
               snippet: "".to_string()
            }) }
         },
      };
      if let Some(ref i) = implied {
         self.rows[t.id].typ = self.unify(&self.rows[t.id].typ, &i, &self.rows[t.id].span)?;
      };
      self.soundck(&self.rows[t.id].typ, &self.rows[t.id].span)?;
      Ok(())
   }
   pub fn unify(&self, lt: &Type, rt: &Type, span: &Span) -> Result<Type,Error> {
      //lt => rt
      let mut lt = self.extend_implied(lt); lt = lt.normalize();
      let mut rt = rt.clone(); rt = rt.normalize();
      if let Ok(tt) = lt.unify(&rt) {
         Ok(tt)
      } else { return Err(Error {
         kind: "Type Error".to_string(),
         rule: format!("failed unification {:?} (x) {:?}",lt,rt),
         span: span.clone(),
         snippet: "".to_string(),
      }) }
   }

   pub fn check(&mut self, globals: Option<ScopeId>, src:&str) -> Result<(),Error> {
      let rows_l = self.rows.len();
      let rules_l = self.rules.len();
      let scopes_l = self.scopes.len();
      let regexes_l = self.regexes.len();
      let globals_l = if let Some(g) = globals { self.scopes[g.id].children.len() } else { 0 };
      let type_is_normal_l = self.type_is_normal.clone();
      let kind_is_normal_l = self.kind_is_normal.clone();
      let typedef_index_l = self.typedef_index.clone();
      let foralls_index_l = self.foralls_index.clone();
      let foralls_rev_index_l = self.foralls_rev_index.clone();

      let r = self.compile_str(globals, src);

      self.rows.truncate(rows_l);
      self.rules.truncate(rules_l);
      self.scopes.truncate(scopes_l);
      self.regexes.truncate(regexes_l);
      if let Some(g) = globals { self.scopes[g.id].children.truncate(globals_l); };
      self.type_is_normal = type_is_normal_l;
      self.kind_is_normal = kind_is_normal_l;
      self.typedef_index = typedef_index_l;
      self.foralls_index = foralls_index_l;
      self.foralls_rev_index = foralls_rev_index_l;

      r?; Ok(())
   }
}
