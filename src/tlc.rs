use std::path::Path;
use pest::Parser;
use pest::iterators::{Pair,Pairs};
use pest::error::{ErrorVariant,InputLocation,LineColLocation};
use regex::Regex;

#[derive(Parser)]
#[grammar = "grammar_tlc.pest"]
struct TlcParser;

pub struct TLC {
   pub rows: Vec<Row>,
   pub rules: Vec<TypeRule>,
   pub scopes: Vec<Scope>,
   pub regexes: Vec<(Typ,Regex)>,
   pub constructors: Vec<(Typ,String,Vec<Typ>,Vec<(String,Typ)>)>,
   pub ident_regex: Regex,
   pub tvar_regex: Regex,
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
   pub children: Vec<(String,Typ)>,
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
impl Typ {
   fn returns(&self) -> Typ {
      match self {
         Typ::Arrow(_p,b) => *b.clone(),
         _ => Typ::And(Vec::new()), //absurd
      }
   }
   fn vars(&self) -> Vec<String> {
      match self {
         Typ::Nil => vec![],
         Typ::Any => vec![],
         Typ::Or(_ts) => vec![],
         Typ::Ident(tn,ts) => {
            let mut nv = vec![tn.clone()];
            for tt in ts.iter() {
               nv.append(&mut tt.vars());
            }
            nv
         }
         Typ::Arrow(p,b) => { let mut pv=p.vars(); pv.append(&mut b.vars()); pv },
         Typ::Ratio(p,b) => { let mut pv=p.vars(); pv.append(&mut b.vars()); pv },
         Typ::And(ts) => {
            let mut nv = Vec::new();
            for tt in ts.iter() {
               nv.append(&mut tt.vars());
            }
            nv
         }
         Typ::Tuple(ts) => {
            let mut nv = Vec::new();
            for tt in ts.iter() {
               nv.append(&mut tt.vars());
            }
            nv
         }
         Typ::Product(ts) => {
            let mut nv = Vec::new();
            for tt in ts.iter() {
               nv.append(&mut tt.vars());
            }
            nv
         }
      }
   }
   fn is_concrete(&self) -> bool {
      match self {
         Typ::Nil => true,
         Typ::Any => false,
         Typ::Or(_ts) => false,
         Typ::Arrow(p,b) => p.is_concrete() && b.is_concrete(),
         Typ::Ratio(p,b) => p.is_concrete() && b.is_concrete(),
         Typ::Ident(_tn,ts) => ts.iter().all(|tc| tc.is_concrete()),
         Typ::And(ts) => ts.len()>0 && ts.iter().all(|tc| tc.is_concrete()),
         Typ::Tuple(ts) => ts.iter().all(|tc| tc.is_concrete()),
         Typ::Product(ts) => ts.iter().all(|tc| tc.is_concrete()),
      }
   }
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
pub fn unify(lt: &Typ, rt: &Typ, span: &Span) -> Result<Typ,Error> {
   match (lt,rt) {
      (Typ::Any,r) => Ok(r.clone()),
      (l,Typ::Any) => Ok(l.clone()),
      (Typ::Nil,Typ::Nil) => Ok(lt.clone()),
      (Typ::Ident(lv,lps),Typ::Ident(rv,rps))
      if lv==rv && lps.len()==rps.len() => {
         let mut tps = Vec::new();
         for (lp,rp) in std::iter::zip(lps,rps) {
            tps.push(unify(lp,rp,span)?);
         }
         Ok(Typ::Ident(lv.clone(),tps))
      }
      (Typ::Arrow(pl,bl),Typ::Arrow(pr,br)) => {
         let pt = unify(pl,pr,span)?;
         let bt = unify(bl,br,span)?;
         Ok(Typ::Arrow(Box::new(pt),Box::new(bt)))
      },
      (Typ::Ratio(pl,bl),Typ::Ratio(pr,br)) => {
         let pt = unify(pl,pr,span)?;
         let bt = unify(bl,br,span)?;
         Ok(Typ::Ratio(Box::new(pt),Box::new(bt)))
      },
      (Typ::Product(la),Typ::Product(ra)) => {
         let mut ts = Vec::new();
         for (lt,rt) in std::iter::zip(la,ra) {
            ts.push(unify(lt,rt,span)?);
         }
         Ok(Typ::Product(ts))
      },
      (Typ::And(la),Typ::And(ra)) => {
         let mut ts = Vec::new();
         for (lt,rt) in std::iter::zip(la,ra) {
            ts.push(unify(lt,rt,span)?);
         }
         Ok(Typ::And(ts))
      },
      (l,r) => {
         Err(Error {
            kind: "Type Error".to_string(),
            rule: format!("failed unification {:?} (x) {:?}",l,r),
            span: span.clone(),
            snippet: "".to_string(),
         })
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
pub enum Typedef {
   Regex(String),
   Constructor(String,Vec<(String,Typ)>),
}

#[derive(Clone)]
pub enum TypeRule {
   Typedef(String,Vec<(Option<String>,Option<Typ>,Option<Kind>)>,Option<Typ>,Vec<Typedef>,Option<Kind>,Span),

   Forall(Vec<(Option<String>,Option<Typ>,Option<Kind>)>, Inference, Option<TermId>, Option<Kind>,Span),
}
impl TypeRule {
   pub fn span(&self) -> Span {
      match self {
         TypeRule::Typedef(_,_,_,_,_,sp) => sp.clone(),
         TypeRule::Forall(_,_,_,_,sp) => sp.clone(),
      }
   }
}
impl std::fmt::Debug for TypeRule {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
           TypeRule::Typedef(tn,itks,implies,_td,tk,_) => write!(f, "{}{}{}{}{}::{:?}",
              tn,
              if itks.len()==0 { "" } else { "<" },
              itks.iter().map(|(i,t,k)| format!("{:?}:{:?}::{:?}",
                    i.clone().unwrap_or("_".to_string()),
                    t.clone().unwrap_or(Typ::Nil),
                    k.clone().unwrap_or(Kind::Nil),
              )).collect::<Vec<String>>().join(","),
              if itks.len()==0 { "" } else { ">" },
              if let Some(ti) = implies { format!(":{:?}",ti) } else { format!("") },
              tk
           ),
           TypeRule::Forall(itks,inf,_t,tk,_) => write!(f, "forall {}. {:?} :: {:?}", 
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
#[derive(Clone)] //clone seems to be needed to deconflict mutable borrows :(
pub enum Term {
   Nil,
   Ident(String),
   App(TermId,TermId),
   Let(ScopeId,String,Vec<Vec<(Option<String>,Option<Typ>,Option<Kind>)>>,Option<TermId>,Typ,Kind),
   Tuple(Vec<TermId>),
   Block(ScopeId,Vec<TermId>),
   Ascript(TermId,Typ),
   Constructor(String,Vec<(String,TermId)>),
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
         regexes: Vec::new(),
         constructors: Vec::new(),
         ident_regex: Regex::new("^[a-z][_0-9a-zA-Z]*$").expect("Failed to compile ident_regex in TLC initialization"),
         tvar_regex: Regex::new("^[A-Z]+$").expect("Failed to compile tvar_regex in TLC initialization"),
      }
   }
   pub fn print_scope(&self, s: ScopeId) -> String {
      let mut buf:String = format!("#{}{{", s.id);
      for (cn,ct) in self.scopes[s.id].children.iter() {
         buf += &format!("{}: {:?}\n", cn, ct);
      }
      buf += "}\n";
      buf
   }
   pub fn print_term(&self, t: TermId) -> String {
      match &self.rows[t.id].term {
         Term::Nil => format!("()"),
         Term::Ident(x) => format!("{}", x),
         Term::App(g,x) => format!("{}({})", self.print_term(*g), self.print_term(*x)),
         Term::Let(_sc,v,_ps,_b,_rt,_rk) => format!("let {}", v),
         Term::Ascript(t,tt) => format!("{}:{:?}", self.print_term(*t), tt),
         Term::Tuple(es) => {
            format!("({})", es.iter().filter(|e|e.id!=0).map(|e| self.print_term(*e)).collect::<Vec<String>>().join(","))
         },
         Term::Block(_,es) => {
            format!("{{{}}}", es.iter().filter(|e|e.id!=0).map(|e| self.print_term(*e)).collect::<Vec<String>>().join(";"))
         },
         Term::Constructor(cn,_kvs) => {
            format!("{}{{}}", cn)
         },
      }
   }
   pub fn project_kinded_type(&mut self, k: &Kind, t: &Typ) -> Typ {
      let ts = match t {
         Typ::And(ts) => ts.clone(),
         tt => vec![tt.clone()],
      };
      for t in ts.iter() {
         let ts = format!("{:?}",t);
         for tr in self.rules.iter() { match tr {
            TypeRule::Typedef(tn,_itks,_implies,_td,tk,_) => {
               if &ts!=tn { continue; }
               let tk = tk.clone().unwrap_or(Kind::Simple("Term".to_string(),Vec::new()));
               if &tk==k { return t.clone(); }
               //TODO: kind-project parameterized types
            },
            _ => ()
         }}
      }
      Typ::Nil
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
   pub fn compile_doc(&mut self, globals: Option<ScopeId>, docname:&str, src:&str) -> Result<TermId,Error> {
      let ast = self.parse_doc(docname, src)?;
      self.compile_rules(docname)?;
      self.typecheck(globals, ast, None)?;
      self.sanitycheck()?;
      Ok(ast)
   }
   pub fn kind_of(&self, t: &Typ) -> Kind {
      for rule in self.rules.iter() { match rule {
         TypeRule::Typedef(tt,_tps,_implies,_td,k,_) => { if &format!("{:?}",t)==tt {
            return k.clone().unwrap_or(Kind::Simple("Term".to_string(),Vec::new()));
         }},
         _ => ()
      }}
      Kind::Nil //undefined types have Nil kind
   }
   pub fn compile_rules(&mut self, _docname:&str) -> Result<(),Error> {

      //check logical consistency of foralls
      for rule in self.rules.iter() { match rule {
         TypeRule::Forall(qs,inf,_t,k,sp) => {
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
                  let tk = qs.iter().fold(None, |lk,(_i,t,tk)| {
                     if &Some(it.clone())==t {tk.clone()} else {lk}
                  });
                  domains.push((it.clone(),tk.clone().unwrap_or(self.kind_of(it))));
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
                  snippet: sp.snippet(),
               })
            }
         },
         TypeRule::Typedef(tn,_itks,_implies,tds,_tk,_span) => {
            for td in tds.iter() { match td {
               Typedef::Regex(pat) => {
                  if let Ok(r) = Regex::new(&pat[1..pat.len()-1]) {
                     self.regexes.push((Typ::Ident(tn.clone(),Vec::new()),r));
                  } else {
                     panic!("typedef regex rejected: {}", pat);
                  }
               },
               Typedef::Constructor(cname,kts) => {
                  self.constructors.push((Typ::Ident(tn.clone(),Vec::new()),cname.clone(),Vec::new(),kts.clone()));
               }
            }}
         },
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
      let p = ps.peek().unwrap();
      let span = Span {
         filename: fp.to_string(),
         linecol_start: p.as_span().start_pos().line_col(),
         linecol_end: p.as_span().end_pos().line_col(),
         offset_start: p.as_span().start(),
         offset_end: p.as_span().end(),
      };
      let file_scope = self.push_scope(Scope {
         parent: None,
         children: Vec::new(),
      }, &span);
      self.unparse_ast(file_scope, fp, p, &span)
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
         Rule::value_term => self.unparse_ast(scope,fp,p.into_inner().next().expect("TLC Grammar Error in rule [value_term]"),span),
         Rule::atom_term => self.unparse_ast(scope,fp,p.into_inner().next().expect("TLC Grammar Error in rule [atom_term]"),span),
         Rule::prefix_term => self.unparse_ast(scope,fp,p.into_inner().next().expect("TLC Grammar Error in rule [prefix_term]"),span),
         Rule::infix_term => self.unparse_ast(scope,fp,p.into_inner().next().expect("TLC Grammar Error in rule [infix_term]"),span),

         //literal value rules
         Rule::ident => Ok(self.push_term(Term::Ident(self.into_ident(p.into_inner().concat())), &span)),
         Rule::constant => Ok(self.push_term(Term::Ident(p.into_inner().concat()), &span)),

         //complex rules
         Rule::let_stmt => {
            let mut ps = p.into_inner();
            let ident  = self.into_ident(ps.next().expect("TLC Grammar Error in rule [let_stmt.1]").into_inner().concat());
            let mut pars: Vec<Vec<(Option<String>,Option<Typ>,Option<Kind>)>> = Vec::new();
            let mut rt = Typ::Nil;
            let mut rk = Kind::Nil;
            let mut t  = None;
            for e in ps { match e.as_rule() {
               Rule::let_stmt_par => {
                  let mut itks = Vec::new();
                  for itkse in e.into_inner() {
                     let mut ident = None;
                     let mut typ   = None;
                     let mut kind  = None;
                     for itk in itkse.into_inner() { match itk.as_rule() {
                        Rule::ident => { ident = Some(itk.into_inner().concat()); },
                        Rule::typ   => { typ   = Some(self.unparse_ast_typ(itk)?); },
                        Rule::kind   => { kind = Some(self.unparse_ast_kind(itk)?); },
                        rule => panic!("unexpected ident_typ_kind rule: {:?}", rule)
                     }}
                     itks.push((ident,typ,kind));
                  }
                  pars.push(itks);
               },
               Rule::typ => { rt = self.unparse_ast_typ(e)?; },
               Rule::kind => { rk = self.unparse_ast_kind(e)?; },
               Rule::term => { t = Some(self.unparse_ast(scope,fp,e,span)?); },
               rule => panic!("unexpected let_stmt rule: {:?}", rule),
            }}
            let mut children = Vec::new();
            for itks in pars.iter() {
               for (i,t,_k) in itks.iter() {
                  children.push((i.clone().unwrap_or("_".to_string()), t.clone().unwrap_or(Typ::Nil)));
               }
            }
            self.scopes[scope.id].children.push((ident.clone(), rt.clone())); //TODO: add gathered type signature for function definitions
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
                  self.unparse_ast_typ(tt)? //type
               ); self.push_term(t, &span)}),
            }
         },
         Rule::app_term => {
            let mut es = p.into_inner();
            let mut e = self.unparse_ast(scope,fp,es.next().expect("TLC Grammar Error in rule [atom_term]"),span)?;
            for args in es {
               e = {let t = Term::App(
                  e,
                  self.unparse_ast(scope,fp,args,span)?
               ); self.push_term(t,&span)};
            }
            Ok(e)
         },
         Rule::divmul_term => {
            let mut es = p.into_inner();
            let e = self.unparse_ast(scope,fp,es.next().expect("TLC Grammar Error in rule [divmul_term]"),span)?;
            //TODO: combine terms
            Ok(e)
         },
         Rule::addsub_term => {
            let mut es = p.into_inner();
            let e = self.unparse_ast(scope,fp,es.next().expect("TLC Grammar Error in rule [addsub_term]"),span)?;
            //TODO: combine terms
            Ok(e)
         },
         Rule::tuple_term => {
            let es = p.into_inner().map(|e|self.unparse_ast(scope,fp,e,span).expect("TLC Grammar Error in rule [tuple_term]"))
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
            let mut implies = None;
            let mut itks = Vec::new();
            let mut typedef = Vec::new();
            let mut kind = None;
            for e in ps { match e.as_rule() {
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
                  itks.push((ident,typ,kind));
               },
               Rule::typ => { implies = Some(self.unparse_ast_typ(e)?); },
               Rule::typedef => {
 
                  for tdb in e.into_inner() {
                     let mut tbs = tdb.into_inner();
                     let tbl = tbs.concat();
                     let tb = tbs.next().expect("TLC Grammar Error in rule [typedef.2]");
                     match tb.as_rule() {
                        Rule::regex => {
                           typedef.push( Typedef::Regex(tbl) );
                        },
                        Rule::typname => {
                           typedef.push( Typedef::Constructor(tbl,Vec::new()) );
                        },
                        rule => panic!("unexpected typedef rule: {:?}", rule)
                     }
                  }
               },
               Rule::kind => { kind = Some(self.unparse_ast_kind(e)?); },
               rule => panic!("unexpected typ_stmt rule: {:?}", rule)
            }}
            self.rules.push(TypeRule::Typedef(
               t,
               itks,
               implies,
               typedef,
               kind,
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
            let mut quants: Vec<(Option<String>,Option<Typ>,Option<Kind>)> = Vec::new();
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
               Rule::term => { term = Some(self.unparse_ast(scope,fp,e,span)?); }
               Rule::kind => { kind = Some(self.unparse_ast_kind(e)?); }
               rule => panic!("unexpected typ_stmt rule: {:?}", rule)
            }}
            self.rules.push(TypeRule::Forall(
               quants.clone(),
               inference.expect("TLC Grammar Error in rule [forall_stmt], expected inference"),
               term,
               kind,
               span.clone(),
            ));
            if let Some(t) = term {
               let mut children = Vec::new();
               for (i,t,_k) in quants.iter() {
                  children.push((i.clone().unwrap_or("_".to_string()), t.clone().unwrap_or(Typ::Nil)));
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
                 Typ::Any,
                 Kind::Simple("Term".to_string(),Vec::new()),
               ),&span))
            } else {
               Ok(TermId { id:0 })
            }
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
         Rule::typname => {
            let name = p.into_inner().concat();
            Ok(Typ::Ident(name,Vec::new()))
         },
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
   pub fn sanitycheck(&mut self) -> Result<(),Error> {
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
            if self.tvar_regex.is_match(tvar) { continue; } //Type variables don't need to be defined
            let mut defined = false;
            for rule in self.rules.iter() { match rule {
               TypeRule::Typedef(tt,_tps,_implies,_td,_k,_) => { if tvar==tt {
                  defined=true;
               }},
               _ => ()
            }}
            if !defined { return Err(Error {
               kind: "Type Error".to_string(),
               rule: format!("inhabited type is not defined: {}", tvar),
               span: r.span.clone(),
               snippet: "".to_string()
            })}
         }
      }
      Ok(())
   }
   pub fn typeof_var(&self, scope: &Option<ScopeId>, v: &str, span: &Span) -> Result<Typ,Error> {
      if let Some(scope) = scope {
         let ref sc = self.scopes[scope.id];
         for (tn,tt) in sc.children.iter() {
            if tn==v { return Ok(tt.clone()) }
         }
         self.typeof_var(&sc.parent.clone(), &v, span)
      } else { Err(Error {
         kind: "Type Error".to_string(),
         rule: format!("variable not found in scope: {}", v),
         span: span.clone(),
         snippet: "".to_string()
      }) }
   }
   pub fn untyped(&mut self, t: TermId) {
      self.rows[t.id].typ = Typ::Nil;
      match self.rows[t.id].term.clone() {
         Term::Nil => (),
         Term::Ident(_x) => (),
         Term::Block(_sid,es) => {
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
   pub fn typecheck(&mut self, scope: Option<ScopeId>, t: TermId, implied: Option<Typ>) -> Result<(),Error> {
      //clone is needed to avoid double mutable borrows?
      match self.rows[t.id].term.clone() {
         Term::Nil => {
            self.rows[t.id].typ = unify(&self.rows[t.id].typ, &Typ::Nil, &self.rows[t.id].span)?;
         },
         Term::Block(sid,es) => {
            let mut last_typ = Typ::Nil;
            for e in es.iter() {
               self.typecheck(Some(sid), *e, None)?;
               last_typ = self.rows[e.id].typ.clone();
            }
            self.rows[t.id].typ = unify(&self.rows[t.id].typ, &last_typ, &self.rows[t.id].span)?;
         },
         Term::Let(s,v,_ps,b,rt,_rk) => {
            if v=="" {
               //term is untyped
               self.untyped(t);
            } else if let Some(ref b) = b {
               self.typecheck(Some(s), *b, Some(rt.clone()))?;
               self.rows[t.id].typ = Typ::Nil;
            } else {
               self.rows[t.id].typ = Typ::Nil;
            }
         },
         Term::Ascript(x,tt) => {
            self.typecheck(scope.clone(), x, Some(tt.clone()))?;
            self.rows[t.id].typ = unify(&self.rows[t.id].typ, &self.rows[x.id].typ, &self.rows[t.id].span)?;
         },
         Term::Ident(x) => {
            if self.ident_regex.is_match(&x) {
               let span = self.rows[t.id].span.clone();
               let xt = self.typeof_var(&scope, &x, &span)?;
               self.rows[t.id].typ = unify(&self.rows[t.id].typ, &xt, &self.rows[t.id].span)?;
            } else if let Some(ref i) = implied {
               let i = self.project_kinded_type(&Kind::Simple("Term".to_string(),Vec::new()), i);
               let mut r = None;
               for (pat,re) in self.regexes.iter() {
                  if pat==&i { r=Some(re); break; }
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
               } else {
                  return Err(Error {
                     kind: "Type Error".to_string(),
                     rule: format!("type {:?} is not literal: {}", i, x),
                     span: self.rows[t.id].span.clone(),
                     snippet: "".to_string()
                  })
               }
            }
	 },
         Term::App(g,x) => {
            self.typecheck(scope.clone(), x, None)?;
            self.typecheck(scope.clone(), g, Some(
               Typ::Arrow(Box::new(self.rows[x.id].typ.clone()),
                          Box::new(implied.clone().unwrap_or(Typ::Any)))
            ))?;
            self.rows[t.id].typ = unify(&self.rows[t.id].typ, &self.rows[g.id].typ.returns(), &self.rows[t.id].span)?;
         },
         Term::Constructor(cname,kvs) => {
            for (_k,v) in kvs.iter() {
               self.typecheck(scope.clone(), *v, None)?;
            }
            let mut found = false;
            for (tt, tname,_tpars,_tkvs) in self.constructors.iter() {
               if &cname==tname {
                  self.rows[t.id].typ = unify(
                     &self.rows[t.id].typ,
                     &tt,
                     &self.rows[t.id].span
                  )?;
                  found = true;
                  break;
               }
            }
            if !found { return Err(Error {
               kind: "Type Error".to_string(),
               rule: format!("type constructor, none found for: {}", self.print_term(t)),
               span: self.rows[t.id].span.clone(),
               snippet: "".to_string()
            }) }
         },
         _ => panic!("TODO typecheck term: {}", self.print_term(t))
      };
      if let Some(ref i) = implied {
         self.rows[t.id].typ = unify(&self.rows[t.id].typ, &i, &self.rows[t.id].span)?;
      };
      Ok(())
   }
   pub fn check(&mut self, globals: Option<ScopeId>, src:&str) -> Result<(),Error> {
      let rows_l = self.rows.len();
      let rules_l = self.rules.len();
      let scopes_l = self.scopes.len();
      let regexes_l = self.regexes.len();

      let r = self.compile_str(globals, src);

      self.rows.truncate(rows_l);
      self.rules.truncate(rules_l);
      self.scopes.truncate(scopes_l);
      self.regexes.truncate(regexes_l);

      r?; Ok(())
   }
}
