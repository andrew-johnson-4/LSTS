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
   pub term_kind: Kind,
   pub nil_type: Typ,
   pub bottom_type: Typ,
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
   pub children: Vec<(String,Vec<(Typ,Kind)>,Typ)>,
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
   Any,
   Ident(String,Vec<Typ>),
   Or(Vec<Typ>),
   And(Vec<Typ>), //Bottom is the empty conjunctive
   Arrow(Box<Typ>,Box<Typ>),
   Tuple(Vec<Typ>),   //Tuple is order-sensitive, Nil is the empty tuple
   Product(Vec<Typ>), //Product is order-insensitive
   Ratio(Box<Typ>,Box<Typ>),
}
impl Typ {
   fn and(&self, other:&Typ) -> Typ {
      match (self,other) {
         (Typ::And(ls),Typ::And(rs)) => {
            let mut ts = ls.clone();
            ts.append(&mut rs.clone());
            Typ::And(ts)
         },
         (Typ::And(ls),r) => {
            let mut ts = ls.clone();
            ts.push(r.clone());
            Typ::And(ts)
         }
         (l,Typ::And(rs)) => {
            let mut ts = rs.clone();
            ts.push(l.clone());
            Typ::And(ts)
         },
         (l,r) => {
            Typ::And(vec![l.clone(),r.clone()])
         }
      }
   }
   fn expects(&self) -> Typ {
      match self {
         Typ::Arrow(p,_b) => *p.clone(),
         _ => Typ::And(Vec::new()), //absurd
      }
   }
   fn returns(&self) -> Typ {
      match self {
         Typ::Arrow(_p,b) => *b.clone(),
         _ => Typ::And(Vec::new()), //absurd
      }
   }
   fn vars(&self) -> Vec<String> {
      match self {
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
   fn normalize(&mut self) {
      match self {
         Typ::Or(ts) => { for t in ts.iter_mut() { t.normalize(); } ts.sort(); ts.dedup(); }
         Typ::And(ts) => { for t in ts.iter_mut() { t.normalize(); } ts.sort(); ts.dedup(); }
         Typ::Product(ts) => { for t in ts.iter_mut() { t.normalize(); } ts.sort(); ts.dedup(); }
         Typ::Tuple(ts) => { for t in ts.iter_mut() { t.normalize(); } }
         Typ::Arrow(p,b) => { p.normalize(); b.normalize(); }
         Typ::Ratio(p,b) => { p.normalize(); b.normalize(); }
         _ => (),
      }
   }
   fn substitute(&self, subs:&Vec<(Typ,Typ)>) -> Typ {
      for (lt,rt) in subs.iter() {
         if self==lt { return rt.clone(); }
      }
      match self {
         Typ::Or(ts) => Typ::Or(ts.iter().map(|t| t.substitute(subs)).collect::<Vec<Typ>>()),
         Typ::And(ts) => Typ::And(ts.iter().map(|t| t.substitute(subs)).collect::<Vec<Typ>>()),
         Typ::Tuple(ts) => Typ::Tuple(ts.iter().map(|t| t.substitute(subs)).collect::<Vec<Typ>>()),
         Typ::Product(ts) => Typ::Product(ts.iter().map(|t| t.substitute(subs)).collect::<Vec<Typ>>()),
         Typ::Arrow(p,b) => Typ::Arrow(Box::new(p.substitute(subs)),Box::new(b.substitute(subs))),
         Typ::Ratio(p,b) => Typ::Ratio(Box::new(p.substitute(subs)),Box::new(b.substitute(subs))),
         _ => self.clone(),
      }
   }
   fn is_concrete(&self) -> bool {
      match self {
         Typ::Any => false,
         Typ::Or(_ts) => false,
         Typ::Arrow(p,b) => p.is_concrete() && b.is_concrete(),
         Typ::Ratio(p,b) => p.is_concrete() && b.is_concrete(),
         Typ::Ident(_tn,ts) => ts.iter().all(|tc| tc.is_concrete()),
         Typ::And(ts) => ts.iter().all(|tc| tc.is_concrete()), //bottom type is also concrete
         Typ::Tuple(ts) => ts.iter().all(|tc| tc.is_concrete()),
         Typ::Product(ts) => ts.iter().all(|tc| tc.is_concrete()),
      }
   }
}
impl std::fmt::Debug for Typ {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
           Typ::Any => write!(f, "?"),
           Typ::Ident(t,ts) => {
              if ts.len()==0 { write!(f, "{}", t) }
              else { write!(f, "{}<{}>", t, ts.iter().map(|t|format!("{:?}",t)).collect::<Vec<String>>().join(",") ) }
           }
           Typ::Or(ts) => write!(f, "({})", ts.iter().map(|t|format!("{:?}",t)).collect::<Vec<String>>().join("|") ),
           Typ::And(ts) => write!(f, "[{}]", ts.iter().map(|t|format!("{:?}",t)).collect::<Vec<String>>().join("+") ),
           Typ::Tuple(ts) => write!(f, "({})", ts.iter().map(|t|format!("{:?}",t)).collect::<Vec<String>>().join(",") ),
           Typ::Product(ts) => write!(f, "({})", ts.iter().map(|t|format!("{:?}",t)).collect::<Vec<String>>().join("*") ),
           Typ::Arrow(p,b) => write!(f, "({:?})=>({:?})", p, b),
           Typ::Ratio(n,d) => write!(f, "({:?})/({:?})", n, d),
        }
    }
}
fn unify_impl(subs: &mut Vec<(Typ,Typ)>, lt: &Typ, rt: &Typ, span: &Span) -> Result<Typ,()> {
   //lt => rt
   match (lt,rt) {
      //wildcard match
      (Typ::Any,r) => Ok(r.clone()),
      (l,Typ::Any) => Ok(l.clone()),
      (Typ::Ident(lv,_lps),rt) if lv.chars().all(char::is_uppercase) => {
         for (sl,sr) in subs.clone().iter() {
            if lt==sl { return unify_impl(subs,sr,rt,span); }
         }
         subs.push((lt.clone(),rt.clone()));
         Ok(rt.clone())
      },
      (lt,Typ::Ident(rv,_rps)) if rv.chars().all(char::is_uppercase) => {
         for (sl,sr) in subs.clone().iter() {
            if rt==sl { return unify_impl(subs,lt,sr,span); }
         }
         subs.push((rt.clone(),lt.clone()));
         Ok(lt.clone())
      },

      //conjunctive normal form takes precedence
      (Typ::And(lts),Typ::And(ra)) => {
         //lt => rt
         let mut lts = lts.clone();
         for rt in ra.iter() {
            lts.push(unify_impl(subs,lt,rt,span)?);
         }
         Ok(Typ::And(lts))
      },
      (Typ::And(lts),rt) => {
         let mut lts = lts.clone();
         let mut accept = false;
         for lt in lts.clone().iter() {
            if let Ok(nt) = unify_impl(subs,lt,rt,span) {
               accept = true;
               lts.push(nt);
            }
         }
         if accept {
            Ok(Typ::And(lts))
         } else {
            Err(())
         }
      },

      //ratio types have next precedence
      (Typ::Ratio(pl,bl),Typ::Ratio(pr,br)) => {
         let pt = unify_impl(subs,pl,pr,span)?;
         let bt = unify_impl(subs,bl,br,span)?;
         Ok(Typ::Ratio(Box::new(pt),Box::new(bt)))
      },
      (Typ::Ratio(pl,bl),r) => {
         let pt = unify_impl(subs,pl,r,span)?;
         Ok(Typ::Ratio(Box::new(pt),Box::new(*bl.clone())))
      },
      (l,Typ::Ratio(pr,br)) => {
         let pt = unify_impl(subs,l,pr,span)?;
         Ok(Typ::Ratio(Box::new(pt),Box::new(*br.clone())))
      },

      //everything else is a mixed bag
      (Typ::Ident(lv,lps),Typ::Ident(rv,rps))
      if lv==rv && lps.len()==rps.len() => {
         let mut tps = Vec::new();
         for (lp,rp) in std::iter::zip(lps,rps) {
            tps.push(unify_impl(subs,lp,rp,span)?);
         }
         Ok(Typ::Ident(lv.clone(),tps))
      }
      (Typ::Arrow(pl,bl),Typ::Arrow(pr,br)) => {
         let pt = unify_impl(subs,pl,pr,span)?;
         let bt = unify_impl(subs,bl,br,span)?;
         Ok(Typ::Arrow(Box::new(pt),Box::new(bt)))
      },
      (Typ::Product(la),Typ::Product(ra)) if la.len()==ra.len() => {
         let mut ts = Vec::new();
         for (lt,rt) in std::iter::zip(la,ra) {
            ts.push(unify_impl(subs,lt,rt,span)?);
         }
         Ok(Typ::Product(ts))
      },
      (Typ::Tuple(la),Typ::Tuple(ra)) if la.len()==ra.len() => {
         let mut ts = Vec::new();
         for (lt,rt) in std::iter::zip(la,ra) {
            ts.push(unify_impl(subs,lt,rt,span)?);
         }
         Ok(Typ::Tuple(ts))
      },
      _ => Err(()),
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
   Typedef(String,Vec<(String,Option<Typ>,Option<Kind>)>,Option<Typ>,Vec<Typedef>,Option<Kind>,Span),

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
              itks.iter().map(|(t,i,k)| format!("{:?}:{:?}::{:?}",
                    t.clone(),
                    i.clone().unwrap_or(Typ::Any),
                    k.clone().unwrap_or(Kind::Nil),
              )).collect::<Vec<String>>().join(","),
              if itks.len()==0 { "" } else { ">" },
              if let Some(ti) = implies { format!(":{:?}",ti) } else { format!("") },
              tk
           ),
           TypeRule::Forall(itks,inf,_t,tk,_) => write!(f, "forall {}. {:?} :: {:?}", 
              itks.iter().map(|(i,t,k)| format!("{:?}:{:?}::{:?}",
                    i.clone().unwrap_or("_".to_string()),
                    t.clone().unwrap_or(Typ::And(Vec::new())),
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
   Ident(String),
   Value(String),
   App(TermId,TermId),
   Let(ScopeId,String,Vec<Vec<(Option<String>,Option<Typ>,Option<Kind>)>>,Option<TermId>,Typ,Kind),
   Tuple(Vec<TermId>),
   Block(ScopeId,Vec<TermId>),
   Ascript(TermId,Typ),
   As(TermId,Typ),
   Constructor(String,Vec<(String,TermId)>),
}

impl TLC {
   pub fn new() -> TLC {
      TLC {
         //the first row, index 0, is nullary
         rows: vec![Row {
            term: Term::Tuple(Vec::new()),
            typ: Typ::Tuple(Vec::new()), //typeof(NULL) is () not []
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
         term_kind: Kind::Simple("Term".to_string(),Vec::new()),
         nil_type: Typ::Tuple(Vec::new()),
         bottom_type: Typ::And(Vec::new()),
      }
   }
   pub fn print_scope(&self, s: ScopeId) -> String {
      let mut buf:String = format!("#{}{{", s.id);
      for (cn,_pks,ct) in self.scopes[s.id].children.iter() {
         buf += &format!("{}: {:?}\n", cn, ct);
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
   pub fn project_kinded(&self, k: &Kind, t: &Typ) -> Typ {
      let ts = match t {
         Typ::And(ts) => ts.clone(),
         tt => vec![tt.clone()],
      };
      for t in ts.iter() {
         let ts = format!("{:?}",t);
         for tr in self.rules.iter() { match tr {
            TypeRule::Typedef(tn,_itks,_implies,_td,tk,_) => {
               if &ts!=tn { continue; }
               let tk = tk.clone().unwrap_or(self.term_kind.clone());
               if &tk==k { return t.clone(); }
               //TODO: kind-project parameterized types
            },
            _ => ()
         }}
      }
      self.bottom_type.clone()
   }
   pub fn remove_kinded(&self, k: &Kind, t: &Typ) -> Typ {
      let ts = match t {
         Typ::And(ts) => ts.clone(),
         tt => vec![tt.clone()],
      };
      let ts = ts.into_iter().filter(|ct|&self.kindof(&ct)!=k).collect::<Vec<Typ>>();
      Typ::And(ts)
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
      let ast = self.parse_doc(globals, docname, src)?;
      self.compile_rules(docname)?;
      self.typecheck(globals, ast, None)?;
      self.sanitycheck()?;
      Ok(ast)
   }
   pub fn kind_of(&self, t: &Typ) -> Kind {
      for rule in self.rules.iter() { match rule {
         TypeRule::Typedef(tt,_tps,_implies,_td,k,_) => { if &format!("{:?}",t)==tt {
            return k.clone().unwrap_or(self.term_kind.clone());
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
                  _ => domains.push((self.bottom_type.clone(),Kind::Nil))
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
         typ: Typ::Any,
         kind: self.term_kind.clone(),
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
            let mut pars: Vec<Vec<(Option<String>,Option<Typ>,Option<Kind>)>> = Vec::new();
            let mut rt = self.bottom_type.clone();
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
                  children.push((i.clone().unwrap_or("_".to_string()), Vec::new(), t.clone().unwrap_or(self.bottom_type.clone())));
               }
            }
            let mut ft = rt.clone();
            let mut fkts = Vec::new();
            for itks in pars.iter().rev() {
               let mut ps = Vec::new();
               for (_i,t,k) in itks.iter() {
                  ps.push(t.clone().unwrap_or(self.bottom_type.clone()));
               }
               let pt = if ps.len()==1 {
                  ps[0].clone()
               } else {
                  Typ::Tuple(ps.clone())
               };
               ft = Typ::Arrow(Box::new(pt),Box::new(ft));
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
                  self.unparse_ast_typ(tt)? //type
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
                  self.unparse_ast_typ(tt)? //type
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
            let mut ps = p.into_inner();
            let t = ps.next().expect("TLC Grammar Error in rule [typ_stmt.1]").into_inner().concat();
            let mut implies = None;
            let mut tiks = Vec::new();
            let mut typedef = Vec::new();
            let mut kind = None;
            for e in ps { match e.as_rule() {
               Rule::typ_inf_kind => {
                  let mut typ = "".to_string();
                  let mut inf = None;
                  let mut kind = None;
                  for tik in e.into_inner() { match tik.as_rule() {
                     Rule::typvar => { typ = tik.into_inner().concat(); },
                     Rule::typ   => { inf   = Some(self.unparse_ast_typ(tik)?); },
                     Rule::kind   => { kind   = Some(self.unparse_ast_kind(tik)?); },
                     rule => panic!("unexpected ident_typ_kind rule: {:?}", rule)
                  }}
                  tiks.push((typ,inf,kind));
               },
               Rule::typ => { implies = Some(self.unparse_ast_typ(e)?); },
               Rule::typedef => {
                  let struct_typ = Typ::Ident(t.clone(), tiks.iter().map(|(t,_i,_k)|Typ::Ident(t.clone(),Vec::new())).collect::<Vec<Typ>>());
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
                                 let kt = self.unparse_ast_typ(kts.next().expect("TLC Grammar Error in rule [typedef.4]"))?;
                                 self.scopes[scope.id].children.push((
                                    format!(".{}",ki.clone()),
                                    Vec::new(),
                                    Typ::Arrow(Box::new(struct_typ.clone()),Box::new(kt.clone())),
                                 ));
                                 tcrows.push((ki,kt));
                              },
                              rule => panic!("unexpected constructor_typedef rule: {:?}", rule)
                           }}
                           typedef.push( Typedef::Constructor(tcname,tcrows) );
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
               tiks,
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
                 Typ::Any,
                 self.term_kind.clone(),
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
         Rule::ident_typ => {
            let mut ps = p.into_inner();
            let tn = ps.next().expect("TLC Grammar Error in rule [ident_typ.1]").into_inner().concat();
            let tps = ps.map(|e|self.unparse_ast_typ(e).expect("TLC Grammar Error in rule [ident_typ.2]")).collect::<Vec<Typ>>();
            Ok(Typ::Ident(tn,tps))
         },
         Rule::atom_typ => self.unparse_ast_typ(p.into_inner().next().expect("TLC Grammar Error in rule [atom_typ]")),
         Rule::any_typ => Ok(Typ::Any),
         Rule::paren_typ => {
            let ts = p.into_inner().map(|e|self.unparse_ast_typ(e).expect("TLC Grammar Error in rule [paren_typ]"))
                      .collect::<Vec<Typ>>();
            if ts.len()==1 {
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
            if tvar.chars().all(char::is_uppercase) { continue; } //Type variables don't need to be defined
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
   pub fn bound_implied(&self, tt: &Typ, span: &Span) -> Result<(),Error> {
      match tt {
         Typ::Any => Ok(()),
         Typ::Or(_ts) => Ok(()),
         Typ::Arrow(p,b) => { self.bound_implied(p,span)?; self.bound_implied(b,span)?; Ok(()) },
         Typ::Ratio(p,b) => { self.bound_implied(p,span)?; self.bound_implied(b,span)?; Ok(()) },
         Typ::And(ts) => { for tc in ts.iter() { self.bound_implied(tc,span)?; } Ok(()) },
         Typ::Tuple(ts) => { for tc in ts.iter() { self.bound_implied(tc,span)?; } Ok(()) },
         Typ::Product(ts) => { for tc in ts.iter() { self.bound_implied(tc,span)?; } Ok(()) },
         Typ::Ident(tn,ts) => {
            if ts.len()==0 { return Ok(()); }
            if !tt.is_concrete() { return Ok(()); }
            for tr in self.rules.iter() { match tr {
               TypeRule::Typedef(tdn,itks,_implies,_td,_tk,_) if tn==tdn && ts.len()==itks.len() => {
                  for (pt,(_bi,bt,_bk)) in std::iter::zip(ts,itks) {
                     if let Some(bt) = bt {
                        self.unify(pt, bt, span)?;
                     }
                  }
                  return Ok(())
               }, _ => (),
            }}
            Ok(())
         },
      }
   }
   pub fn extend_implied(&self, tt: &Typ) -> Typ {
      match tt {
         Typ::Any => tt.clone(),
         Typ::Or(_ts) => tt.clone(),
         Typ::Arrow(p,b) => Typ::Arrow(Box::new(self.extend_implied(p)),Box::new(self.extend_implied(b))),
         Typ::Ratio(p,b) => Typ::Ratio(Box::new(self.extend_implied(p)),Box::new(self.extend_implied(b))),
         Typ::Ident(tn,ts) => {
            for tr in self.rules.iter() { match tr {
               TypeRule::Typedef(tdn,itks,implies,_td,_tk,_) if tn==tdn && ts.len()==itks.len() => {
                  let implies = if let Some(it) = implies {
                     match it {
                        Typ::And(its) => its.clone(),
                        i => vec![i.clone()],
                     }
                  } else { Vec::new() };
                  if implies.len()==0 { return tt.clone(); }
                  let mut ats = Vec::new();
                  ats.push(tt.clone());
                  //TODO: substitute type variables in implied types
                  ats.append(&mut implies.clone());
                  return Typ::And(ats);
               }, _ => (),
	    }}
            tt.clone()
         },
         Typ::And(ts) => {
            let mut ats = Vec::new();
            for tc in ts.iter() {
               let ct = self.extend_implied(tc);
               if let Typ::And(mut cts) = ct {
                  ats.append(&mut cts);
               } else {
                  ats.push(ct);
               }
            }
            Typ::And(ats)
         },
         Typ::Tuple(ts) => Typ::Tuple(ts.iter().map(|tc| self.extend_implied(tc)).collect::<Vec<Typ>>()),
         Typ::Product(ts) => Typ::Product(ts.iter().map(|tc| self.extend_implied(tc)).collect::<Vec<Typ>>()),
      }
   }
   pub fn kindof(&self, tt:&Typ) -> Kind {
      match tt {
         //only simple types can be kinded
         Typ::Ident(tn,ts) => {
            for tr in self.rules.iter() { match tr {
               TypeRule::Typedef(tdn,itks,_implies,_td,tk,_) => {
                  if tn==tdn && ts.len()==itks.len() {
                     return tk.clone().unwrap_or(self.term_kind.clone());
                  }
               },
               _ => ()
            }}
            Kind::Nil
         }, _ => Kind::Nil
      }
   }
   pub fn typeof_var(&self, scope: &Option<ScopeId>, v: &str, implied: &Option<Typ>, span: &Span) -> Result<Typ,Error> {
      if let Some(scope) = scope {
         let ref sc = self.scopes[scope.id];
         for (tn,_tkts,tt) in sc.children.iter() {
            if tn==v {
               if let Some(it) = implied {
                  //if tt => it
                  if let Ok(rt) = self.unify(&tt,it,span) {
                     return Ok(rt.clone());
                  }
               } else {
                  return Ok(tt.clone());
               }
            }
         }
         self.typeof_var(&sc.parent.clone(), v, implied, span)
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
   pub fn typecheck(&mut self, scope: Option<ScopeId>, t: TermId, implied: Option<Typ>) -> Result<(),Error> {
      eprintln!("typecheck {}", self.print_term(t));
      //clone is needed to avoid double mutable borrows?
      match self.rows[t.id].term.clone() {
         Term::Block(sid,es) => {
            let mut last_typ = self.nil_type.clone();
            for e in es.iter() {
               self.typecheck(Some(sid), *e, None)?;
               last_typ = self.rows[e.id].typ.clone();
            }
            self.rows[t.id].typ = self.unify(&self.rows[t.id].typ, &last_typ, &self.rows[t.id].span)?;
         },
         Term::Tuple(es) => {
            let mut ts = Vec::new();
            for e in es.iter() {
               self.typecheck(scope, *e, None)?;
               ts.push(self.rows[e.id].typ.clone());
            }
            self.rows[t.id].typ = self.unify(&self.rows[t.id].typ, &Typ::Tuple(ts), &self.rows[t.id].span)?;
         },
         Term::Let(s,v,_ps,b,rt,_rk) => {
            if v=="" {
               //term is untyped
               self.untyped(t);
            } else if let Some(ref b) = b {
               self.typecheck(Some(s), *b, Some(rt.clone()))?;
               self.rows[t.id].typ = self.bottom_type.clone();
            } else {
               self.rows[t.id].typ = self.bottom_type.clone();
            }
            self.bound_implied(&rt,&self.rows[t.id].span)?;
         },
         Term::Ascript(x,tt) => {
            self.typecheck(scope.clone(), x, Some(tt.clone()))?;
            self.rows[t.id].typ = self.unify(&self.rows[t.id].typ, &self.rows[x.id].typ, &self.rows[t.id].span)?;
         },
         Term::As(x,into) => {
            self.typecheck(scope.clone(), x, None)?;
            let into_kind = self.kindof(&into);
            if let Ok(nt) = self.unify(&self.rows[x.id].typ, &into, &self.rows[t.id].span) {
               //if cast is already satisfied, do nothing
               self.rows[t.id].typ = self.unify(&nt, &self.rows[t.id].typ, &self.rows[t.id].span)?;
            } else {
               let from_kinded = self.project_kinded(&into_kind, &self.rows[x.id].typ);
               for tr in self.rules.iter() { match tr {
                  TypeRule::Forall(_itks,Inference::Imply(lt,rt),_term,tk,_) if tk.clone().unwrap_or(self.term_kind.clone())==into_kind => {
                     if let Ok(lt) = self.unify(&from_kinded, lt, &self.rows[t.id].span) {
                     if let Ok(rt) = self.unify(&rt, &into, &self.rows[t.id].span) {
                        //if conversion rule matches, (L=>R), typeof(x) => L, R => Into :: kindof(Into)
                        //eliminate typeof(x) :: kindof(Into)
                        let l_narrowed = self.remove_kinded(&into_kind, &lt);
                        //introduce typeof(x) (x) R
                        let l_widened = self.unify(&rt, &l_narrowed, &self.rows[t.id].span)?;
                        self.rows[t.id].typ = l_widened;
                        //TODO: substitute term t into macro body if exists
                        break;
                     }}
                  }, _ => {} 
               }}
            }
         },
         Term::Ident(x) => {
            eprintln!("typecheck Ident.1");
            let span = self.rows[t.id].span.clone();
            let xt = self.typeof_var(&scope, &x, &implied, &span)?;
            //typeof(x) => t.typ
            eprintln!("typecheck Ident.2");
            self.rows[t.id].typ = self.unify(&xt, &self.rows[t.id].typ, &self.rows[t.id].span)?;
            eprintln!("typecheck Ident.3");
         },
         Term::Value(x) => {
            let i = if let Some(ref i) = implied { i.clone() } else { Typ::Any };
            let ki = self.project_kinded(&self.term_kind, &i);
            let mut r = None;
            for (pat,re) in self.regexes.iter() {
               if pat==&ki { r=Some(re); break; } //Term kinded is not []
               else if self.bottom_type==ki && re.is_match(&x) { //Term kinded is []
                  r = Some(re);
                  self.rows[t.id].typ = self.unify(&self.rows[t.id].typ, pat, &self.rows[t.id].span)?;
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
               if self.bottom_type!=ri {
                  self.rows[t.id].typ = self.rows[t.id].typ.and(&ri);
               }
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
            self.typecheck(scope.clone(), x, None)?;
            self.typecheck(scope.clone(), g, Some(
               Typ::Arrow(Box::new(self.rows[x.id].typ.clone()),
                          Box::new(Typ::Any))
            ))?;
            self.rows[x.id].typ = self.unify(&self.rows[x.id].typ, &self.rows[g.id].typ.expects(), &self.rows[x.id].span)?;
            self.rows[t.id].typ = self.unify(&self.rows[t.id].typ, &self.rows[g.id].typ.returns(), &self.rows[t.id].span)?;
         },
         Term::Constructor(cname,kvs) => {
            for (_k,v) in kvs.iter() {
               self.typecheck(scope.clone(), *v, None)?;
            }
            let mut found = false;
            for (tt, tname,_tpars,_tkvs) in self.constructors.iter() {
               if &cname==tname {
                  self.rows[t.id].typ = self.unify(
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
      };
      if let Some(ref i) = implied {
         eprintln!("start typecheck implied {:?}", i);
         self.rows[t.id].typ = self.unify(&self.rows[t.id].typ, &i, &self.rows[t.id].span)?;
         eprintln!("end typecheck implied {:?}", i);
      };
      self.bound_implied(&self.rows[t.id].typ,&self.rows[t.id].span)?;
      Ok(())
   }
   pub fn unify(&self, lt: &Typ, rt: &Typ, span: &Span) -> Result<Typ,Error> {
      eprintln!("unify {:?} (x) {:?}", lt, rt);
      //lt => rt
      let mut lt = self.extend_implied(lt); lt.normalize();
      let mut rt = rt.clone(); rt.normalize();
      let mut subs = Vec::new();
      let r = unify_impl(&mut subs, &lt, &rt, span);
      subs.sort();
      subs.dedup();
      let mut tt = if let Ok(tt) = r {
         tt.substitute(&subs)
      } else { return Err(Error {
         kind: "Type Error".to_string(),
         rule: format!("failed unification {:?} (x) {:?}",lt,rt),
         span: span.clone(),
         snippet: "".to_string(),
      }) };
      tt.normalize();
      eprintln!("unify {:?} (x) {:?} yields {:?}", lt, rt, &tt);
      Ok(tt)
   }

   pub fn check(&mut self, globals: Option<ScopeId>, src:&str) -> Result<(),Error> {
      let rows_l = self.rows.len();
      let rules_l = self.rules.len();
      let scopes_l = self.scopes.len();
      let regexes_l = self.regexes.len();
      let globals_l = if let Some(g) = globals { self.scopes[g.id].children.len() } else { 0 };

      let r = self.compile_str(globals, src);

      self.rows.truncate(rows_l);
      self.rules.truncate(rules_l);
      self.scopes.truncate(scopes_l);
      self.regexes.truncate(regexes_l);
      if let Some(g) = globals { self.scopes[g.id].children.truncate(globals_l); };

      r?; Ok(())
   }
}
