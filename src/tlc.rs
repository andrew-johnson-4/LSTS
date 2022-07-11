use std::rc::Rc;
use std::collections::{HashSet,HashMap};
use std::path::Path;
use pest::Parser;
use pest::iterators::{Pair,Pairs};
use pest::error::{ErrorVariant,InputLocation,LineColLocation};
use regex::Regex;
use crate::term::{Term,TermId};
use crate::scope::{Scope,ScopeId};
use crate::typ::{Type,IsParameter};
use crate::kind::Kind;
use crate::token::{Span,tokenize,span_of};
use crate::debug::Error;
use crate::ll::ll1_file;

#[derive(Parser)]
#[grammar = "grammar_tlc.pest"]
struct TlcParser;

pub struct TLC {
   pub rows: Vec<Row>,
   pub rules: Vec<TypeRule>,
   pub scopes: Vec<Scope>,
   pub regexes: Vec<(Type,Rc<Regex>)>,
   pub constructors: HashMap<String,(Type,Vec<Type>,Vec<(String,Type)>)>,
   pub type_is_normal: HashSet<Type>,
   pub kind_is_normal: HashSet<Kind>,
   pub typedef_index: HashMap<String,usize>,
   pub foralls_index: HashMap<Type,Vec<usize>>,
   pub foralls_rev_index: HashMap<Type,Vec<usize>>,
   pub constant_index: HashMap<Constant,TermId>,
   pub tconstant_index: HashMap<TermId,Constant>,
   pub term_kind: Kind,
   pub constant_kind: Kind,
   pub nil_type: Type,
   pub bottom_type: Type,
}

#[derive(Clone,Eq,PartialEq,Ord,PartialOrd,Hash)]
pub enum Constant {
   NaN,
   Boolean(bool),
   Integer(i64),
   Op(String),
   Tuple(Vec<Constant>),
}
impl std::fmt::Debug for Constant {
   fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
      match self {
        Constant::NaN => write!(f, "NaN"),
        Constant::Boolean(c) => write!(f, "{}", if *c {"True"} else {"False"}),
        Constant::Integer(i) => write!(f, "{}", i),
        Constant::Op(op) => write!(f, "{}", op),
        Constant::Tuple(ts) => write!(f, "({})", ts.iter()
           .map(|t|format!("{:?}",t)).collect::<Vec<String>>()
           .join(",") ),
      }
   }
}

pub struct Row {
   pub term: Term,
   pub typ: Type,
   pub kind: Kind,
   pub span: Span,
   pub constant: Option<Constant>,
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
   pub prop: TermId,
   pub algs: TermId,
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
               filename: Rc::new("".to_string()),
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
         constant_index: HashMap::new(),
         tconstant_index: HashMap::new(),
         type_is_normal: HashSet::new(),
         kind_is_normal: HashSet::new(),
         term_kind: Kind::Simple("Term".to_string(),Vec::new()),
         constant_kind: Kind::Simple("Constant".to_string(),Vec::new()),
         nil_type: Type::Tuple(Vec::new()),
         bottom_type: Type::And(Vec::new()),
      }
   }
   pub fn print_type(&self, kinds: &HashMap<Type,Kind>, tt: &Type) -> String {
      let ts = match tt {
         Type::Any => format!("?"),
         Type::Ident(t,ts) => {
            if ts.len()==0 { format!("{}", t) }
            else { format!("{}<{}>", t, ts.iter().map(|t|self.print_type(kinds,t)).collect::<Vec<String>>().join(",") ) }
         }
         Type::And(ts) => format!("{{{}}}", ts.iter().map(|t|self.print_type(kinds,t)).collect::<Vec<String>>().join("+") ),
         Type::Tuple(ts) => format!("({})", ts.iter().map(|t|self.print_type(kinds,t)).collect::<Vec<String>>().join(",") ),
         Type::Product(ts) => format!("({})", ts.iter().map(|t|self.print_type(kinds,t)).collect::<Vec<String>>().join("*") ),
         Type::Arrow(p,b) => format!("({})=>({})", self.print_type(kinds,p), self.print_type(kinds,b)),
         Type::Ratio(n,d) => format!("({})/({})", self.print_type(kinds,n), self.print_type(kinds,d)),
         Type::Constant(v,c) => {
            if let Some(ct) = self.tconstant_index.get(c) {
               format!("[{}{:?}#{}]", if *v {"'"} else {""}, ct, c.id)
            } else {
               format!("[{}{}#{}]", if *v {"'"} else {""}, self.print_term(*c), c.id)
            }
         },
      };
      if let Some(k) = kinds.get(tt) {
         format!("{}::{:?}", ts, k)
      } else { ts }
   }
   pub fn print_scope(&self, s: ScopeId) -> String {
      let mut buf:String = format!("#{}{{\n", s.id);
      for (cn,pks,ct,_v) in self.scopes[s.id].children.iter() {
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
         Term::Substitution(e,a,b) => format!("{}\\[{}|{}]", self.print_term(*e), self.print_term(*a), self.print_term(*b)),
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
         if self.kind(t).has(k) {
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
      let ts = ts.into_iter().filter(|ct|ct!=&Type::Any&&!self.kind(ct).has(k)).collect::<Vec<Type>>();
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
      let mut tokens = tokenize(docname.to_string(), src)?;
      let file_scope = globals.unwrap_or(self.push_scope(Scope {
         parent: None,
         children: Vec::new(),
      }, &span_of(&tokens)));
      let ast = ll1_file(self, file_scope, &mut tokens)?;
      self.compile_rules(docname)?;
      self.typeck(globals, ast, None)?;
      self.sanityck()?;
      Ok(ast)
   }
   pub fn kinds_of(&self, kinds: &mut HashMap<Type,Kind>, tt: &Type) -> Option<Kind> {
      match tt {
         Type::Any => {
            kinds.insert(Type::Any, Kind::Nil);
            Some(Kind::Nil)
         },
         Type::Constant(_,_) => {
            kinds.insert(tt.clone(), self.constant_kind.clone());
            Some(self.constant_kind.clone())
         },
         Type::And(ts) => {
            let mut ks = Vec::new();
            for ct in ts.iter() {
            if let Some(k) = self.kinds_of(kinds,ct) {
               ks.push(k.clone());
            }};
            let k = if ks.len()==0 { Kind::Nil
            } else if ks.len()==1 { ks[0].clone()
            } else { Kind::and(ks) };
            kinds.insert(tt.clone(), k.clone());
            Some(k)
         },
         Type::Product(ts) => {
            let mut k = None;
            for ct in ts.iter() {
               k = self.kinds_of(kinds,ct);
            };
            if let Some(ref k) = k {
               kinds.insert(tt.clone(), k.clone());
            }
            k
         },
         Type::Tuple(ts) => {
            let mut k = None;
            for ct in ts.iter() {
               k = self.kinds_of(kinds,ct);
            }
            if let Some(ref k) = k {
               kinds.insert(tt.clone(), k.clone());
            } else {
               kinds.insert(tt.clone(), self.term_kind.clone());
            }
            k
         },
         Type::Arrow(p,b) => {
            self.kinds_of(kinds,p);
            self.kinds_of(kinds,b)
         },
         Type::Ratio(p,b) => {
                self.kinds_of(kinds,p);
            let k = self.kinds_of(kinds,b);
            if let Some(ref k) = k {
               kinds.insert(tt.clone(), k.clone());
            };
            k
         },
         Type::Ident(cn,_) => {
            if let Some(t) = kinds.get(tt) { return Some(t.clone()); }
            let mut k = None;
            if let Some(ti) = self.typedef_index.get(cn) {
            if let TypeRule::Typedef(_tn,_norm,_tps,_implies,_td,tk,_props,_) = &self.rules[*ti] {
               kinds.insert(tt.clone(), tk.clone());
               k = Some(tk.clone());
            }}
            k
         },
      }
   }
   pub fn compile_rules(&mut self, _docname:&str) -> Result<(),Error> {

      //check logical consistency of foralls
      for rule in self.rules.clone().iter() { match rule {
         TypeRule::Forall(_qs,_inf,_t,_k,_sp) => {
            /* domain check hasn't been working for a while
               just comment it out until the topic comes up again
               TODO: add some motivating examples and tests for domain checks
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
                  domains.push((it.clone(),self.kind(it)));
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
               })
            }
            */
         },
         TypeRule::Typedef(tn,_norm,_itks,_implies,tds,_tks,props,_span) => {
            for td in tds.iter() { match td {
               Typedef::Regex(pat) => {
                  if let Ok(r) = Regex::new(&pat[1..pat.len()-1]) {
                     self.regexes.push((Type::Ident(tn.clone(),Vec::new()),Rc::new(r)));
                  } else {
                     panic!("typedef regex rejected: {}", pat);
                  }
               },
               Typedef::Constructor(cname,kts) => {
                  self.constructors.insert(cname.clone(), (Type::Ident(tn.clone(),Vec::new()),Vec::new(),kts.clone()));
               }
            }}
            for p in props.iter() {
               self.untyped(p.prop);
               self.untyped(p.algs);
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
                filename: Rc::new(docname.to_string()),
                offset_start: istart,
                offset_end: iend,
                linecol_start: start,
                linecol_end: end,
             },
          })
        }
      } 
   }
   pub fn unparse_file(&mut self, scope:Option<ScopeId>, fp:&str, ps: Pairs<crate::tlc::Rule>) -> Result<TermId,Error> {
      let p = ps.peek().unwrap();
      let span = Span {
         filename: Rc::new(fp.to_string()),
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
   pub fn maybe_constant(&self, t: TermId) -> Option<Constant> {
      if let Some(c) = self.tconstant_index.get(&t) {
         Some(c.clone())
      } else {
         None
      }
   }
   pub fn push_constant(&mut self, c: &Constant, t: TermId) -> TermId {
      if let Some(ct) = self.constant_index.get(c) {
         *ct
      } else {
         self.constant_index.insert(c.clone(),t);
         self.tconstant_index.insert(t,c.clone());
         t
      }
   }
   pub fn parse_constant(&self, c: &str) -> Option<Constant> {
      if c=="NaN" { Some(Constant::NaN)
      } else if c=="True" { Some(Constant::Boolean(true))
      } else if c=="False" { Some(Constant::Boolean(false))
      } else if c=="if" { Some(Constant::Op(c.to_string()))
      } else if c=="not" { Some(Constant::Op(c.to_string()))
      } else if c=="pos" { Some(Constant::Op(c.to_string()))
      } else if c=="neg" { Some(Constant::Op(c.to_string()))
      } else if c=="+" { Some(Constant::Op(c.to_string()))
      } else if c=="-" { Some(Constant::Op(c.to_string()))
      } else if c=="*" { Some(Constant::Op(c.to_string()))
      } else if c=="/" { Some(Constant::Op(c.to_string()))
      } else if c=="^" { Some(Constant::Op(c.to_string()))
      } else if c=="%" { Some(Constant::Op(c.to_string()))
      } else if c=="==" { Some(Constant::Op(c.to_string()))
      } else if c=="!=" { Some(Constant::Op(c.to_string()))
      } else if c=="<" { Some(Constant::Op(c.to_string()))
      } else if c=="<=" { Some(Constant::Op(c.to_string()))
      } else if c==">" { Some(Constant::Op(c.to_string()))
      } else if c==">=" { Some(Constant::Op(c.to_string()))
      } else if c=="&&" { Some(Constant::Op(c.to_string()))
      } else if c=="||" { Some(Constant::Op(c.to_string()))
      } else if let Ok(ci) = c.parse::<i64>() { Some(Constant::Integer(ci))
      } else { None }
   }
   pub fn push_dep_type(&mut self, term: &Term, ti: TermId) -> Type {
      match term {
         Term::Value(ct) => {
            if let Some(c) = self.parse_constant(ct) {
               let ci = self.push_constant(&c, ti);
               Type::Constant(false, ci)
            } else {
               Type::Constant(false, ti)
            }
         }, Term::Ident(_) => {
            Type::Constant(true, ti)
         }, _ => Type::Constant(false, ti),
      }
   }
   pub fn push_term_type(&mut self, term: &Term, ti: TermId) -> Type {
      match term {
         Term::Value(ct) => {
            if let Some(c) = self.parse_constant(ct) {
               let ci = self.push_constant(&c, ti);
               Type::And(vec![Type::Any,Type::Constant(false, ci)])
            } else {
               Type::Any
            }
         }, _ => Type::Any,
      }
   }
   pub fn push_term(&mut self, term: Term, span: &Span) -> TermId {
      let index = self.rows.len();
      let ti = TermId { id: index };
      let tt = self.push_term_type(&term, ti);
      self.rows.push(Row {
         term: term,
         typ: tt,
         kind: self.term_kind.clone(),
         span: span.clone(),
         constant: None,
      });
      ti
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

         //passthrough rules
         Rule::stmt => self.unparse_ast(scope,fp,p.into_inner().next().expect("TLC Grammar Error in rule [stmt]"),span),
         Rule::term => self.unparse_ast(scope,fp,p.into_inner().next().expect("TLC Grammar Error in rule [term]"),span),
         Rule::value_term => self.unparse_ast(scope,fp,p.into_inner().next().expect("TLC Grammar Error in rule [value_term]"),span),
         Rule::atom_term => self.unparse_ast(scope,fp,p.into_inner().next().expect("TLC Grammar Error in rule [atom_term]"),span),
         Rule::prefix_term => {
            let mut prefixes = Vec::new();
            let mut term = None;
            for pe in p.into_inner() { match pe.as_rule() {
               Rule::prefix_op => { match pe.into_inner().concat() {
                  s if s=="+" => { prefixes.push("pos".to_string()); },
                  s if s=="-" => { prefixes.push("neg".to_string()); },
                  s => panic!("TLC Grammar Error in rule [prefix_term.0] '{}'", s),
               }},
               Rule::atom_term => { term = Some(self.unparse_ast(scope,fp,pe,span)?); },
               _ => panic!("TLC Grammar Error in rule [prefix_term.1]")
            }}
            if let Some(mut t) = term {
               for op in prefixes.iter().rev() {
                  let f = self.push_term(Term::Ident(op.to_string()),span);
                  t = self.push_term(Term::App(f,t),span);
               }
               Ok(t)
            } else {
               panic!("TLC Grammar Error in rule [prefix_term.2]")
            }
         },

         //literal value rules
         Rule::ident => Ok(self.push_term(Term::Ident(self.into_ident(p.into_inner().concat())), &span)),
         Rule::constant => Ok(self.push_term(Term::Value(p.into_inner().concat()), &span)),

         //complex rules
         Rule::let_stmt => {
            let mut ps = p.into_inner();
            let ident  = self.into_ident(ps.next().expect("TLC Grammar Error in rule [let_stmt.1]").into_inner().concat());
            let mut pars: Vec<Vec<(Option<String>,Option<Type>,Kind)>> = Vec::new();
            let mut rt = self.nil_type.clone();
            let mut rk = self.term_kind.clone();
            let mut t  = None;
            let mut dept = HashMap::new();
            for e in ps { match e.as_rule() {
               Rule::let_stmt_par => {
                  let mut itks = Vec::new();
                  for itkse in e.into_inner() {
                     let mut ident = None;
                     let mut typ   = None;
                     let mut kind  = self.term_kind.clone();
                     for itk in itkse.into_inner() { match itk.as_rule() {
                        Rule::ident => { ident = Some(itk.into_inner().concat()); },
                        Rule::typ   => { typ   = Some(self.unparse_ast_type(&mut dept,scope,fp,itk,span)?); },
                        Rule::kind   => { kind = self.unparse_ast_kind(scope,fp,itk,span)?; },
                        rule => panic!("unexpected ident_typ_kind rule: {:?}", rule)
                     }}
                     if let Some(tt) = &typ {
                     if tt.is_constant() {
                        kind = self.constant_kind.clone();
                     }};
                     itks.push((ident,typ,kind));
                  }
                  pars.push(itks);
               },
               Rule::typ => { rt = self.unparse_ast_type(&mut dept,scope,fp,e,span)?; },
               Rule::kind => { rk = self.unparse_ast_kind(scope,fp,e,span)?; },
               Rule::term => { t = Some(self.unparse_ast(scope,fp,e,span)?); },
               rule => panic!("unexpected let_stmt rule: {:?}", rule),
            }}
            if rt.is_constant() {
               rk = self.constant_kind.clone();
            };
            let mut children = Vec::new();
            for itks in pars.iter() {
               for (i,t,k) in itks.iter() {
                  let t = t.clone().unwrap_or(self.bottom_type.clone()).normalize();
                  let mut ks = HashMap::new(); ks.insert(t.clone(),k.clone());
                  let vn = i.clone().unwrap_or("_".to_string());
                  let vt = self.push_term(Term::Ident(vn.clone()),span);
                  self.untyped(vt);
                  children.push((vn.clone(), ks, t.clone(), vt));
               }
            }
            let mut ft = rt.clone();
            let mut fkts = HashMap::new();
            for itks in pars.iter().rev() {
               let mut ps = Vec::new();
               for (_i,t,k) in itks.iter() {
                  let t = t.clone().unwrap_or(self.bottom_type.clone()).normalize();
                  fkts.insert(t.clone(),k.clone());
                  ps.push(t.clone());
               }
               let pt = if ps.len()==1 {
                  ps[0].clone()
               } else {
                  Type::Tuple(ps.clone())
               };
               ft = Type::Arrow(Box::new(pt),Box::new(ft));
            }
            self.reduce_type(&HashMap::new(), &mut ft, span); //destructively reduce constants in type
            ft = ft.normalize();
            let vt = self.push_term(Term::Ident(ident.clone()), span);
            self.untyped(vt);
            self.scopes[scope.id].children.push((ident.clone(), fkts, ft, vt));
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
                  self.unparse_ast_type(&mut HashMap::new(),scope,fp,tt,span)? //type
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
                  self.unparse_ast_type(&mut HashMap::new(),scope,fp,tt,span)? //type
               ); self.push_term(t, &span)}),
            }
         },
         Rule::if_term => {
            let mut es = p.into_inner();
            let ct = self.unparse_ast(scope,fp,es.next().expect("TLC Grammar Error in rule [if_term.1]"),span)?;
            let tt = self.unparse_ast(scope,fp,es.next().expect("TLC Grammar Error in rule [if_term.1]"),span)?;
            let ft = if let Some(fe) = es.next() {
               self.unparse_ast(scope,fp,fe,span)?
            } else {
               self.push_term(Term::Block(scope,Vec::new()),span)
            };
            let it = {let t = Term::App(
               self.push_term(Term::Ident("if".to_string()),span),
               self.push_term(Term::Tuple(vec![
                  ct,
                  tt,
                  ft
               ]),span),
            ); self.push_term(t,span)};
            Ok(it)
         }
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
         Rule::expr_term => {
            //shunting yard algorithm to avoid recursion explosion
            let precedence = |s:&str| { match s {
               "^" => 1,
               "*" => 2, "/" => 2, "%" => 2,
               "+" => 3, "-" => 3,
               "<" => 4, "<=" => 4, ">" => 4, ">=" => 4, "==" => 4, "!=" => 4,
               "&&" => 5, "||" => 5,
               op => panic!("unknown operator {:?}", op),
            }};

            let mut es = p.into_inner();
            let e = self.unparse_ast(scope,fp,es.next().expect("TLC Grammar Error in rule [expr_term.1]"),span)?;

            let mut values: Vec<TermId> = vec![e];
            let mut operators: Vec<String> = Vec::new();

            while let Some(op) = es.next() {
               let op = op.into_inner().concat();
               while operators.len()>0 && precedence(&operators[operators.len()-1])<=precedence(&op) {
                  let pop = operators.pop().unwrap();
                  let rt = values.pop().unwrap();
                  let lt = values.pop().unwrap();
                  let t = Term::App(
                     self.push_term(Term::Ident(pop),span),
                     self.push_term(Term::Tuple(vec![lt,rt]),span),
                  );
                  values.push(self.push_term(t,span));
               }
               operators.push(op);

               let d = self.unparse_ast(scope,fp,es.next().expect("TLC Grammar Error in rule [expr_term.2]"),span)?;
               values.push(d);
            }
            while operators.len()>0 {
               let pop = operators.pop().unwrap();
               let rt = values.pop().unwrap();
               let lt = values.pop().unwrap();
               let t = Term::App(
                  self.push_term(Term::Ident(pop),span),
                  self.push_term(Term::Tuple(vec![lt,rt]),span),
               );
               values.push(self.push_term(t,span));
            }

            Ok(values.pop().unwrap())
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
         Rule::algebra_term => {
            let mut es = p.into_inner();
            let mut e = self.unparse_ast(scope,fp,es.next().expect("TLC Grammar Error in rule [algebra_term.1]"),span)?;
            while let Some(a) = es.next() {
               let mut a = self.unparse_ast(scope,fp,a,span)?;
               self.untyped(a); self.unify_varnames(&mut HashMap::new(),&mut a);
               let mut b = self.unparse_ast(scope,fp,es.next().expect("TLC Grammar Error in rule [algebra_term.2]"),span)?;
               self.untyped(b); self.unify_varnames(&mut HashMap::new(),&mut b);
               e = {let t = Term::Substitution(
                  e,
                  a,
                  b,
               ); self.push_term(t,&span)};
            }
            Ok(e)
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
            let mut dept = HashMap::new();
            for e in p.into_inner() { match e.as_rule() {
               Rule::typname => { t=e.into_inner().concat(); },
               Rule::normal => { normal=true; },
               Rule::typ_inf_kind => {
                  let mut typ = "".to_string();
                  let mut inf = None;
                  let mut kind = self.term_kind.clone();
                  for tik in e.into_inner() { match tik.as_rule() {
                     Rule::typvar => { typ = tik.into_inner().concat(); },
                     Rule::typ   => { inf   = Some(self.unparse_ast_type(&mut dept,scope,fp,tik,span)?); },
                     Rule::kind   => { kind   = self.unparse_ast_kind(scope,fp,tik,span)?; },
                     rule => panic!("unexpected ident_typ_kind rule: {:?}", rule)
                  }}
                  tiks.push((typ,inf,kind));
               },
               Rule::typ => { implies = Some(self.unparse_ast_type(&mut dept,scope,fp,e,span)?); },
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
                                 let kt = self.unparse_ast_type(&mut dept,scope,fp,kts.next().expect("TLC Grammar Error in rule [typedef.4]"),span)?;
                                 let vn = format!(".{}",ki.clone());
                                 let vt = self.push_term(Term::Ident(vn.clone()),span);
                                 self.untyped(vt);
                                 self.scopes[scope.id].children.push((
                                    vn,
                                    HashMap::new(),
                                    Type::Arrow(Box::new(struct_typ.clone()),Box::new(kt.clone())),
                                    vt
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
                  let mut prop = None;
                  let mut algs = None;
                  for tip in e.into_inner() { match tip.as_rule() {
                     Rule::ident_typ_kind => {
                        let mut idn = None;
                        let mut inf = None;
                        let mut kind = self.term_kind.clone();
                        for itk in tip.into_inner() { match itk.as_rule() {
                           Rule::ident => { idn = Some(itk.into_inner().concat()); },
                           Rule::typ   => { inf   = Some(self.unparse_ast_type(&mut dept,scope,fp,itk,span)?); },
                           Rule::kind   => { kind   = self.unparse_ast_kind(scope,fp,itk,span)?; },
                           rule => panic!("unexpected ident_typ_kind rule: {:?}", rule)
                        }}
                        itks.push((idn,inf,kind));
                     },
                     Rule::term => {
                        if prop.is_none() {
                           prop = Some(self.unparse_ast(scope,fp,tip,span)?);
                        } else {
                           algs = Some(self.unparse_ast(scope,fp,tip,span)?);
                        }
                     }
                     rule => panic!("unexpected typ_invariant rule: {:?}", rule)
                  }}
                  let algs = if let Some(a) = algs { a }
                  else { self.push_term(Term::Ident("True".to_string()),&span) };
                  props.push(Invariant {
                     itks: itks,
                     prop: prop.expect("TLC Grammar Error in rule [typ_invariant]"),
                     algs: algs,
                  });
               },
               rule => panic!("unexpected typ_stmt rule: {:?}", rule)
            }}
            let kinds = if kinds.len()==0 { self.term_kind.clone()
            } else if kinds.len()==1 { kinds[0].clone()
            } else { Kind::and(kinds) };
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
            for inv in props.iter() {
            if let Term::App(g,x) = &self.rows[inv.prop.id].term.clone() {
            if let Term::Ident(gn) = &self.rows[g.id].term.clone() {
               let mut xs = Vec::new();
               let mut fkts = HashMap::new();
               match &self.rows[x.id].term.clone() {
                  Term::Tuple(ts) => {
                     for ct in ts.iter() {
                        if let Term::Ident(ctn) = &self.rows[ct.id].term.clone() {
                        if ctn == "self" { //replace "self" in invariants with this type rule
                           xs.push(Type::Ident(t.clone(),Vec::new()));
                           fkts.insert(xs[xs.len()-1].clone(), self.term_kind.clone());
                           continue;
                        }}
                        let ctt = self.rows[ct.id].term.clone();
                        xs.push(self.push_dep_type(&ctt, *ct));
                        fkts.insert(xs[xs.len()-1].clone(), self.constant_kind.clone());
                     }
                  }, pt => {
                     let mut is_self = false;
                     if let Term::Ident(ctn) = pt {
                     if ctn == "self" { //replace "self" in invariants with this type rule
                        xs.push(Type::Ident(t.clone(),Vec::new()));
                        fkts.insert(xs[xs.len()-1].clone(), self.term_kind.clone());
                        is_self = true;
                     }}
                     if !is_self {
                        xs.push(self.push_dep_type(&pt, *x));
                        fkts.insert(xs[xs.len()-1].clone(), self.constant_kind.clone());
                     }
                  }
               }
               let rt = self.push_dep_type(&self.rows[inv.algs.id].term.clone(), inv.algs);

               let pt = if xs.len()==1 {
                  xs[0].clone()
               } else {
                  Type::Tuple(xs)
               };
               let ft = Type::Arrow(Box::new(pt),Box::new(rt));
               let vt = self.push_term(Term::Ident(gn.clone()),span);
               self.untyped(vt);
               self.scopes[scope.id].children.push((gn.clone(), fkts, ft, vt));
            }}}
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
            let mut dept = HashMap::new();
            for e in p.into_inner() { match e.as_rule() {
               Rule::ident_typ_kind => {
                  let mut ident = None;
                  let mut typ = None;
                  let mut kind = self.term_kind.clone();
                  for itk in e.into_inner() { match itk.as_rule() {
                     Rule::ident => { ident = Some(itk.into_inner().concat()); },
                     Rule::typ   => { typ   = Some(self.unparse_ast_type(&mut dept,scope,fp,itk,span)?); },
                     Rule::kind   => { kind   = self.unparse_ast_kind(scope,fp,itk,span)?; },
                     rule => panic!("unexpected ident_typ_kind rule: {:?}", rule)
                  }}
                  if let Some(tt) = &typ {
                  if tt.is_constant() {
                     kind = self.constant_kind.clone();
                  }};
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
                  let vn = i.clone().unwrap_or("_".to_string());
                  let vt = self.push_term(Term::Ident(vn.clone()),span);
                  self.untyped(vt);
                  children.push((vn.clone(), HashMap::new(), t.clone().unwrap_or(self.bottom_type.clone()), vt));
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
      let mut dept = HashMap::new();
      for e in p.into_inner() { match e.as_rule() {
         Rule::typ => {
            if a.is_none() { a = Some(self.unparse_ast_type(&mut dept,scope,fp,e,span)?); }
            else { b = Some(self.unparse_ast_type(&mut dept,scope,fp,e,span)?); }
         },
         rule => panic!("unexpected inference rule: {:?}", rule)
      }}
      if a.is_none() { panic!("TLC Grammar Error in rule [inference]") }
      else if b.is_none() { Ok(Inference::Type(a.unwrap())) }
      else { Ok(Inference::Imply(a.unwrap(), b.unwrap())) }
   }
   pub fn unparse_ast_type(&mut self, dept: &mut HashMap<String,TermId>, scope:ScopeId, fp:&str, p: Pair<crate::tlc::Rule>, span: &Span) -> Result<Type,Error> {
      match p.as_rule() {
         Rule::typname => {
            let name = p.into_inner().concat();
            Ok(Type::Ident(name,Vec::new()))
         },
         Rule::typ => self.unparse_ast_type(dept,scope,fp,p.into_inner().next().expect("TLC Grammar Error in rule [typ]"),span),
         Rule::ident_typ => {
            let mut ps = p.into_inner();
            let tn = ps.next().expect("TLC Grammar Error in rule [ident_typ.1]").into_inner().concat();
            let tps = ps.map(|e|self.unparse_ast_type(dept,scope,fp,e,span).expect("TLC Grammar Error in rule [ident_typ.2]")).collect::<Vec<Type>>();
            Ok(Type::Ident(tn,tps))
         },
         Rule::atom_typ => self.unparse_ast_type(dept,scope,fp,p.into_inner().next().expect("TLC Grammar Error in rule [atom_typ]"),span),
         Rule::any_typ => Ok(Type::Any),
         Rule::paren_typ => {
            let ts = p.into_inner().map(|e|self.unparse_ast_type(dept,scope,fp,e,span).expect("TLC Grammar Error in rule [paren_typ]"))
                      .collect::<Vec<Type>>();
            if ts.len()==1 {
               Ok(ts[0].clone())
            } else {
               Ok(Type::Tuple(ts))
            }
         },
         Rule::arrow_typ => {
            let mut ts = p.into_inner();
            let mut t = self.unparse_ast_type(dept,scope,fp,ts.next().expect("TLC Grammar Error in rule [arrow_typ]"),span)?;
            for tr in ts {
               t = Type::Arrow(
                  Box::new(t),
                  Box::new(self.unparse_ast_type(dept,scope,fp,tr,span)?)
               );
            }
            Ok(t)
         },
         Rule::suffix_typ => {
            let mut ts = p.into_inner();
            let mut t = self.unparse_ast_type(dept,scope,fp,ts.next().expect("TLC Grammar Error in rule [suffix_typ.1]"),span)?;
            for bracketed in ts.rev() {
               let mut dt = if let Some(br) = bracketed.into_inner().next() {
                  self.unparse_ast(scope,fp,br,span)?
               } else {
                  self.push_term(Term::Ident("length".to_string()),span)
               };
               self.untyped(dt);
               self.unify_varnames(dept,&mut dt);
               let ct = self.push_dep_type(&self.rows[dt.id].term.clone(), dt);
               t = Type::Ident("Tensor".to_string(), vec![
                  t,
                  ct
               ]);
            }
            Ok(t)
         },
         Rule::ratio_typ => {
            let ts = p.into_inner().map(|e|self.unparse_ast_type(dept,scope,fp,e,span).expect("TLC Grammar Error in rule [ratio_typ.1]"))
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
            let ts = p.into_inner().map(|e|self.unparse_ast_type(dept,scope,fp,e,span).expect("TLC Grammar Error in rule [or_typ]"))
                      .collect::<Vec<Type>>();
            if ts.len()==1 {
               Ok(ts[0].clone())
            } else {
               Ok(Type::Product(ts))
            }
         },
         Rule::and_typ => {
            let ts = p.into_inner().map(|e|self.unparse_ast_type(dept,scope,fp,e,span).expect("TLC Grammar Error in rule [and_typ]"))
                      .collect::<Vec<Type>>();
            if ts.len()==1 {
               Ok(ts[0].clone())
            } else {
               Ok(Type::And(ts))
            }
         },
         Rule::dep_typ => {
            let mut t = self.unparse_ast(scope,fp,p.into_inner().next().expect("TLC Grammar Error in rule [stmt]"),span)?;
            self.untyped(t);
            self.unify_varnames(dept,&mut t); //convert all varnames to "var#{term.id}" and give identical vars the same id
            let ct = self.push_dep_type(&self.rows[t.id].term.clone(), t);  //check if type is constant
            Ok(ct)
         }
         Rule::typeof_typ => {
            let v = p.into_inner().concat();
            let vt = self.typeof_var(&Some(scope), &v, &None, span)?;
            Ok(vt)
         },
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
            })}
         }
      }
      Ok(())
   }
   pub fn soundck(&mut self, tt: &Type, span: &Span) -> Result<(),Error> {
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
            if let TypeRule::Typedef(_tn,_norm,itks,_implies,_td,_tk,_props,_) = &self.rules[*ti].clone() {
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
         Type::Constant(_,_) => Ok(()),
      }
   }
   pub fn extend_implied(&self, tt: &Type) -> Type {
      match tt {
         Type::Any => tt.clone(),
         Type::Arrow(p,b) => Type::Arrow(Box::new(self.extend_implied(p)),Box::new(self.extend_implied(b))),
         Type::Ratio(p,b) => Type::Ratio(Box::new(self.extend_implied(p)),Box::new(self.extend_implied(b))),
         Type::Ident(tn,ts) => {
            let ts = ts.iter().map(|ct|self.extend_implied(ct)).collect::<Vec<Type>>();
            let mut implies: Vec<Type> = Vec::new();
            let mut subs = HashMap::new();

            //lookup typedefs
            if let Some(ti) = self.typedef_index.get(tn) {
            if let TypeRule::Typedef(_tn,_norm,tiks,imp,_td,_tk,_props,_) = &self.rules[*ti] {
               for ((ot,_it,_k),st) in std::iter::zip(tiks.iter(), ts.iter()) {
                  subs.insert(Type::Ident(ot.clone(),Vec::new()), st.clone());
               }
               if let Some(it) = imp {
                  match self.extend_implied(it) {
                     Type::And(mut its) => { implies.append(&mut its); },
                     i => { implies.push(i); },
                  }
               }
            }}

            //lookup constructors
            if let Some((bt,_ps,_kts)) = self.constructors.get(tn) {
               implies.push(bt.clone());
            }

            let mut ats = Vec::new();
            ats.push(Type::Ident(tn.clone(),ts));
            for i in implies.iter() {
               ats.push(i.substitute(&subs));
            }

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
         Type::Constant(v,c) => Type::Constant(*v,*c)
      }
   }
   pub fn kind(&self, tt:&Type) -> Kind {
      let mut kinds = HashMap::new();
      self.kinds_of(&mut kinds,tt);
      if let Some(k) = kinds.get(tt) {
         k.clone()
      } else {
         panic!("kinds_of did not ascribe a kind for type {:?}", tt)
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
         Type::Constant(_,_) => true,
      }
   }
   pub fn narrow(&self, kinds: &HashMap<Type,Kind>, projection: &Kind, tt: &Type) -> Type {
      match tt {
         Type::Any => Type::Any,
         Type::Arrow(tp,tb) => { Type::Arrow(
            Box::new(self.narrow(kinds,projection,tp)),
            Box::new(self.narrow(kinds,projection,tb))
         ) },
         Type::And(ts) => {
            let mut cts = Vec::new();
            for ct in ts.iter() {
            if let Some(ck) = kinds.get(ct) {
            if ck.has(projection) {
               cts.push(ct.clone());
            }}}
            if cts.len()==1 { cts[0].clone() }
            else { Type::And(cts) }
         },
         _ => tt.clone(),
      }
   }
   pub fn typeof_var(&mut self, scope: &Option<ScopeId>, v: &str, implied: &Option<Type>, span: &Span) -> Result<Type,Error> {
      if let Some(scope) = scope {
         let mut candidates = Vec::new();
         let mut matches = Vec::new();
         let ref sc = self.scopes[scope.id].clone();
         for (tn,tkts,tt,vt) in sc.children.iter() {
            if tn==v {
               //match variable binding if
               //1) binding is not an arrow
               //2) implied => binding

               candidates.push(tt.clone());
               if let Type::Arrow(tp,_tb) = &tt {
               if let Some(it) = implied {
                  let mut tkts = tkts.clone();
                  self.kinds_of(&mut tkts, &tt);
                  self.kinds_of(&mut tkts, it);
                  //implied type maybe need to be narrowed by kind
                  let narrow_it = if let Some(tk) = tkts.get(tp) {
                     self.narrow(&tkts, tk, &it)
                  } else { it.clone() };
                  if let Ok(rt) = self.unify_with_kinds(&tkts,&narrow_it,&tt,span,IsParameter::Top) {
                     matches.push(rt.clone());
                  }
               }} else {
                  let kt = self.project_kinded(&self.constant_kind, tt);
                  let tt = if kt==self.bottom_type {
                     let vt = Type::Constant(false,*vt);
                     tt.and(&vt)
                  } else { tt.clone() };
                  matches.push(tt);
               }
            }
         }
         if matches.len()>0 {
            //it is OK for multiple functions to match
            Ok(self.extend_implied(&Type::And(matches)).normalize())
         } else if candidates.len() > 0 {
            let implied = implied.clone().unwrap_or(Type::Any);
            let mut tkts = HashMap::new();
            self.kinds_of(&mut tkts, &implied);
            for (tn,tks,_,_) in sc.children.iter() {
            if tn == v {
               tkts.extend(tks.clone().into_iter());
            }}
         Err(Error {
            kind: "Type Error".to_string(),
            rule: format!("variable {}: {} did not match any candidate {}",
                     v,
                     self.print_type(&tkts, &implied),
                     candidates.iter().map(|t|self.print_type(&tkts,t))
                               .collect::<Vec<String>>().join(" | "),
                  ),
            span: span.clone(),
         }) } else {
            self.typeof_var(&sc.parent.clone(), v, implied, span)
         }
      } else { Err(Error {
         kind: "Type Error".to_string(),
         rule: format!("variable not found in scope: {}", v),
         span: span.clone(),
      }) }
   }
   pub fn reduce_type(&mut self, subs: &HashMap<Type,Type>, tt: &mut Type, span: &Span) {
      match tt {
         Type::Constant(_v, c) => {
            self.untyped_eval(subs, c);
         },
         Type::Any => {},
         Type::Ident(_tn,tps) => {
            for mut tp in tps.iter_mut() {
               self.reduce_type(subs, &mut tp, span);
            }
         },
         Type::And(ts) => {
            for mut ct in ts.iter_mut() {
               self.reduce_type(subs, &mut ct, span);
            }
            ts.sort(); ts.dedup();
            /* TODO: complain if there are two conflicting constants for one value
            if ts.clone().iter().filter(|tt| tt.is_constant()).count() > 1 {
               let dept = ts.clone().into_iter().filter(|tt| tt.is_constant()).collect::<Vec<Type>>();
               let mut trad = ts.clone().into_iter().filter(|tt| !tt.is_constant()).collect::<Vec<Type>>();
               let d1 = dept[0].clone();
               for d2 in &dept[1..] {
                  panic!("type has two conflicting dependent values: {}, {}",
                         self.print_type(&HashMap::new(), &d1),
                         self.print_type(&HashMap::new(), &d2));
               }
               ts.clear();
               ts.push(d1);
               ts.append(&mut trad);
            }
            */
         },
         Type::Tuple(ts) => {
            for mut ct in ts.iter_mut() {
               self.reduce_type(subs, &mut ct, span);
            }
         },
         Type::Product(ts) => {
            for mut ct in ts.iter_mut() {
               self.reduce_type(subs, &mut ct, span);
            }
         },
         Type::Arrow(ref mut p, ref mut b) => {
            self.reduce_type(subs, p, span);
            self.reduce_type(subs, b, span);
         },
         Type::Ratio(ref mut p, ref mut b) => {
            self.reduce_type(subs, p, span);
            self.reduce_type(subs, b, span);
         },
      }
   }
   pub fn untyped_eval(&mut self, subs: &HashMap<Type,Type>, t: &mut TermId) -> Option<Constant> {
      //reduce constant expressions in untyped context
      //designed for use inside of dependent type signatures

      match self.rows[t.id].term.clone() {
         //evaluation can change the t.id of a term to the canonical t.id of a constant
         Term::Block(_sid,es) if es.len()==0 => {
            let c = Constant::NaN;
            t.id = self.push_constant(&c, *t).id;
            return Some(c);
         },
         Term::Value(v) => {
            if let Some(c) = self.parse_constant(&v) {
               t.id = self.push_constant(&c, *t).id;
               return Some(c);
            };
         },
         Term::Constructor(cn,ref mut _fts) => {
            if let Some(c) = self.parse_constant(&cn) {
               t.id = self.push_constant(&c, *t).id;
               return Some(c);
            };
         },
         Term::Ident(g) => {
            if let Some(c) = self.parse_constant(&g) {
               t.id = self.push_constant(&c, *t).id;
               return Some(c);
            }
            for (k,v) in subs.iter() {
               if let Type::Constant(_kv,kt) = k {
               if let Type::Constant(_vv,vt) = v {
                  //variable substitution
                  if let Term::Ident(kg) = self.rows[kt.id].term.clone() {
                  if g==kg {
                     t.id = vt.id;
                     return self.maybe_constant(*t);
                  }}
               }}
            }
         },
         Term::App(ref mut g,ref mut x) => {
            let gc = self.untyped_eval(subs,g);
            let xc = self.untyped_eval(subs,x);
            match (gc,xc) {
               (Some(Constant::Op(uop)),Some(Constant::Boolean(x))) => {
                  let c = if uop=="not" { Constant::Boolean(!x) }
                     else { Constant::NaN };
                  t.id = self.push_constant(&c, *t).id;
                  return Some(c);
               },
               (Some(Constant::Op(uop)),Some(Constant::Integer(x))) => {
                  let c = if uop=="pos" { Constant::Integer(x) }
                     else if uop=="neg" { Constant::Integer(-x) }
                     else { Constant::NaN };
                  t.id = self.push_constant(&c, *t).id;
                  return Some(c);
               }, (Some(Constant::Op(bop)),
                   Some(Constant::Tuple(ps))) if ps.len()==2 => {
                  match (&ps[0], &ps[1]) {
                     (Constant::Boolean(a),Constant::Boolean(b)) => {
                        let c = if bop=="&&" { Constant::Boolean(*a && *b) }
                           else if bop=="||" { Constant::Boolean(*a || *b) }
                           else { Constant::NaN };
                        t.id = self.push_constant(&c, *t).id;
                        return Some(c);
                     }, _ => {},
                  };
                  let a = if let Constant::Integer(a) = ps[0] { a
                  } else { return Some(Constant::NaN); };
                  let b = if let Constant::Integer(b) = ps[1] { b
                  } else { return Some(Constant::NaN); };
                  if b==0 && (bop=="/" || bop=="%") {
                     let c = Constant::NaN;
                     t.id = self.push_constant(&c, *t).id;
                     return Some(c);
                  }
                  let c = if bop=="+" { Constant::Integer(a + b) }
                     else if bop=="-" { Constant::Integer(a - b) }
                     else if bop=="*" { Constant::Integer(a * b) }
                     else if bop=="/" { Constant::Integer(a / b) }
                     else if bop=="%" { Constant::Integer(a % b) }
                     else if bop=="^" { Constant::Integer(a.pow(b as u32)) }
                     else if bop=="<"  { Constant::Boolean(a < b) }
                     else if bop=="<=" { Constant::Boolean(a <= b) }
                     else if bop==">"  { Constant::Boolean(a > b) }
                     else if bop==">=" { Constant::Boolean(a >= b) }
                     else if bop=="==" { Constant::Boolean(a == b) }
                     else if bop=="!=" { Constant::Boolean(a != b) }
                     else { Constant::NaN };
                  t.id = self.push_constant(&c, *t).id;
                  return Some(c);
               }, (Some(Constant::Op(top)),
                   Some(Constant::Tuple(ps))) if ps.len()==3 => {
                  let x = if let Constant::Boolean(a) = ps[0] {
                     let b = ps[1].clone();
                     let c = ps[2].clone();
                          if top=="if" && a { b }
                     else if top=="if" && !a { c }
                     else { Constant::NaN }
                  } else { Constant::NaN };
                  t.id = self.push_constant(&x, *t).id;
                  return Some(x);
               }, (Some(gc),Some(xc)) => {
                  panic!("TODO: apply {:?} ( {:?} )", gc, xc);
               },
               _ => {},
            }
         },
         Term::Tuple(ref mut ts) => {
            let mut all_const = true;
            let mut consts = Vec::new();
            for tc in ts.iter_mut() {
               if let Some(cc) = self.untyped_eval(subs,tc) {
                  consts.push(cc);
               } else {
                  all_const = false;
               }
            }
            if all_const {
               let tsc = Constant::Tuple(consts);
               t.id = self.push_constant(&tsc, *t).id;
               return Some(tsc);
            }
         },
         _ => panic!("TODO: untyped eval {}", self.print_term(*t)),
      };
      None
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
         Term::Constructor(_cn,fts) => {
            for (_f,ft) in fts.iter() {
               self.untyped(*ft);
            }
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
            let kinds = HashMap::new();
            let mut subs = HashMap::new();
            if let Ok(_) = lt.unify_impl(&kinds, &mut subs, &n) {
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
            let kinds = HashMap::new();
            let mut subs = HashMap::new();
            if let Ok(_) = lt.unify_impl(&kinds, &mut subs, &d) {
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
   pub fn cast_into_kind(&mut self, mut l_only: Type, into: &Type, span: &Span) -> Result<Type,Error> {

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
               let kinds = HashMap::new();
               let mut subs = HashMap::new();
               if let Ok(_) = lt.unify_impl(&kinds, &mut subs, &n) {
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
               let kinds = HashMap::new();
               let mut subs = HashMap::new();
               if let Ok(_) = lt.unify_impl(&kinds, &mut subs, &d) {
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

   pub fn unify_varnames(&mut self, dept: &mut HashMap<String,TermId>, t: &mut TermId) {
      match self.rows[t.id].term.clone() {
         Term::Ident(tn) => {
            if ["self","if","not","pos","neg","+","-","*","/","%","^","==","!=","<","<=",">",">=","&&","||"].contains(&tn.as_str()) {
               //pass
            } else if let Some(v) = dept.get(&tn) {
               let nn = format!("var#{}", v.id);
               self.rows[t.id].term = Term::Ident(nn);
               t.id = v.id; //clobber the namespace
            } else {
               let nn = format!("var#{}", t.id);
               dept.insert(tn.clone(), *t);
               self.rows[t.id].term = Term::Ident(nn);
            }
         },
         Term::Value(_) => {},
         Term::Block(_sid,ref mut es) => {
            for e in es.iter_mut() {
               self.unify_varnames(dept,e);
            }
         },
         Term::Tuple(ref mut es) => {
            for e in es.iter_mut() {
               self.unify_varnames(dept,e);
            }
         },
         Term::Let(_,_,_,_,_,_) => {
            panic!("TODO: unify_varnames in Let term")
         },
         Term::App(ref mut g,ref mut x) => {
            self.unify_varnames(dept,g);
            self.unify_varnames(dept,x);
         },
         Term::Ascript(ref mut t,_tt) => {
            self.unify_varnames(dept,t);
         },
         Term::As(ref mut t,_tt) => {
            self.unify_varnames(dept,t);
         },
         Term::Constructor(_c,ref mut kts) => {
            for (_k,t) in kts.iter_mut() {
               self.unify_varnames(dept,t);
            }
         },
         Term::Substitution(ref mut e,ref mut a,ref mut b) => {
            self.unify_varnames(dept,e);
            self.unify_varnames(dept,a);
            self.unify_varnames(dept,b);
         }
      }
   }
   pub fn are_terms_equal(&self, lt: TermId, rt: TermId) -> bool {
      match (&self.rows[lt.id].term,&self.rows[rt.id].term) {
         (Term::Ident(lv),Term::Ident(rv)) => lv==rv,
         (Term::Value(lv),Term::Value(rv)) => lv==rv,
         (Term::Constructor(lc,ls),Term::Constructor(rc,rs)) => lc==rc && ls.len()==0 && rs.len()==0,
         (Term::App(lp,lb),Term::App(rp,rb)) => self.are_terms_equal(*lp,*rp) && self.are_terms_equal(*lb,*rb),
         (Term::Tuple(ls),Term::Tuple(rs)) => ls.len()==rs.len() && std::iter::zip(ls,rs).all(|(lc,rc)|self.are_terms_equal(*lc,*rc)),
         _ => false,
      }
   }
   pub fn alpha_convert_type(&mut self, tt: &Type, lt: TermId, rt: TermId) -> Type {
      match tt {
         Type::Any => tt.clone(),
         Type::Ident(_,_) => tt.clone(),
         Type::Arrow(_,_) => tt.clone(), //the inner types here are guarded
         Type::Constant(v,c) => Type::Constant(*v, self.alpha_convert_term(*c,lt,rt)),
         Type::Ratio(nt,dt) => Type::Ratio(
            Box::new(self.alpha_convert_type(nt,lt,rt)),
            Box::new(self.alpha_convert_type(dt,lt,rt))
         ),
         Type::And(ts) => Type::And( ts.iter().map(|ct|self.alpha_convert_type(ct,lt,rt)).collect::<Vec<Type>>() ),
         Type::Tuple(ts) => Type::Tuple( ts.iter().map(|ct|self.alpha_convert_type(ct,lt,rt)).collect::<Vec<Type>>() ),
         Type::Product(ts) => Type::Product( ts.iter().map(|ct|self.alpha_convert_type(ct,lt,rt)).collect::<Vec<Type>>() ),
      }
   }
   pub fn alpha_convert_term(&mut self, et: TermId, lt: TermId, rt: TermId) -> TermId {
      if self.are_terms_equal(et,lt) {
         self.rows[et.id].term = self.rows[rt.id].term.clone();
         return et;
      }
      let mut mut_rec = Vec::new();
      match &self.rows[et.id].term {
         Term::App(ep,eb) => { mut_rec.push(*ep); mut_rec.push(*eb); }
         Term::Tuple(es) => {
            for ct in es.iter() {
               mut_rec.push(*ct);
            }
         },
         _ => {},
      }
      for rec in mut_rec.into_iter() {
         self.alpha_convert_term(rec, lt, rt);
      }
      et
   }
   pub fn is_exhaustive(&mut self, t: TermId) -> Option<(TermId,TermId,TermId,TermId)> {
      if let Term::App(or1,app1) = &self.rows[t.id].term {
      if let Term::Ident(orop) = &self.rows[or1.id].term {
      if orop=="||" {
      if let Term::Tuple(app1s) = &self.rows[app1.id].term {
      if app1s.len()==2 {
         if let Term::App(or2,app2) = &self.rows[app1s[0].id].term {
         if let Term::Ident(orop) = &self.rows[or2.id].term {
         if orop=="||" {
         if let Term::Tuple(app2s) = &self.rows[app2.id].term {
         if app2s.len()==2 {
            let prop = app1s[1];
            if let Term::App(gt,gt1) = &self.rows[app2s[0].id].term {
            if let Term::Ident(gtop) = &self.rows[gt.id].term {
            if gtop==">" {
            if let Term::Tuple(gt1s) = &self.rows[gt1.id].term {
            if gt1s.len()==2 {
               let lower_bounds = gt1s[0];
               let lower_i = gt1s[1];
               if let Term::App(gt,gt2) = &self.rows[app2s[1].id].term {
               if let Term::Ident(gtop) = &self.rows[gt.id].term {
               if gtop==">" {
               if let Term::Tuple(gt2s) = &self.rows[gt2.id].term {
               if gt2s.len()==2 {
                  let upper_i = gt2s[0];
                  let upper_bounds = gt2s[1];
                  if let Term::Ident(i1) = &self.rows[lower_i.id].term {
                  if let Term::Ident(i2) = &self.rows[upper_i.id].term {
                  if i1==i2 {
                     return Some((lower_bounds,lower_i,upper_bounds,prop));
                  }}}
               }}}}}
            }}}}}
         }}}}}
      }}}}}
      None
   }
   pub fn check_invariants(&mut self, t: TermId) -> Result<(),Error> {
      let mut ground_types = Vec::new();
      let mut subs = HashMap::new();
      let self_term = Term::Ident("self".to_string());
      let self_termid = self.push_term(self_term.clone(), &self.rows[t.id].span.clone());
      self.untyped(self_termid);
      let self_type = self.push_dep_type(&self_term, self_termid);
      match &self.rows[t.id].typ {
         Type::Ident(tn,ts) => {
            ground_types.push(Type::Ident(tn.clone(),ts.clone()));
         },
         Type::And(tcs) => {
            for tc in tcs.iter() {
            match tc {
               Type::Ident(tn,ts) => {
                  ground_types.push(Type::Ident(tn.clone(),ts.clone()));
               }, Type::Constant(v,ct) => {
                  subs.insert(self_type.clone(), Type::Constant(*v,*ct));
               }, _ => {},
            }}
         },
         _ => {},
      }
      for g in ground_types.iter() {
      if let Type::Ident(tn,_ts) = g {
      if let Some(ti) = self.typedef_index.get(tn) {
      if let TypeRule::Typedef(_cname,_normal,_tiks,_imp,_cons,_k,inv,_span) = &self.rules[*ti] {
         for invariant in inv.clone().iter() {
            let p = self.untyped_eval(&subs, &mut invariant.prop.clone());
            let a = self.untyped_eval(&subs, &mut invariant.algs.clone());
            if p.is_some() && p==a {
               //pass
            } else if let Some((mut low,i,mut high,prop)) = self.is_exhaustive(invariant.prop) {
               if let Some(Constant::Integer(low)) = self.untyped_eval(&subs, &mut low) {
               if let Some(Constant::Integer(high)) = self.untyped_eval(&subs, &mut high) {
                  let i_type = self.push_dep_type(&self.rows[i.id].term.clone(), i);
                  for ival in low..=high {
                     let mut prop_mut = prop;

                     let ic = Constant::Integer(ival);
                     let it = self.push_constant(&ic, i);
                     subs.insert(i_type.clone(), Type::Constant(false,it));

                     if let Some(Constant::Boolean(true)) = self.untyped_eval(&subs, &mut prop_mut) {
                        //pass
                     } else {
                        let st = subs.get(&self_type).unwrap_or(&Type::Any);
                        return Err(Error {
                           kind: "Type Error".to_string(),
                           rule: format!("invariant not satisfied for self={}, {}={}: {}",
                                 self.print_type(&HashMap::new(), st),
                                 self.print_term(i), ival, self.print_term(prop)),
                           span: self.rows[t.id].span.clone(),
                        })
                     }
                  }
               }}
            } else {
               return Err(Error {
                  kind: "Type Error".to_string(),
                  rule: format!("invariant not satisfied {}: {} | {}", tn, self.print_term(invariant.prop), self.print_term(invariant.algs)),
                  span: self.rows[t.id].span.clone(),
               })
            }
         }
      }}}}
      Ok(())
   }

   pub fn typeck(&mut self, scope: Option<ScopeId>, t: TermId, implied: Option<Type>) -> Result<(),Error> {
      let implied = implied.map(|tt|tt.normalize());
      //clone is needed to avoid double mutable borrows?
      match self.rows[t.id].term.clone() {
         Term::Block(sid,es) => {
            let mut last_typ = self.nil_type.clone();
            for e in es.iter() {
               self.typeck(Some(sid), *e, None)?;
               last_typ = self.rows[e.id].typ.clone();
            }
            self.rows[t.id].typ = self.unify(&last_typ, &self.rows[t.id].typ.clone(), &self.rows[t.id].span.clone())?;
         },
         Term::Tuple(es) => {
            let mut ts = Vec::new();
            for e in es.iter() {
               self.typeck(scope, *e, None)?;
               ts.push(self.rows[e.id].typ.clone());
            }
            self.rows[t.id].typ = self.unify(&Type::Tuple(ts), &self.rows[t.id].typ.clone(), &self.rows[t.id].span.clone())?;
         },
         Term::Let(s,v,_ps,b,rt,_rk) => {
            if v=="" {
               //term is untyped
               self.untyped(t);
            } else if let Some(ref b) = b {
               self.typeck(Some(s), *b, Some(rt.clone()))?;
               self.rows[t.id].typ = self.nil_type.clone();
            } else {
               self.rows[t.id].typ = self.nil_type.clone();
            }
            self.soundck(&rt, &self.rows[t.id].span.clone())?;
         },
         Term::Ascript(x,tt) => {
            self.typeck(scope.clone(), x, Some(tt.clone()))?;
            self.rows[t.id].typ = self.unify(&self.rows[x.id].typ.clone(), &tt, &self.rows[t.id].span.clone())?;
         },
         Term::As(x,into) => {
            self.typeck(scope.clone(), x, None)?;
            let into_kind = self.kind(&into).first();
            if let Ok(nt) = self.unify(&self.rows[x.id].typ.clone(), &into, &self.rows[t.id].span.clone()) {
               //if cast is already satisfied, do nothing
               self.rows[t.id].typ = self.unify(&nt, &self.rows[t.id].typ.clone(), &self.rows[t.id].span.clone())?;
            } else {
               if self.is_knormal(&into_kind) {
                  let l_only = self.project_kinded(&into_kind, &self.rows[x.id].typ.clone());
                  let l_alts = self.remove_kinded(&into_kind, &self.rows[x.id].typ.clone());
                  let l_only = self.cast_into_kind(l_only, &into, &self.rows[t.id].span.clone())?;

                  //quod erat demonstrandum
                  self.rows[t.id].typ = self.unify(&l_only,&into,&self.rows[t.id].span.clone())?.and(&l_alts);
               } else {
                  let mut accept = false;
                  for tr in self.rules.clone().iter() { match tr {
                     TypeRule::Forall(_itks,Inference::Imply(lt,rt),_term,k,_) if k==&into_kind => {
                        if let Ok(lt) = self.unify(&self.rows[x.id].typ.clone(), lt, &self.rows[t.id].span.clone()) {
                        if let Ok(rt) = self.unify(&rt, &into, &self.rows[t.id].span.clone()) {
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
                     })
                  }
               }
            }
            self.check_invariants(t)?;
         },
         Term::Ident(x) => {
            self.rows[t.id].typ = self.typeof_var(&scope, &x, &implied, &self.rows[t.id].span.clone())?;
         },
         Term::Value(x) => {
            let i = if let Some(ref i) = implied { i.clone() } else { self.bottom_type.clone() };
            let ki = self.project_kinded(&self.term_kind, &i);
            let alt_i = self.remove_kinded(&self.constant_kind, &i); //implied values are absurd
            let alt_i = alt_i.and(&self.push_dep_type(&Term::Value(x.clone()),t));
            let mut r = None;
            for (pat,re) in self.regexes.clone().into_iter() {
               if let Ok(nt) = self.unify(&ki,&pat,&self.rows[t.id].span.clone()) { //Term kinded is not []
                  r = Some(re.clone());
                  self.rows[t.id].typ = nt.and(&alt_i);
                  break;
               }
               else if ki==self.bottom_type && re.is_match(&x) { //Term kinded is []
                  r = Some(re.clone());
                  self.rows[t.id].typ = pat.and(&alt_i);
                  break;
               }
            }
            if let Some(re) = r {
               if !re.is_match(&x) {
                  return Err(Error {
                     kind: "Type Error".to_string(),
                     rule: format!("type {:?} rejected the literal {}", i, x),
                     span: self.rows[t.id].span.clone(),
                  })
               }
            } else {
               return Err(Error {
                  kind: "Type Error".to_string(),
                  rule: format!("type {:?} is not literal: {}", i, x),
                  span: self.rows[t.id].span.clone(),
               })
            }
            self.check_invariants(t)?;
	 },
         Term::App(g,x) => {
            self.typeck(scope.clone(), x, None)?;
            self.typeck(scope.clone(), g, Some(
               Type::Arrow(Box::new(self.rows[x.id].typ.clone()),
                          Box::new(Type::Any))
            ))?;
            self.rows[x.id].typ = self.unify_par(&self.rows[x.id].typ.clone(), &self.rows[g.id].typ.domain(), &self.rows[x.id].span.clone(), IsParameter::Yes)?; //x => domain(f(x))
            self.rows[t.id].typ = self.unify_par(&self.rows[t.id].typ.clone(), &self.rows[g.id].typ.range(), &self.rows[t.id].span.clone(), IsParameter::No)?; //f(x) => range(f(x))
         },
         Term::Constructor(cname,kvs) => {
            for (_k,v) in kvs.clone().into_iter() {
               self.typeck(scope.clone(), v, None)?;
            }
            if let Some((ref tt,_tpars,_tkvs)) = self.constructors.get(&cname) {
               self.rows[t.id].typ = tt.clone();
               self.rows[t.id].typ = self.rows[t.id].typ.and(&Type::Ident(cname.clone(),Vec::new())).normalize();
            } else { return Err(Error {
               kind: "Type Error".to_string(),
               rule: format!("type constructor, none found for: {}", self.print_term(t)),
               span: self.rows[t.id].span.clone(),
            }) }
         },
         Term::Substitution(e,a,b) => {
            self.typeck(scope.clone(), e, None)?;
            let mut et = self.rows[e.id].typ.clone();
            self.reduce_type(&HashMap::new(), &mut et, &self.rows[t.id].span.clone());
            let mut et = self.alpha_convert_type(&et, a, b);
            self.reduce_type(&HashMap::new(), &mut et, &self.rows[t.id].span.clone());
            self.rows[e.id].typ = et.clone();
            self.rows[t.id].typ = et.clone();
         },
      };
      if let Some(implied) = implied {
         if let Type::Arrow(_p,_b) = implied {
            //arrow unification can narrow the type signature of the arrow
            //e.g.   {Integer+[1]} => ?
            //yields [1]           => [2]
            //this is ok
         } else {
            self.rows[t.id].typ = self.unify(&self.rows[t.id].typ.clone(), &implied, &self.rows[t.id].span.clone())?;
         }
      }
      self.soundck(&self.rows[t.id].typ.clone(), &self.rows[t.id].span.clone())?;
      Ok(())
   }
   pub fn unify_par(&mut self, lt: &Type, rt: &Type, span: &Span, par: IsParameter) -> Result<Type,Error> {
      let kinds = HashMap::new();
      self.unify_with_kinds(&kinds, lt, rt, span, par)
   }
   pub fn unify(&mut self, lt: &Type, rt: &Type, span: &Span) -> Result<Type,Error> {
      let kinds = HashMap::new();
      self.unify_with_kinds(&kinds, lt, rt, span, IsParameter::Top)
   }
   pub fn unify_with_kinds(&mut self, kinds: &HashMap<Type,Kind>, lt: &Type, rt: &Type, span: &Span, par: IsParameter) -> Result<Type,Error> {
      //lt => rt
      let mut subs = HashMap::new();
      let mut lt = self.extend_implied(lt);
      self.reduce_type(&subs, &mut lt, span); //reduce constant expressions in dependent types
      lt = lt.normalize();
      let mut rt = rt.clone();
      self.reduce_type(&subs, &mut rt, span);
      rt = rt.normalize();
      if let Ok(ref mut tt) = lt.unify_impl_par(kinds, &mut subs, &rt, par) {
         self.reduce_type(&subs, tt, span);
         let tt = tt.normalize();
         Ok(tt)
      } else { return Err(Error {
         kind: "Type Error".to_string(),
         rule: format!("failed unification {} (x) {}", self.print_type(kinds,&lt), self.print_type(kinds,&rt)),
         span: span.clone(),
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
      let constant_index_l = self.constant_index.clone();
      let tconstant_index_l = self.tconstant_index.clone();

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
      self.constant_index = constant_index_l;
      self.tconstant_index = tconstant_index_l;

      r?; Ok(())
   }
}
