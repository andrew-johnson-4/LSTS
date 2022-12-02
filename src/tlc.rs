use std::rc::Rc;
use std::collections::{HashSet,HashMap};
use regex::Regex;
use crate::term::{Term,TermId};
use crate::scope::{Scope,ScopeId};
use crate::typ::{Type,InArrow};
use crate::kind::Kind;
use crate::token::{Span,TokenReader,tokenize_string,tokenize_file,span_of};
use crate::constant::Constant;
use crate::debug::Error;
use crate::ll::ll1_file;

pub struct TLC {
   pub strict: bool,
   pub rows: Vec<Row>,
   pub hints: HashMap<String,ForallRule>,
   pub rules: Vec<TypeRule>,
   pub scopes: Vec<Scope>,
   pub value_regexes: Vec<(String,Regex)>,
   pub regexes: Vec<(Type,Rc<Regex>)>,
   pub constructors: HashMap<String,(Type,Vec<Type>,Vec<(String,Type)>)>,
   pub type_is_normal: HashSet<Type>,
   pub kind_is_normal: HashSet<Kind>,
   pub typedef_index: HashMap<String,usize>,
   pub foralls_index: HashMap<Type,Vec<usize>>,
   pub foralls_rev_index: HashMap<Type,Vec<usize>>,
   pub term_kind: Kind,
   pub constant_kind: Kind,
   pub nil_type: Type,
   pub bottom_type: Type,
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
pub enum TypedefBranch {
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
pub struct TypedefRule {
   pub name: String,
   pub is_normal: bool,
   pub is_constant: bool,
   pub parameters: Vec<(String,Option<Type>,Kind)>,
   pub implies: Option<Type>,
   pub definition: Vec<TypedefBranch>, //having a definition implies that the term is a Valued type
   pub invariants: Vec<Invariant>,
   pub kind: Kind,
   pub span: Span,
}

#[derive(Clone)]
pub struct ForallRule {
   pub axiom: bool,
   pub name: Option<String>,
   pub parameters: Vec<(Option<String>,Option<Type>,Kind)>,
   pub scope: ScopeId,
   pub inference: Inference,
   pub rhs: Option<TermId>,
   pub kind: Kind,
   pub span: Span,
}

#[derive(Clone)]
pub enum TypeRule {
   Typedef(TypedefRule),
   Forall(ForallRule),
}
impl TypeRule {
   pub fn span(&self) -> Span {
      match self {
         TypeRule::Typedef(tr) => tr.span.clone(),
         TypeRule::Forall(fr) => fr.span.clone(),
      }
   }
}
impl std::fmt::Debug for TypeRule {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
           TypeRule::Typedef(tr) => write!(f, "{}{}{}{}{}::{:?}",
              tr.name,
              if tr.parameters.len()==0 { "" } else { "<" },
              tr.parameters.iter().map(|(t,i,k)| format!("{:?}:{:?}::{:?}",
                    t.clone(),
                    i.clone().unwrap_or(Type::Any),
                    k
              )).collect::<Vec<String>>().join(","),
              if tr.parameters.len()==0 { "" } else { ">" },
              if let Some(ref ti) = tr.implies { format!(":{:?}",ti) } else { format!("") },
              tr.kind
           ),
           TypeRule::Forall(fr) => write!(f, "forall {}. {:?} :: {:?}", 
              fr.parameters.iter().map(|(i,t,k)| format!("{:?}:{:?}::{:?}",
                    i.clone().unwrap_or("_".to_string()),
                    t.clone().unwrap_or(Type::And(Vec::new())),
                    k,
              )).collect::<Vec<String>>().join(","),
              fr.inference,
              fr.kind,
           ),
        }
    }
}

impl TLC {
   pub fn new() -> TLC {
      TLC {
         strict: false,
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
         value_regexes: Vec::new(),
         regexes: Vec::new(),
         hints: HashMap::new(),
         constructors: HashMap::new(),
         foralls_index: HashMap::new(),
         foralls_rev_index: HashMap::new(),
         typedef_index: HashMap::new(),
         type_is_normal: HashSet::new(),
         kind_is_normal: HashSet::new(),
         term_kind: Kind::Named("Term".to_string(),Vec::new()),
         constant_kind: Kind::Named("Constant".to_string(),Vec::new()),
         nil_type: Type::Tuple(Vec::new()),
         bottom_type: Type::And(Vec::new()),
      }
   }
   pub fn strict(mut self) -> TLC {
      self.strict = true;
      self
   }
   pub fn print_type(&self, kinds: &HashMap<Type,Kind>, tt: &Type) -> String {
      let ts = match tt {
         Type::Any => format!("?"),
         Type::Named(t,ts) => {
            if ts.len()==0 { format!("{}", t) }
            else { format!("{}<{}>", t, ts.iter().map(|t|self.print_type(kinds,t)).collect::<Vec<String>>().join(",") ) }
         }
         Type::And(ts) => format!("{{{}}}", ts.iter().map(|t|self.print_type(kinds,t)).collect::<Vec<String>>().join("+") ),
         Type::Tuple(ts) => format!("({})", ts.iter().map(|t|self.print_type(kinds,t)).collect::<Vec<String>>().join(",") ),
         Type::Product(ts) => format!("({})", ts.iter().map(|t|self.print_type(kinds,t)).collect::<Vec<String>>().join("*") ),
         Type::Arrow(p,b) => format!("({})->({})", self.print_type(kinds,p), self.print_type(kinds,b)),
         Type::Ratio(n,d) => format!("({})/({})", self.print_type(kinds,n), self.print_type(kinds,d)),
         Type::Constant(ct,cv) => {
            if let Some(cv) = cv {
               format!("[{}#{} = {:?}]", self.print_term(*ct), ct.id, cv)
            } else {
               format!("[{}#{}]", self.print_term(*ct), ct.id)
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
         Term::Arrow(p,b) => format!("({} -> {})", self.print_term(*p), self.print_term(*b)),
         Term::App(g,x) => format!("{}({})", self.print_term(*g), self.print_term(*x)),
         Term::Let(lt) => format!("let {}", lt.name),
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
         Term::RuleApplication(t,n) => format!("{} @{}", self.print_term(*t), n),
         Term::Literal(t) => format!("|{}|", self.print_term(*t)),
      }
   }
   pub fn push_forall(&mut self, globals: ScopeId, axiom: bool, name: Option<String>, quants: Vec<(Option<String>,Option<Type>,Kind)>,
                             inference: Inference, term: Option<TermId>, kind: Kind, span: Span) {
      let mut fa_closed: Vec<(String,HashMap<Type,Kind>,Type,Option<TermId>)> = Vec::new();
      for (qn,qt,qk) in quants.iter() {
      if let Some(qn) = qn {
         let qt = if let Some(qt) = qt { qt.clone() } else { Type::Any };
         let mut fk = HashMap::new();
         fk.insert(qt.clone(), qk.clone());
         fa_closed.push( (qn.clone(), fk, qt.clone(), None) );
      }}
      let fa_scope = self.push_scope(Scope {
         parent: Some(globals),
         children: fa_closed,
      }, &span);
      let fi = self.rules.len();
      let fa = ForallRule {
         axiom: axiom,
         name: name.clone(),
         parameters: quants,
         scope: fa_scope,
         inference: inference.clone(),
         rhs: term,
         kind: kind,
         span: span
      };
      if let Some(h) = name {
         self.hints.insert(h, fa.clone());
      }
      self.rules.push(TypeRule::Forall(fa));
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
      let tks = tokenize_string(self, "[string]", src)?;
      self.compile_doc(globals, "[string]", tks)
   }
   pub fn import_str(&mut self, globals: Option<ScopeId>, src:&str) -> Result<ScopeId,Error> {
      let tks = tokenize_string(self, "[string]", src)?;
      self.compile_doc(globals, "[string]", tks)?;
      Ok(ScopeId {id:0})
   }
   pub fn parse_file(&mut self, globals: Option<ScopeId>, filename:&str) -> Result<TermId,Error> {
      let mut tks = tokenize_file(self, filename)?;
      let file_scope = globals.unwrap_or(self.push_scope(Scope {
         parent: None,
         children: Vec::new(),
      }, &span_of(&mut tks)));
      Ok(ll1_file(self, file_scope, &mut tks)?)
   }
   pub fn check_file(&mut self, globals: Option<ScopeId>, filename:&str) -> Result<ScopeId,Error> {
      let ast = self.parse_file(globals, filename)?;
      self.compile_rules(filename)?;
      self.typeck(&globals, ast, None)?;
      self.sanityck()?;
      Ok(ScopeId {id:0})
   }
   pub fn import_file(&mut self, globals: Option<ScopeId>, filename:&str) -> Result<ScopeId,Error> {
      let ast = self.parse_file(globals, filename)?;
      self.compile_rules(filename)?;
      self.typeck(&globals, ast, None)?;
      self.sanityck()?;
      Ok(ScopeId {id:0})
   }
   pub fn compile_doc(&mut self, globals: Option<ScopeId>, docname:&str, mut tokens: TokenReader) -> Result<TermId,Error> {
      let file_scope = globals.unwrap_or(self.push_scope(Scope {
         parent: None,
         children: Vec::new(),
      }, &span_of(&mut tokens)));
      let ast = ll1_file(self, file_scope, &mut tokens)?;
      self.compile_rules(docname)?;
      self.typeck(&globals, ast, None)?;
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
         Type::Named(cn,_) => {
            if let Some(t) = kinds.get(tt) { return Some(t.clone()); }
            let mut k = None;
            if let Some(ti) = self.typedef_index.get(cn) {
            if let TypeRule::Typedef(tr) = &self.rules[*ti] {
               kinds.insert(tt.clone(), tr.kind.clone());
               k = Some(tr.kind.clone());
            }}
            k
         },
      }
   }
   pub fn compile_rules(&mut self, _docname:&str) -> Result<(),Error> {
      for rule in self.rules.clone().iter() { match rule {
         TypeRule::Forall(fr) => { if self.strict && !fr.axiom {
            if let Some(ref rhs) = fr.rhs {
            if let Inference::Type(ref tt) = fr.inference {
               self.typeck(&Some(fr.scope), *rhs, Some(tt.clone()))?;
            }}
         }},
         TypeRule::Typedef(tr) => {
            for td in tr.definition.iter() { match td {
               TypedefBranch::Regex(pat) => {
                  if let Ok(r) = Regex::new(&pat[1..pat.len()-1]) {
                     self.regexes.push((Type::Named(tr.name.clone(),Vec::new()),Rc::new(r)));
                  } else {
                     panic!("typedef regex rejected: {}", pat);
                  }
               },
               TypedefBranch::Constructor(cname,kts) => {
                  self.constructors.insert(cname.clone(), (Type::Named(tr.name.clone(),Vec::new()),Vec::new(),kts.clone()));
               }
            }}
            for p in tr.invariants.iter() {
               self.untyped(p.prop);
               self.untyped(p.algs);
            }
         },
      }}

      Ok(())
   }
   pub fn parse(&mut self, src:&str) -> Result<TermId,Error> {
      //used mainly in tests
      let mut tokens = tokenize_string(self, "[string]", src)?;
      let file_scope = self.push_scope(Scope {
         parent: None,
         children: Vec::new(),
      }, &span_of(&mut tokens));
      ll1_file(self, file_scope, &mut tokens)
   }
   pub fn maybe_constant(&self, t: TermId) -> Option<Constant> {
      self.rows[t.id].constant.clone()
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
               self.push_constant(ti, &c);
               Type::Constant(ti,Some(c.clone()))
            } else {
               Type::Constant(ti,None)
            }
         }, Term::Ident(_) => {
            Type::Constant(ti,None)
         }, _ => Type::Constant(ti,None),
      }
   }
   pub fn get_term_type(&mut self, term: &Term, ti: TermId) -> (Type,Option<Constant>) {
      match term {
         Term::Value(ct) => {
            if let Some(c) = self.parse_constant(ct) {
               (Type::And(vec![Type::Any,Type::Constant(ti,Some(c.clone()))]), Some(c.clone()) )
            } else {
               (Type::Any,None)
            }
         }, _ => (Type::Any,None),
      }
   }
   pub fn push_term(&mut self, term: Term, span: &Span) -> TermId {
      let index = self.rows.len();
      let ti = TermId { id: index };
      let (tt,tc) = self.get_term_type(&term, ti);
      self.rows.push(Row {
         term: term,
         typ: tt,
         kind: self.term_kind.clone(),
         span: span.clone(),
         constant: tc,
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
            Term::Let(lt) => {
               rvars.append(&mut lt.rtype.vars());
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
            if let Type::Named(tn,_ts) = tc {
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
         Type::Named(tn,ts) => {
            if ts.len()==0 { return Ok(()); }
            if !tt.is_concrete() { return Ok(()); }
            if let Some(ti) = self.typedef_index.get(tn) {
            if let TypeRule::Typedef(tr) = &self.rules[*ti].clone() {
            if ts.len()==tr.parameters.len() {
               for (pt,(_bi,bt,_bk)) in std::iter::zip(ts,&tr.parameters) {
                  if let Some(bt) = bt {
                     self.implies(pt, &bt, span)?;
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
         Type::Named(tn,ts) => {
            let ts = ts.iter().map(|ct|self.extend_implied(ct)).collect::<Vec<Type>>();
            let mut implies: Vec<Type> = Vec::new();
            let mut subs = HashMap::new();

            //lookup typedefs
            if let Some(ti) = self.typedef_index.get(tn) {
            if let TypeRule::Typedef(tr) = &self.rules[*ti] {
               for ((ot,_it,_k),st) in std::iter::zip(tr.parameters.iter(), ts.iter()) {
                  subs.insert(Type::Named(ot.clone(),Vec::new()), st.clone());
               }
               if let Some(ref it) = tr.implies {
                  match self.extend_implied(it) {
                     Type::And(mut its) => { implies.append(&mut its); },
                     i => { implies.push(i); },
                  }
               }
            }}

            let mut ats = Vec::new();
            ats.push(Type::Named(tn.clone(),ts));
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
         Type::Constant(ct,cv) => Type::Constant(*ct,cv.clone())
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
         Type::Named(tn,ts) => self.type_is_normal.contains(&Type::Named(tn.clone(),Vec::new())) &&
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
         Type::Named(_t,_ts) => {
            //should named types protect their parameters from narrowing?
            let nk = if let Some(nk) = kinds.get(tt) { nk.clone() }
                     else { self.term_kind.clone() };
            if nk.has(projection) {
               tt.clone()
            } else {
               self.bottom_type.clone()
            }
         },
         Type::Arrow(tp,tb) => {
            let tp = self.narrow(kinds,projection,tp);
            if tp.is_bottom() { return tp.clone(); }
            //don't narrow return argument
            Type::Arrow( Box::new(tp), Box::new((**tb).clone()) )
         },
         Type::Ratio(tp,tb) => {
            let tp = self.narrow(kinds,projection,tp);
            if tp.is_bottom() { return tp.clone(); }
            let tb = self.narrow(kinds,projection,tb);
            if tb.is_bottom() { return tb.clone(); }
            Type::Ratio( Box::new(tp), Box::new(tb))
         },
         Type::And(ts) => {
            let mut cts = Vec::new();
            for ct in ts.iter() {
               let cp = self.narrow(kinds,projection,ct);
               if cp.is_bottom() { continue; }
               cts.push(cp);
            }
            if cts.len()==1 { cts[0].clone() }
            else { Type::And(cts) }
         },
         Type::Tuple(ts) => {
            let mut cts = Vec::new();
            for ct in ts.iter() {
               let cp = self.narrow(kinds,projection,ct);
               if cp.is_bottom() { return self.bottom_type.clone(); }
               cts.push(cp);
            }
            Type::Tuple(cts)
         },
         Type::Product(ts) => {
            let mut cts = Vec::new();
            for ct in ts.iter() {
               let cp = self.narrow(kinds,projection,ct);
               if cp.is_bottom() { return self.bottom_type.clone(); }
               cts.push(cp);
            }
            Type::Product(cts)
         },
         Type::Constant(_ct,_cv) => {
            if projection == &self.constant_kind {
               tt.clone()
            } else {
               self.bottom_type.clone()
            }
         }
      }
   }
   fn kflat(kinds: &HashMap<Type,Kind>) -> HashSet<Kind> {
      let mut ks = HashSet::new();
      for (_,nw) in kinds.iter() {
      for nw in nw.flatten() {
         ks.insert(nw.clone());
      }}
      ks
   }
   pub fn typeof_var(&mut self, scope: &Option<ScopeId>, v: &str, implied: &Option<Type>, span: &Span) -> Result<Type,Error> {
      if let Some(scope) = scope {
         let mut candidates = Vec::new();
         let mut matches = Vec::new();
         let ref sc = self.scopes[scope.id].clone();
         for (tn,tkts,tt,_vt) in sc.children.iter() {
            if tn==v {
               //match variable binding if
               //1) binding is not an arrow
               //2) implied => binding

               candidates.push(tt.clone());
               if let Type::Arrow(_tp,_tb) = &tt {
               if let Some(it) = implied {
                  let mut tkts = tkts.clone();
                  self.kinds_of(&mut tkts, &tt);
                  let ks = TLC::kflat(&tkts);
                  self.kinds_of(&mut tkts, &it);
                  for nw in ks.iter() {
                     let narrow_it = self.narrow(&tkts, nw, &it);
                     let rt = Type::implies(self, &narrow_it, &tt);
                     if rt.is_bottom() { continue; }
                     matches.push(rt.clone());
                  }
               }} else {
                  matches.push(tt.clone());
               }
            }
         }
         if matches.len()==1 {
            Ok(matches[0].clone())
         } else if matches.len()>1 {
            let rt = Type::And(matches);
            let rt = rt.normalize();
            //it is OK for multiple functions to match
            Ok(rt)
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
   pub fn reduce_type(&mut self, subs: &mut HashMap<String,Constant>, tt: &mut Type) {
      match tt {
         Type::Constant(c, cv) => {
            if cv.is_none() {
            if let Some(ncv) = self.untyped_eval(subs, c, false) {
               *cv = Some(ncv);
            }}
         },
         Type::Any => {},
         Type::Named(_tn,tps) => {
            for mut tp in tps.iter_mut() {
               self.reduce_type(subs, &mut tp);
            }
         },
         Type::And(ts) => {
            for mut ct in ts.iter_mut() {
               self.reduce_type(subs, &mut ct);
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
            for ref mut ct in ts.iter_mut() {
               self.reduce_type(subs, ct);
            }
         },
         Type::Product(ts) => {
            for ref mut ct in ts.iter_mut() {
               self.reduce_type(subs, ct);
            }
         },
         Type::Arrow(ref mut p, ref mut b) => {
            self.reduce_type(subs, p);
            self.reduce_type(subs, b);
         },
         Type::Ratio(ref mut p, ref mut b) => {
            self.reduce_type(subs, p);
            self.reduce_type(subs, b);
         },
      }
   }
   pub fn untyped_match(&mut self, subs: &mut HashMap<String,Constant>, gp: &mut TermId, gb: &mut TermId, v: Constant) -> Option<Constant> {
      self.untyped_destructure(subs, gp, &v);
      self.untyped_eval(subs, gb, true)
   }
   pub fn untyped_destructure(&mut self, subs: &mut HashMap<String,Constant>, gp: &TermId, v: &Constant) {
      match (&self.rows[gp.id].term.clone(), &v) {
         (Term::Ident(gn), v) => {
            subs.insert(gn.clone(), (*v).clone());
         },
         (Term::Tuple(gs), Constant::Tuple(vs)) if gs.len()==vs.len() => {
            for (gc,vc) in std::iter::zip(gs,vs) {
               self.untyped_destructure(subs, gc, vc);
            }
         },
         (_lhs, _rhs) => {
            unimplemented!("untyped_match {} = {:?}", self.print_term(*gp), v.clone())
         }
      }
   }
   pub fn untyped_eval(&mut self, subs: &mut HashMap<String,Constant>, t: &mut TermId, nested: bool) -> Option<Constant> {
      //reduce constant expressions in untyped context
      //designed for use inside of dependent type signatures

      if !nested { //short circuit known global constants
      if let Some(ct) = &self.rows[t.id].constant {
         return Some(ct.clone())
      }}
      match self.rows[t.id].term.clone() {
         //evaluation can change the t.id of a term to the canonical t.id of a constant
         Term::Block(_sid,es) if es.len()==0 => {
            return Some(self.push_constant(*t, &Constant::NaN));
         },
         Term::Value(v) => {
            if let Some(c) = self.parse_constant(&v) {
               return Some(self.push_constant(*t, &c));
            };
         },
         Term::Constructor(cn,ref mut _fts) => {
            if let Some(c) = self.parse_constant(&cn) {
               return Some(self.push_constant(*t, &c));
            };
         },
         Term::Ident(g) => {
            if let Some(c) = self.parse_constant(&g) {
               return Some(self.push_constant(*t, &c));
            }
            if let Some(c) = subs.get(&g) {
               return Some(self.push_constant(*t, &c));
            }
         },
         Term::App(ref mut g,ref mut x) => {
            let xc = self.untyped_eval(subs,x,true);
            if let Term::Arrow(ref mut gp,ref mut gb) = self.rows[g.id].term.clone() {
            if let Some(ref xc) = xc { //Argument must be constant, otherwise don't reduce the expression
               if let Some(c) = self.untyped_match(subs,gp,gb,xc.clone()) {
                  return Some(self.push_constant(*t, &c));
               }
            }}
            let gc = self.untyped_eval(subs,g,true);
            match (gc,xc) {
               (Some(Constant::Op(uop)),Some(Constant::Boolean(x))) => {
                  let c = if uop=="not" { Constant::Boolean(!x) }
                     else { Constant::NaN };
                  return Some(self.push_constant(*t, &c));
               },
               (Some(Constant::Op(uop)),Some(Constant::Integer(x))) => {
                  let c = if uop=="pos" { Constant::Integer(x) }
                     else if uop=="neg" { Constant::Integer(-x) }
                     else { Constant::NaN };
                  return Some(self.push_constant(*t, &c));
               }, (Some(Constant::Op(bop)),
                   Some(Constant::Tuple(ps))) if ps.len()==2 => {
                  match (&ps[0], &ps[1]) {
                     (Constant::Boolean(a),Constant::Boolean(b)) => {
                        let c = if bop=="&&" { Constant::Boolean(*a && *b) }
                           else if bop=="||" { Constant::Boolean(*a || *b) }
                           else { Constant::NaN };
                        return Some(self.push_constant(*t, &c));
                     }, _ => {},
                  };
                  let a = if let Constant::Integer(a) = ps[0] { a
                  } else { return Some(Constant::NaN); };
                  let b = if let Constant::Integer(b) = ps[1] { b
                  } else { return Some(Constant::NaN); };
                  if b==0 && (bop=="/" || bop=="%") {
                     return Some(self.push_constant(*t, &Constant::NaN));
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
                  return Some(self.push_constant(*t, &c));
               }, (Some(Constant::Op(top)),
                   Some(Constant::Tuple(ps))) if ps.len()==3 => {
                  let x = if let Constant::Boolean(a) = ps[0] {
                     let b = ps[1].clone();
                     let c = ps[2].clone();
                          if top=="if" && a { b }
                     else if top=="if" && !a { c }
                     else { Constant::NaN }
                  } else { Constant::NaN };
                  return Some(self.push_constant(*t, &x));
               }, (Some(gc),Some(xc)) => {
                  panic!("TODO: apply {:?} ( {:?} )", gc, xc);
               },
               _ => {},
            }
         },
         Term::Tuple(ref mut ts) => {
            let mut all_const = true;
            let mut consts = Vec::new();
            for ref mut tc in ts.iter_mut() {
               if let Some(cc) = self.untyped_eval(subs,tc,true) {
                  self.push_constant(**tc, &cc);
                  consts.push(cc);
               } else {
                  all_const = false;
               }
            }
            if all_const {
               let tsc = Constant::Tuple(consts);
               return Some(self.push_constant(*t, &tsc));
            }
         },
         Term::Arrow(_,_) => {}, //irreducible
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
         Term::Arrow(p,b) => { self.untyped(p); self.untyped(b); },
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
         Term::Let(lt) => {
            if let Some(b) = lt.body {
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
   pub fn cast_normal(&mut self, l_only: &Type, span: &Span) -> Result<Type,Error> {
      let mut num_collector = Vec::new();
      let mut den_collector = Vec::new();
      let (numerator,denominator) = l_only.project_ratio();

      for mut n in numerator.into_iter() {
         if self.is_normal(&n) {
            num_collector.push(n);
            continue;
         }

         if let Type::Named(nn,nns) = &n {
            let mut nns = nns.clone();
            for ni in 0..nns.len() {
            if !self.is_normal(&nns[ni]) {
               nns[ni] = self.cast_normal(&nns[ni], span)?;
            }}
            n = Type::Named(nn.clone(),nns);
         }

         if let Type::Named(nn,_nns) = &n {
         if let Some(ti) = self.typedef_index.get(nn) {
         if let TypeRule::Typedef(tr) = &self.rules[*ti] {
         let it = tr.implies.clone().unwrap_or(Type::Any);
         if self.is_normal(&it) {
            let (inum, iden) = it.project_ratio();
            num_collector.append(&mut inum.clone());
            den_collector.append(&mut iden.clone());
            continue;
         }}}}

         let mnt = n.mask();
         let mut found = false;
         if let Some(ref tis) = self.foralls_index.get(&mnt) {
         for ti in tis.iter() { if !found {
         if let TypeRule::Forall(ForallRule { inference:Inference::Imply(lt,rt), .. }) = &self.rules[*ti].clone() {
            let mut subs = Vec::new();
            let nt = Type::nored_implies(self, &mut subs, &lt, &n);
            if !nt.is_bottom() {
            if let Ok(subs) = Type::compile_subs(&subs) {
               let srt = rt.substitute(&subs);
               if self.is_normal(&srt) {
                  let (inum, iden) = srt.project_ratio();
                  num_collector.append(&mut inum.clone());
                  den_collector.append(&mut iden.clone());
                  found = true;
                  continue;
               }
            }}
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

         if let Type::Named(dn,dns) = &d {
            let mut dns = dns.clone();
            for ni in 0..dns.len() {
            if !self.is_normal(&dns[ni]) {
               dns[ni] = self.cast_normal(&dns[ni], span)?;
            }}
            d = Type::Named(dn.clone(),dns);
         }

         if let Type::Named(dn,_dns) = &d {
         if let Some(ti) = self.typedef_index.get(dn) {
         if let TypeRule::Typedef(tr) = &self.rules[*ti] {
         let it = tr.implies.clone().unwrap_or(Type::Any);
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
         if let TypeRule::Forall(ForallRule { inference:Inference::Imply(lt,rt), .. }) = &self.rules[*ti] {
            let mut subs = Vec::new();
            let nt = Type::nored_implies(self, &mut subs, &lt, &d);
            if !nt.is_bottom() {
            if let Ok(subs) = Type::compile_subs(&subs) {
               let srt = rt.substitute(&subs);
               if self.is_normal(&srt) {
                  let (inum, iden) = srt.project_ratio();
                  num_collector.append(&mut iden.clone());
                  den_collector.append(&mut inum.clone());
                  found = true;
                  continue;
               }
            }}
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
   pub fn implies(&mut self, lt: &Type, rt: &Type, span: &Span) -> Result<Type,Error> {
      self.arrow_implies(lt,rt,span,InArrow::No)
   }
   pub fn arrow_implies(&mut self, lt: &Type, rt: &Type, span: &Span, inarrow: InArrow) -> Result<Type,Error> {
      let ks = HashMap::new();
      let mut lt = lt.clone();
      let mut rt = rt.clone();
      let nt = Type::arrow_implies(self, &mut lt, &mut rt, inarrow);
      match nt {
         Type::And(nts) if nts.len()==0 => {
            Err(Error {
               kind: "Type Error".to_string(),
               rule: format!("failed unification {} (x) {}", self.print_type(&ks,&lt), self.print_type(&ks,&rt)),
               span: span.clone(),
            })
         },
         _ => { Ok(nt) }
      }
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
            if let Type::Named(nn,_nns) = n {
            if let Some(ti) = self.typedef_index.get(nn) {
            if let TypeRule::Typedef(tr) = &self.rules[*ti] {
            let it = tr.implies.clone().unwrap_or(Type::Any);
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
            if let TypeRule::Forall(ForallRule { inference:Inference::Imply(lt,rt), .. }) = &self.rules[*ti] {
               let mut subs = Vec::new();
               let nt = Type::nored_implies(self, &mut subs, &lt, &n);
               if !nt.is_bottom() {
               if let Ok(subs) = Type::compile_subs(&subs) {
                  let srt = rt.substitute(&subs);
                  if self.is_normal(&srt) {
                     let (inum, iden) = srt.project_ratio();
                     num_collector.append(&mut inum.clone());
                     den_collector.append(&mut iden.clone());
                     found = true;
                     continue;
                  }
               }}
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
            if let Type::Named(dn,_dns) = d {
            if let Some(ti) = self.typedef_index.get(dn) {
            if let TypeRule::Typedef(tr) = &self.rules[*ti] {
            let it = tr.implies.clone().unwrap_or(Type::Any);
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
            if let TypeRule::Forall(ForallRule { inference:Inference::Imply(lt,rt), .. }) = &self.rules[*ti] {
               let mut subs = Vec::new();
               let nt = Type::nored_implies(self, &mut subs, &lt, &d);
               if !nt.is_bottom() {
               if let Ok(subs) = Type::compile_subs(&subs) {
                  let srt = rt.substitute(&subs);
                  if self.is_normal(&srt) {
                     let (inum, iden) = srt.project_ratio();
                     num_collector.append(&mut iden.clone());
                     den_collector.append(&mut inum.clone());
                     found = true;
                     continue;
                  }
               }}
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

         self.implies(&l_only, &b_only, span)?;
         l_only = into.clone();
      }

      Ok(l_only)
   }

   pub fn unify_varnames_t(&mut self, dept: &mut HashMap<String,TermId>, tt: &Type, lhs: bool) {
      match tt {
         Type::Any => {},
         Type::Named(_tn,tcs) => {
            for tc in tcs.iter() {
               self.unify_varnames_t(dept,tc,lhs);
            }
         },
         Type::And(tcs) => {
            for tc in tcs.iter() {
               self.unify_varnames_t(dept,tc,lhs);
            }
         },
         Type::Tuple(tcs) => {
            for tc in tcs.iter() {
               self.unify_varnames_t(dept,tc,lhs);
            }
         },
         Type::Product(tcs) => {
            for tc in tcs.iter() {
               self.unify_varnames_t(dept,tc,lhs);
            }
         },
         Type::Arrow(p,b) => {
            self.unify_varnames_t(dept,p,true);
            self.unify_varnames_t(dept,b,lhs);
         },
         Type::Ratio(p,b) => {
            self.unify_varnames_t(dept,p,lhs);
            self.unify_varnames_t(dept,b,lhs);
         },
         Type::Constant(ct,_cv) => {
            let mut ct = *ct;
            self.unify_varnames_lhs(dept, &mut ct, lhs);
         }
      }
   }
   pub fn unify_varnames(&mut self, dept: &mut HashMap<String,TermId>, t: &mut TermId) {
      self.unify_varnames_lhs( dept, t, false );
   }
   pub fn unify_varnames_lhs(&mut self, dept: &mut HashMap<String,TermId>, t: &mut TermId, lhs: bool ) {
      match self.rows[t.id].term.clone() {
         Term::Ident(tn) => {
            if ["self","if","not","pos","neg","+","-","*","/","%","^","==","!=","<","<=",">",">=","&&","||"].contains(&tn.as_str()) {
                            //don't clobber reserved words
            } else if let Some(dt) = dept.get(&tn) {
                            //do clobber captured variables
               if let Term::Ident(dn) = self.rows[dt.id].term.clone() {
                  self.rows[t.id].term = Term::Ident(dn);
               }
            } else if lhs { //do capture arrow parameters
               let nn = format!("var#{}", t.id);
               dept.insert(tn.clone(), *t);
               self.rows[t.id].term = Term::Ident(nn);
            } else {}       //don't clobber free variables
         },
         Term::Value(_) => {},
         Term::Block(_sid,ref mut es) => {
            for e in es.iter_mut() {
               self.unify_varnames_lhs(dept,e,lhs);
            }
         },
         Term::Tuple(ref mut es) => {
            for e in es.iter_mut() {
               self.unify_varnames_lhs(dept,e,lhs);
            }
         },
         Term::Let(ref mut _lt) => {
            panic!("TODO: unify_varnames in Let term")
         },
         Term::Arrow(ref mut p,ref mut b) => {
            self.unify_varnames_lhs(dept,p,true);
            self.unify_varnames_lhs(dept,b,lhs);
         },
         Term::App(ref mut g,ref mut x) => {
            self.unify_varnames_lhs(dept,g,lhs);
            self.unify_varnames_lhs(dept,x,lhs);
         },
         Term::Ascript(ref mut t,_tt) => {
            self.unify_varnames_lhs(dept,t,lhs);
         },
         Term::As(ref mut t,_tt) => {
            self.unify_varnames_lhs(dept,t,lhs);
         },
         Term::Constructor(_c,ref mut kts) => {
            for (_k,t) in kts.iter_mut() {
               self.unify_varnames_lhs(dept,t,lhs);
            }
         },
         Term::Substitution(ref mut e,ref mut a,ref mut b) => {
            self.unify_varnames_lhs(dept,e,lhs);
            self.unify_varnames_lhs(dept,a,lhs);
            self.unify_varnames_lhs(dept,b,lhs);
         },
         Term::RuleApplication(_,_) => {},
         Term::Literal(ref mut t) => {
            self.unify_varnames_lhs(dept,t,lhs);
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
         Type::Named(_,_) => tt.clone(),
         Type::Arrow(_,_) => tt.clone(), //the inner types here are guarded
         Type::Constant(ct,cv) => Type::Constant(self.alpha_convert_term(*ct,lt,rt),cv.clone()),
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
   pub fn is_exhaustive(&mut self, t: TermId) -> Option<(TermId,String,TermId,TermId)> {
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
            if let Term::Ident(lower_i) = &self.rows[gt1s[1].id].term {
            if gt1s.len()==2 {
               let lower_bounds = gt1s[0];
               if let Term::App(gt,gt2) = &self.rows[app2s[1].id].term {
               if let Term::Ident(gtop) = &self.rows[gt.id].term {
               if gtop==">" {
               if let Term::Tuple(gt2s) = &self.rows[gt2.id].term {
               if gt2s.len()==2 {
                  let upper_i = gt2s[0];
                  let upper_bounds = gt2s[1];
                  if let Term::Ident(i2) = &self.rows[upper_i.id].term {
                  if lower_i==i2 {
                     return Some((lower_bounds,lower_i.clone(),upper_bounds,prop));
                  }}
               }}}}}
            }}}}}}
         }}}}}
      }}}}}
      None
   }
   pub fn check_invariants(&mut self, t: TermId) -> Result<(),Error> {
      let mut ground_types = Vec::new();
      let mut subs: HashMap<String,Constant> = HashMap::new();
      let self_term = Term::Ident("self".to_string());
      let self_termid = self.push_term(self_term.clone(), &self.rows[t.id].span.clone());
      self.untyped(self_termid);
      match self.rows[t.id].typ.clone() {
         Type::Named(tn,ts) => {
            ground_types.push(Type::Named(tn.clone(),ts.clone()));
         },
         Type::And(tcs) => {
            for tc in tcs.iter() {
            match tc {
               Type::Named(tn,ts) => {
                  ground_types.push(Type::Named(tn.clone(),ts.clone()));
               }, Type::Constant(ref ct, ref cv) => {
                  if let Some(ct) = cv {
                     subs.insert("self".to_string(), ct.clone());
                  } else if let Some(ct) = self.untyped_eval(&mut subs, &mut ct.clone(), true) {
                     subs.insert("self".to_string(), ct);
                  }
               }, _ => {},
            }}
         },
         _ => {},
      }
      for g in ground_types.iter() {
      if let Type::Named(tn,_ts) = g {
      if let Some(ti) = self.typedef_index.get(tn) {
      if let TypeRule::Typedef(tr) = &self.rules[*ti] {
         for invariant in tr.invariants.clone().iter() {
            let p = self.untyped_eval(&mut subs, &mut invariant.prop.clone(), true);
            let a = self.untyped_eval(&mut subs, &mut invariant.algs.clone(), true);
            if p.is_some() && p==a {
               //pass
            } else if let Some((mut low,i,mut high,prop)) = self.is_exhaustive(invariant.prop) {
               if let Some(Constant::Integer(low)) = self.untyped_eval(&mut subs, &mut low, true) {
               if let Some(Constant::Integer(high)) = self.untyped_eval(&mut subs, &mut high, true) {
                  for ival in low..=high {
                     let mut prop_mut = prop;

                     let ic = Constant::Integer(ival);
                     subs.insert(i.clone(), ic);

                     if let Some(Constant::Boolean(true)) = self.untyped_eval(&mut subs, &mut prop_mut, true) {
                        //pass
                     } else {
                        let st = subs.get("self").unwrap_or(&Constant::NaN);
                        return Err(Error {
                           kind: "Type Error".to_string(),
                           rule: format!("invariant not satisfied for self={:?}, {}={}: {}",
                                 st, i, ival, self.print_term(prop)),
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

   fn constant_as_term(&mut self, v: &Constant) -> Option<TermId> {
      match v {
         Constant::Integer(vi) => {
            Some( self.push_term(Term::Value( format!("{}",vi) ), &self.rows[0].span.clone()) )
         },
         Constant::Boolean(vi) => {
            Some(if *vi {
               self.push_term(Term::Value( format!("True") ), &self.rows[0].span.clone())
            } else {
               self.push_term(Term::Value( format!("True") ), &self.rows[0].span.clone())
            })
         },
         _vc => {
            None
         }
      }
   }

   fn destructure_ctuple(&mut self, lt: &Type, rt: &Type) -> Option<(Vec<TermId>,Vec<TermId>)> {
      match (lt,rt) {
         (Type::Tuple(lts), Type::Tuple(rts)) if lts.len()==rts.len() => {
            let mut ls = Vec::new();
            for lc in lts.iter() {
            if let Type::Constant(lid,_) = lc {
               ls.push(*lid);
            }}
            let mut rs = Vec::new();
            for rc in rts.iter() {
               if let Type::Constant(_rid,Some(riv)) = rc {
               if let Some(vt) = self.constant_as_term(riv) {
                  rs.push(vt);
                  continue;
               }}
               if let Type::Constant(rid,_) = rc {
                  rs.push(*rid);
               }
            }
            return Some((ls,rs));
         },
         _ => {}
      }
      None
   }

   pub fn push_constant(&mut self, t: TermId, c: &Constant) -> Constant {
      self.rows[t.id].constant = Some(c.clone());
      c.clone()
   }
   pub fn push_dtype(&mut self, t: TermId, c: &Option<Constant>) {
      if let Some(cv) = c {
         self.rows[t.id].constant = Some(cv.clone());
      }
   }
   pub fn unify_dvars(&mut self, dvars: &mut HashMap<String,Constant>, lhs: &Type, rhs: &Type) {
      match (lhs, rhs) {
         (Type::Named(ln,lcs), Type::Named(rn,rcs)) if ln==rn && lcs.len()==rcs.len() => {
            for (lc,rc) in std::iter::zip(lcs,rcs) {
               self.unify_dvars(dvars, lc, rc);
            }
         },
         (Type::Arrow(lp,lb), Type::Arrow(rp,rb)) => {
            self.unify_dvars(dvars, lp, rp);
            self.unify_dvars(dvars, lb, rb);
         },
         (Type::Ratio(lp,lb), Type::Ratio(rp,rb)) => {
            self.unify_dvars(dvars, lp, rp);
            self.unify_dvars(dvars, lb, rb);
         },
         (Type::Tuple(lcs),Type::Tuple(rcs)) if lcs.len()==rcs.len() => {
            for (lc,rc) in std::iter::zip(lcs,rcs) {
               self.unify_dvars(dvars, lc, rc);
            }
         },
         (Type::Product(lcs),Type::Product(rcs)) if lcs.len()==rcs.len() => {
            for (lc,rc) in std::iter::zip(lcs,rcs) {
               self.unify_dvars(dvars, lc, rc);
            }
         },
         (Type::Constant(lt,lc),Type::Constant(_rt,rc)) => {
            match (lc,rc) {
               (None,Some(rv)) => {
                  if let Term::Ident(ln) = &self.rows[lt.id].term.clone() {
                     dvars.insert(ln.clone(), rv.clone());
                  }
               },
               _ => {}
            }
         },
         _tt => {},
      }
   }
   pub fn apply_dvars(&mut self, dvars: &HashMap<String,Constant>, g: &mut Type) {
      match g {
         Type::Named(_gn,gcs) => {
            for gc in gcs.iter_mut() {
               self.apply_dvars(dvars, gc);
            }
         },
         Type::Arrow(gp,gb) => {
            self.apply_dvars(dvars, gp);
            self.apply_dvars(dvars, gb);
         },
         /* TODO unquote
         (Type::Ratio(lp,lb), Type::Ratio(rp,rb)) => {
            self.unify_dvars(dvars, lp, rp);
            self.unify_dvars(dvars, lb, rb);
         },
         (Type::Tuple(lcs),Type::Tuple(rcs)) if lcs.len()==rcs.len() => {
            for (lc,rc) in std::iter::zip(lcs,rcs) {
               self.unify_dvars(dvars, lc, rc);
            }
         },
         (Type::Product(lcs),Type::Product(rcs)) if lcs.len()==rcs.len() => {
            for (lc,rc) in std::iter::zip(lcs,rcs) {
               self.unify_dvars(dvars, lc, rc);
            }
         },
         */
         Type::Constant(gt,gc) => {
            if let Term::Ident(gn) = &self.rows[gt.id].term.clone() {
            if let Some(gv) = dvars.get(gn) {
               *gc = Some(gv.clone());
            }}
         },
         _tt => {},
      }
   }
   pub fn typeck_hint(&mut self, bound: &mut HashMap<String,TermId>, scope: &Option<ScopeId>, hint: &String, lhs: TermId, rhs: TermId) -> Result<(),Error> {
      match ( self.rows[lhs.id].term.clone(), self.rows[rhs.id].term.clone() ) {
         (Term::Ident(ln), Term::Ident(rn)) if ln==rn => {
            Ok(()) //This is unsound, just a workaround for now
         },
         (_lhsx, Term::Ident(x)) => {
            let realized = self.rows[lhs.id].typ.clone();
            let required = self.typeof_var(scope, &x, &Some(realized.clone()), &self.rows[lhs.id].span.clone())?;
            self.implies(&realized, &required, &self.rows[lhs.id].span.clone())?;
            if let Some(prevx) = bound.get(&x) {
               if !Term::equals(self, lhs, *prevx) {
                  return Err(Error {
                     kind: "Type Error".to_string(),
                     rule: format!("hint parameter does not structurally match in term: {}", hint),
                     span: self.rows[lhs.id].span.clone(),
                  })
               }
            } else {
               bound.insert(x.clone(), lhs);
            }
            Ok(())
         },
         (Term::Block(_lsid,_les),Term::Block(_rsid,_res)) => {
            unimplemented!("TODO: typeck_hint Term::Block")
         },
         (Term::Tuple(les),Term::Tuple(res)) if les.len()==res.len() => {
            for (lx,rx) in std::iter::zip(les,res) {
               self.typeck_hint(bound, scope, hint, lx, rx)?;
            }
            Ok(())
         },
         (Term::Let(_llt),Term::Let(_rlt)) => {
            unimplemented!("TODO: typeck_hint Term::Let")
         },
	 (Term::Ascript(_lx,_ltt),Term::Ascript(_rx,_rtt)) => {
            unimplemented!("TODO: typeck_hint Term::Ascript")
         },
	 (Term::As(_lx,_ltt),Term::As(_rx,_rtt)) => {
            unimplemented!("TODO: typeck_hint Term::As")
         },
	 (Term::RuleApplication(_lx,_ltt),Term::RuleApplication(_rx,_rtt)) => {
            unimplemented!("TODO: typeck_hint Term::RuleApplication")
         },
	 (Term::Arrow(_llhs,_lrhs),Term::Arrow(_rlhs,_rrhs)) => {
            unimplemented!("TODO: typeck_hint Term::Arrow")
         },
	 (Term::App(lg,lx),Term::App(rg,rx)) => {
            self.typeck_hint(bound, scope, hint, lg, rg)?;
            self.typeck_hint(bound, scope, hint, lx, rx)?;
            Ok(())
         },
         (Term::Constructor(_ln,_lkvs),Term::Constructor(_rn,_rkvs)) => {
            unimplemented!("TODO: typeck_hint Term::Constructor")
         },
         (Term::Substitution(_le,_la,_lb),Term::Substitution(_re,_ra,_rb)) => {
            unimplemented!("TODO: typeck_hint Term::Substitution")
         },
	 (Term::Value(lx),Term::Value(rx)) if lx == rx => { Ok(()) },
         (_,_) => {
            return Err(Error {
               kind: "Type Error".to_string(),
               rule: format!("hint does not structurally match term: {}", hint),
               span: self.rows[lhs.id].span.clone(),
            })
         }
      }
   }
   pub fn typeck(&mut self, scope: &Option<ScopeId>, t: TermId, implied: Option<Type>) -> Result<(),Error> {
      let implied = implied.map(|tt|tt.normalize());
      //clone is needed to avoid double mutable borrows?
      match self.rows[t.id].term.clone() {
         Term::Literal(l) => {
            self.untyped(l);
            if let Some(ref i) = implied {
               //TODO: typeck dynamic expression body vs literal pattern definitions
               self.rows[t.id].typ = i.clone();
            } else {
               return Err(Error {
                  kind: "Type Error".to_string(),
                  rule: format!("Literal Expressions must have an Implied Type"),
                  span: self.rows[t.id].span.clone(),
               })
            }
         },
         Term::Block(sid,es) => {
            let mut last_typ = self.nil_type.clone();
            for e in es.iter() {
               self.typeck(&Some(sid), *e, None)?;
               last_typ = self.rows[e.id].typ.clone();
            }
            if last_typ.is_bottom() {
               self.rows[t.id].typ = self.nil_type.clone();
            } else {
               self.rows[t.id].typ = last_typ;
            }
         },
         Term::Tuple(es) => {
            let mut ts = Vec::new();
            for e in es.iter() {
               self.typeck(scope, *e, None)?;
               ts.push(self.rows[e.id].typ.clone());
            }
            self.rows[t.id].typ = self.implies(&Type::Tuple(ts), &self.rows[t.id].typ.clone(), &self.rows[t.id].span.clone())?;
         },
         Term::Let(lt) => {
            if lt.name=="" {
               //term is untyped
               self.untyped(t);
            } else if let Some(ref b) = lt.body {
               self.typeck(&Some(lt.scope), *b, Some(lt.rtype.clone()))?;
               self.rows[t.id].typ = self.nil_type.clone();
            } else {
               self.rows[t.id].typ = self.nil_type.clone();
            }
            self.soundck(&lt.rtype, &self.rows[t.id].span.clone())?;
         },
         Term::Ascript(x,tt) => {
            self.typeck(scope, x, Some(tt.clone()))?;
            self.rows[t.id].typ = self.implies(&self.rows[x.id].typ.clone(), &tt, &self.rows[t.id].span.clone())?;
         },
         Term::As(x,into) => {
            self.typeck(scope, x, None)?;
            let into_kind = self.kind(&into).first();
            if let Ok(nt) = self.implies(&self.rows[x.id].typ.clone(), &into, &self.rows[t.id].span.clone()) {
               //if cast is already satisfied, do nothing
               self.rows[t.id].typ = self.implies(&nt, &self.rows[t.id].typ.clone(), &self.rows[t.id].span.clone())?;
            } else {
               if self.is_knormal(&into_kind) {
                  let l_only = self.project_kinded(&into_kind, &self.rows[x.id].typ.clone());
                  let l_alts = self.remove_kinded(&into_kind, &self.rows[x.id].typ.clone());
                  let l_only = self.cast_into_kind(l_only, &into, &self.rows[t.id].span.clone())?;

                  //quod erat demonstrandum
                  self.rows[t.id].typ = self.implies(&l_only,&into,&self.rows[t.id].span.clone())?.and(&l_alts);
               } else {
                  let mut accept = false;
                  for tr in self.rules.clone().iter() { match tr {
                     TypeRule::Forall(ForallRule { inference:Inference::Imply(lt,rt), kind:k, .. }) if k==&into_kind => {
                        if let Ok(lt) = self.implies(&self.rows[x.id].typ.clone(), lt, &self.rows[t.id].span.clone()) {
                        if let Ok(rt) = self.implies(&rt, &into, &self.rows[t.id].span.clone()) {
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
               if let Ok(nt) = self.implies(&ki,&pat,&self.rows[t.id].span.clone()) { //Term kinded is not []
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
         Term::RuleApplication(lhs,h) => {
            if h == "reduce" {
               self.typeck(scope, lhs, None)?;
               let vt = Term::reduce(self, scope, &HashMap::new(), lhs);
               if vt.is_some() {
                  self.rows[t.id].typ = self.rows[lhs.id].typ.and( &Type::Constant(lhs, vt) );
               } else { return Err(Error {
                  kind: "Type Error".to_string(),
                  rule: format!("failed to reduce expression: {}", self.print_term(lhs)),
                  span: self.rows[t.id].span.clone(),
               }) }
            } else if let Some(fa) = self.hints.get(&h) {
               let fa_scope = fa.scope.clone();
               let fa_inference = fa.inference.clone();
               if let Some(rhs) = fa.rhs {
                  self.typeck(scope, lhs, None)?;
                  self.typeck_hint(&mut HashMap::new(), &Some(fa_scope), &h, lhs, rhs)?;
                  //at this point rule must have matched, so apply it
                  if let Inference::Type(fat) = fa_inference {
                     self.rows[t.id].typ = self.rows[lhs.id].typ.and( &fat );
                  }
               } else { return Err(Error {
                  kind: "Type Error".to_string(),
                  rule: format!("hint rule must have a rhs: {}", h),
                  span: self.rows[t.id].span.clone(),
               }) }
            } else { return Err(Error {
               kind: "Type Error".to_string(),
               rule: format!("hint not found in statements: {}", h),
               span: self.rows[t.id].span.clone(),
            }) }
         },
         Term::Arrow(_p,_b) => {
            unimplemented!("TODO typecheck Term::Arrow")
         },
         Term::App(g,x) => {
            let mut ks = HashMap::new();
            self.typeck(scope, x, None)?;
            self.typeck(scope, g, Some(
               Type::Arrow(Box::new(self.rows[x.id].typ.clone()),
                          Box::new(Type::Any))
            ))?;
            self.kinds_of(&mut ks, &self.rows[g.id].typ);
            self.kinds_of(&mut ks, &self.rows[x.id].typ);
            let mut gs = Vec::new();
            let mut xs = Vec::new();
            let mut tcs: Vec<Type> = Vec::new();
            for kn in TLC::kflat(&ks).iter() {
               let nt = self.narrow(&ks, kn, &self.rows[g.id].typ);
               if nt.is_bottom() { continue; }
               let mut nts = match nt {
                  Type::And(cts) => { cts.clone() },
                  ct => { vec![ ct.clone() ] },
               };
               let xt = self.narrow(&ks, kn, &self.rows[x.id].typ);
               if xt.is_bottom() { continue; }
               let mut tt = self.narrow(&ks, kn, &self.rows[t.id].typ);
               if tt.is_bottom() { tt = Type::Any; }
               for mut nt in nts.iter_mut() {
               match (&mut nt, &xt) {
                  (Type::Arrow(cp,cb), Type::Constant(ref xc,ref xcv)) => {
                  if let (Type::Constant(ref mut cp, ref cpv),Type::Constant(ref mut cb, ref cbv)) = ((**cp).clone(),(**cb).clone()) {
                     self.push_dtype(*xc,xcv);
                     self.push_dtype(*cp,cpv);
                     self.push_dtype(*cb,cbv);
                     gs.push(nt.clone());
                     xs.push(xt.clone());
                     let gct = self.push_term(Term::Arrow(*cp,*cb), &self.rows[t.id].span.clone());
                     self.untyped(gct);
                     let gxct = self.push_term(Term::App(gct,*xc), &self.rows[t.id].span.clone());
                     self.untyped(gxct);
                     tcs.push(Type::Constant(gxct,None));
                  }},
                  (Type::Arrow(cp,cb), xc) if (**cp).is_ctuple() && xc.is_ctuple() => {
                     if let Some((cps,xcs)) = self.destructure_ctuple(cp,xc) {
                     if let Type::Constant(ref mut cb, ref cbv) = (**cb).clone() {
                        self.push_dtype(*cb,cbv);
                        gs.push(nt.clone());
                        xs.push(xt.clone());
                        let cpst = self.push_term(Term::Tuple(cps), &self.rows[t.id].span.clone());
                        self.untyped(cpst);
                        let xcst = self.push_term(Term::Tuple(xcs), &self.rows[t.id].span.clone());
                        self.untyped(xcst);
                        let gct = self.push_term(Term::Arrow(cpst,*cb), &self.rows[t.id].span.clone());
                        self.untyped(gct);
                        let gxct = self.push_term(Term::App(gct,xcst), &self.rows[t.id].span.clone());
                        self.untyped(gxct);
                        tcs.push(Type::Constant(gxct,None));
                     }} else {
                        panic!("malformed ctuple {:?} (x) {:?}", **cp, xc);
                     }
                  },
                  (gt, xt) => {
                     let mut dvars = HashMap::new();
                     self.unify_dvars(&mut dvars, &gt.domain(), &xt);
                     self.apply_dvars(&dvars, gt);
                     gs.push(gt.clone());
                     xs.push(xt.clone());
                     tcs.push(gt.range());
                     self.arrow_implies(&xt, &gt.domain(), &self.rows[t.id].span.clone(), InArrow::Lhs)?;
                     tcs.push(self.arrow_implies(&gt.range(), &tt, &self.rows[t.id].span.clone(), InArrow::Rhs)?);
                  }
               }}
            }
            self.rows[g.id].typ = if gs.len()==1 { gs[0].clone() }
            else { Type::And(gs) };
            self.rows[x.id].typ = if xs.len()==1 { xs[0].clone() }
            else { Type::And(xs) };
            self.rows[t.id].typ = self.rows[t.id].typ.and(&Type::And( tcs ));
         },
         Term::Constructor(cname,kvs) => {
            for (_k,v) in kvs.clone().into_iter() {
               self.typeck(scope, v, None)?;
            }
            if let Some((ref tt,_tpars,_tkvs)) = self.constructors.get(&cname) {
               self.rows[t.id].typ = tt.clone();
            } else { return Err(Error {
               kind: "Type Error".to_string(),
               rule: format!("type constructor, none found for: {}", self.print_term(t)),
               span: self.rows[t.id].span.clone(),
            }) }
         },
         Term::Substitution(e,a,b) => {
            self.typeck(scope, e, None)?;
            let mut et = self.rows[e.id].typ.clone();
            self.reduce_type(&mut HashMap::new(), &mut et);
            let mut et = self.alpha_convert_type(&et, a, b);
            self.reduce_type(&mut HashMap::new(), &mut et);
            self.rows[e.id].typ = et.clone();
            self.rows[t.id].typ = et.clone();
         },
      };
      if let Some(implied) = implied {
         //check that implication is satisfied, but this unification does not change the term's type
         self.implies(&self.rows[t.id].typ.clone(), &implied, &self.rows[t.id].span.clone())?;
      }
      self.soundck(&self.rows[t.id].typ.clone(), &self.rows[t.id].span.clone())?;
      Ok(())
   }
   pub fn check(&mut self, globals: Option<ScopeId>, src:&str) -> Result<(),Error> {
      let rows_l = self.rows.len();
      let rules_l = self.rules.len();
      let scopes_l = self.scopes.len();
      let value_regexes_l = self.value_regexes.len();
      let regexes_l = self.regexes.len();
      let globals_l = if let Some(g) = globals {
         if self.scopes.len()>0 {
            self.scopes[g.id].children.len()
         } else {
            0
         }
      } else { 0 };
      let type_is_normal_l = self.type_is_normal.clone();
      let kind_is_normal_l = self.kind_is_normal.clone();
      let typedef_index_l = self.typedef_index.clone();
      let foralls_index_l = self.foralls_index.clone();
      let foralls_rev_index_l = self.foralls_rev_index.clone();

      let r = self.compile_str(globals, src);

      self.rows.truncate(rows_l);
      self.rules.truncate(rules_l);
      self.scopes.truncate(scopes_l);
      self.value_regexes.truncate(value_regexes_l);
      self.regexes.truncate(regexes_l);
      if let Some(g) = globals {
         if self.scopes.len()>0 {
            self.scopes[g.id].children.truncate(globals_l);
         }
      };
      self.type_is_normal = type_is_normal_l;
      self.kind_is_normal = kind_is_normal_l;
      self.typedef_index = typedef_index_l;
      self.foralls_index = foralls_index_l;
      self.foralls_rev_index = foralls_rev_index_l;

      r?; Ok(())
   }
}
