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
   pub hints: HashMap<String,Vec<ForallRule>>,
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
   pub scope: ScopeId,
   pub itks: Vec<(Option<String>,Option<Type>,Kind)>,
   pub prop: TermId,
   pub algs: Constant,
}

#[derive(Clone)]
pub struct TypedefRule {
   pub name: String,
   pub is_normal: bool,
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
         Term::Literal(lps) => format!("literal {}",
            lps.iter().map(|lp| format!("{:?}",lp)).collect::<Vec<String>>().join(" ")
         ),
         Term::Project(v) => format!("π{:?}", v),
         Term::Fail => format!("fail"),
         Term::Ident(x) => format!("{}", x),
         Term::Value(x) => format!("{}", x),
         Term::Arrow(_sc,p,rt,b) => format!("(fn({}){} = {})",
            self.print_term(*p),
            if let Some(rt) = rt { format!(":{:?}", rt) } else { format!("") },
            self.print_term(*b)),
         Term::App(g,x) => format!("{}({})", self.print_term(*g), self.print_term(*x)),
         Term::Let(lt) => format!("let {}", lt.name),
         Term::Ascript(t,tt) => format!("{}:{:?}", self.print_term(*t), tt),
         Term::As(t,tt) => format!("{} as {:?}", self.print_term(*t), tt),
         Term::Match(dv,lrs) => {
            let mut s = "".to_string();
            for (i,(l,r)) in lrs.iter().enumerate() {
               if i>0 { s += ", "; };
               s += &format!("{} => {}", self.print_term(*l), self.print_term(*r));
            }
            format!("match {} {{ {} }}", self.print_term(*dv), s)
         },
         Term::Tuple(es) => {
            format!("({})", es.iter().filter(|e|e.id!=0).map(|e| self.print_term(*e)).collect::<Vec<String>>().join(","))
         },
         Term::Block(_,es) => {
            format!("{{{}}}", es.iter().filter(|e|e.id!=0).map(|e| self.print_term(*e)).collect::<Vec<String>>().join(";"))
         },
         Term::Constructor(cn,kvs) => {
            format!("{}{{{}}}", cn, kvs.iter().map(|(k,v)|format!("{}={}",k,self.print_term(*v))).collect::<Vec<String>>().join(","))
         },
         Term::RuleApplication(t,n) => format!("{} @{}", self.print_term(*t), n),
      }
   }
   pub fn fails(&self, t: TermId) -> bool {
      match &self.rows[t.id].term {
         Term::Fail => true,
         _ => false,
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
      if let Some(ref h) = name {
         if self.hints.get(h).is_none() {
            self.hints.insert(h.clone(), Vec::new());
         };
         self.hints.get_mut(h).unwrap().push(fa.clone());
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

   pub fn parse_toks(&mut self, globals: Option<ScopeId>, tks:&mut TokenReader) -> Result<TermId,Error> {
      let file_scope = globals.unwrap_or(self.push_scope(Scope {
         parent: None, children: Vec::new(),
      }, &span_of(tks)));
      Ok(ll1_file(self, file_scope, tks)?)
   }
   pub fn check_toks(&mut self, globals: Option<ScopeId>, tks:&mut TokenReader) -> Result<TermId,Error> {
      let ast = self.parse_toks(globals, tks)?;
      self.compile_rules()?;
      self.typeck(&globals, ast, None)?;
      self.sanityck()?;
      Ok(ast)
   }
   pub fn import_toks(&mut self, globals: Option<ScopeId>, tks:&mut TokenReader) -> Result<ScopeId,Error> {
      self.check_toks(globals, tks)?;
      Ok(ScopeId {id:0})
   }
   pub fn reduce_toks(&mut self, globals: Option<ScopeId>, tks:&mut TokenReader) -> Result<Constant,Error> {
      let _ast = self.check_toks(globals, tks)?;
      //TODO: call Term::reduce
      Ok(Constant::Literal("0".to_string()))
   }

   pub fn parse_file(&mut self, globals: Option<ScopeId>, filename:&str) -> Result<TermId,Error> {
      let mut tks = tokenize_file(self, filename)?;
      self.parse_toks(globals, &mut tks)
   }
   pub fn check_file(&mut self, globals: Option<ScopeId>, filename:&str) -> Result<TermId,Error> {
      let mut tks = tokenize_file(self, filename)?;
      self.check_toks(globals, &mut tks)
   }
   pub fn import_file(&mut self, globals: Option<ScopeId>, filename:&str) -> Result<ScopeId,Error> {
      let mut tks = tokenize_file(self, filename)?;
      self.import_toks(globals, &mut tks)
   }
   pub fn reduce_file(&mut self, globals: Option<ScopeId>, filename:&str) -> Result<Constant,Error> {
      let mut tks = tokenize_file(self, filename)?;
      self.reduce_toks(globals, &mut tks)
   }

   pub fn parse_str(&mut self, globals: Option<ScopeId>, src:&str) -> Result<TermId,Error> {
      let mut tks = tokenize_string(self, "[string]", src)?;
      self.parse_toks(globals, &mut tks)
   }
   pub fn check_str(&mut self, globals: Option<ScopeId>, src:&str) -> Result<TermId,Error> {
      let mut tks = tokenize_string(self, "[string]", src)?;
      self.check_toks(globals, &mut tks)
   }
   pub fn import_str(&mut self, globals: Option<ScopeId>, src:&str) -> Result<ScopeId,Error> {
      let mut tks = tokenize_string(self, "[string]", src)?;
      self.import_toks(globals, &mut tks)
   }
   pub fn reduce_str(&mut self, globals: Option<ScopeId>, src:&str) -> Result<Constant,Error> {
      let mut tks = tokenize_string(self, "[string]", src)?;
      self.reduce_toks(globals, &mut tks)
   }


   pub fn kinds_of(&self, kinds: &mut HashMap<Type,Kind>, tt: &Type) -> Option<Kind> {
      match tt {
         Type::Any => {
            kinds.insert(Type::Any, Kind::Nil);
            Some(Kind::Nil)
         },
         Type::Constant(_) => {
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
   pub fn compile_rules(&mut self) -> Result<(),Error> {
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
   pub fn push_term(&mut self, term: Term, span: &Span) -> TermId {
      let index = self.rows.len();
      let ti = TermId { id: index };
      self.rows.push(Row {
         term: term,
         typ: Type::Any,
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
         Type::Constant(_) => Ok(()),
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
         Type::Constant(cv) => Type::Constant(cv.clone())
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
         Type::Constant(_) => true,
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
         Type::Constant(_cv) => {
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
            rule: format!("variable {}: {:?} did not match any candidate {}",
                     v,
                     &implied,
                     candidates.iter().map(|t|format!("{:?}",t))
                               .collect::<Vec<String>>().join(" | "),
                  ),
            span: span.clone(),
         }) } else {
            self.typeof_var(&sc.parent.clone(), v, implied, span)
         }
      } else { Err(Error {
         kind: "Type Error".to_string(),
         rule: format!("variable not found in scope: {} : {:?}", v, implied.clone().unwrap_or(Type::Any) ),
         span: span.clone(),
      }) }
   }
   pub fn untyped(&mut self, t: TermId) {
      self.rows[t.id].typ = self.bottom_type.clone();
      match self.rows[t.id].term.clone() {
         Term::Literal(_lp) => (),
         Term::Ident(_x) => (),
         Term::Value(_x) => (),
         Term::App(g,x) => { self.untyped(g); self.untyped(x); },
         Term::Arrow(_sc,p,_rt,b) => { self.untyped(p); self.untyped(b); },
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
      let mut lt = lt.clone();
      let mut rt = rt.clone();
      let nt = Type::arrow_implies(self, &mut lt, &mut rt, inarrow);
      match nt {
         Type::And(nts) if nts.len()==0 => {
            Err(Error {
               kind: "Type Error".to_string(),
               rule: format!("failed unification {:?} (x) {:?}", &lt, &rt),
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
   pub fn check_invariants(&mut self, _scope: &Option<ScopeId>, t: TermId) -> Result<(),Error> {
      let mut ground_types = Vec::new();
      let mut subs: HashMap<String,Constant> = HashMap::new();
      match self.rows[t.id].typ.clone() {
         Type::Named(tn,ts) => {
            ground_types.push(Type::Named(tn.clone(),ts.clone()));
         },
         Type::And(tcs) => {
            for tc in tcs.iter() {
            match tc {
               Type::Named(tn,ts) => {
                  ground_types.push(Type::Named(tn.clone(),ts.clone()));
               }, Type::Constant(ref cv) => {
                  subs.insert("self".to_string(), cv.clone());
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
            let p = Term::reduce(self, &Some(invariant.scope), &mut subs, invariant.prop)?;
            if p == invariant.algs {
               continue;
            }
            return Err(Error {
               kind: "Type Error".to_string(),
               rule: format!("invariant not satisfied {}: {} | {:?}", tn, self.print_term(invariant.prop), invariant.algs),
               span: self.rows[t.id].span.clone(),
            })
            /*
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
            */
         }
      }}}}
      Ok(())
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
	 (Term::Arrow(_lsc,_llhs,_lrt,_lrhs),Term::Arrow(_rsc,_rlhs,_rrt,_rrhs)) => {
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
      //TODO: remove clone here because it is bloating the memory footprint
      match self.rows[t.id].term.clone() {
         Term::Project(_v) => panic!("Projection Constants cannot be Values at {:?}", &self.rows[t.id].span),
         Term::Literal(_lp) => {
            self.rows[t.id].typ = implied.clone().unwrap_or(Type::Any);
         },
         Term::Fail => {
            self.rows[t.id].typ = implied.clone().unwrap_or(Type::Any);
         },
         Term::Match(dv, lrs) => {
            if lrs.len()==0 {
               return Err(Error {
                  kind: "Type Error".to_string(),
                  rule: format!("pattern cannot match because it has no branches"),
                  span: self.rows[t.id].span.clone(),
               })
            };
            self.typeck(scope, dv, None)?;
            let mut rts = Vec::new();
            for (l,r) in lrs.iter() {
               self.untyped(*l);
               self.typeck(scope, *r, implied.clone())?;
               rts.push( self.rows[r.id].typ.clone() );
            }
            let mut rt = rts[0].clone();
            for ri in 1..rts.len() {
               rt = rt.most_general_unifier(&rts[ri]);
            }
            self.rows[t.id].typ = rt;
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
                  //non-normal casts are hard casts
                  //in strict mode, hard casts much be proven as subsets
                  //in casual mode, hard casts are gradually typed in @reduce
                  self.rows[t.id].typ = into.clone();
               }
            }
            self.check_invariants(scope, t)?;
         },
         Term::Ident(x) => {
            self.rows[t.id].typ = self.typeof_var(&scope, &x, &implied, &self.rows[t.id].span.clone())?;
         },
         Term::Value(x) => {
            let i = if let Some(ref i) = implied { i.clone() } else { self.bottom_type.clone() };
            let ki = self.project_kinded(&self.term_kind, &i);
            let mut r = None;
            for (pat,re) in self.regexes.clone().into_iter() {
               if let Ok(nt) = self.implies(&ki,&pat,&self.rows[t.id].span.clone()) { //Term kinded is not []
                  r = Some(re.clone());
                  self.rows[t.id].typ = nt.and(&ki);
                  break;
               }
               else if ki==self.bottom_type && re.is_match(&x) { //Term kinded is []
                  r = Some(re.clone());
                  self.rows[t.id].typ = pat;
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
            self.rows[t.id].typ = self.rows[t.id].typ.and( &Type::Constant(Constant::parse(self,&x).unwrap()) );
            self.check_invariants(scope, t)?;
	 },
         Term::RuleApplication(lhs,h) => {
            //borrowing self even in a .clone'd expression fails the borrow checker
            if h == "reduce" {
               self.typeck(scope, lhs, None)?;
               let vt = Term::reduce(self, scope, &HashMap::new(), lhs)?;
               self.rows[t.id].typ = self.rows[lhs.id].typ.and( &Type::Constant(vt.clone()) );
            } else if let Some(fas) = self.hints.get(&h).cloned() {
               self.typeck(scope, lhs, None)?;
               let mut matched = false;
               self.rows[t.id].typ = self.rows[lhs.id].typ.clone();
               for fa in fas.iter() {
                  let fa_scope = fa.scope.clone();
                  let fa_inference = fa.inference.clone();
                  if let Some(rhs) = fa.rhs {
                  if let Ok(_) = self.typeck_hint(&mut HashMap::new(), &Some(fa_scope), &h, lhs, rhs) {
                     //at this point rule must have matched, so apply it
                     if let Inference::Type(fat) = fa_inference {
                        self.rows[t.id].typ = self.rows[t.id].typ.and( &fat );
                     }
                     matched = true;
                  }}
               };
               if !matched { return Err(Error {
                  kind: "Type Error".to_string(),
                  rule: format!("hint did not match any declared rule: {}", h),
                  span: self.rows[t.id].span.clone(),
               }) }
            } else { return Err(Error {
               kind: "Type Error".to_string(),
               rule: format!("hint not found in statements: {}", h),
               span: self.rows[t.id].span.clone(),
            }) }
         },
         Term::Arrow(ref sc,p,rt,b) => {
            self.typeck(sc, b, rt)?;
            self.rows[t.id].typ = Type::Arrow(
               Box::new(self.rows[p.id].typ.clone()),
               Box::new(self.rows[b.id].typ.clone()),
            );
         },
         Term::App(g,x) => {
            self.typeck(scope, x, None)?;
            if let Term::Project(Constant::Literal(cs)) = &self.rows[g.id].term {
               let pi = str::parse::<usize>(&cs).unwrap();
               if let Type::Tuple(gts) = self.rows[x.id].typ.clone() {
                  self.rows[g.id].typ = gts[pi].clone();
                  self.rows[t.id].typ = gts[pi].clone();
               }
            } else {
               let grt = match &self.rows[x.id].typ {
                  Type::Tuple(ts) if ts.len()==0 => { implied.clone().unwrap_or(Type::Any) },
                  _ => Type::Any,
               };
               self.typeck(scope, g, Some(
                  Type::Arrow(Box::new(self.rows[x.id].typ.clone()),
                             Box::new(grt.clone()))
               ))?;
               self.rows[t.id].typ = self.rows[g.id].typ.range();
            }
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

      let r = self.import_str(globals, src);

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
