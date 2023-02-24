use std::rc::Rc;
use std::collections::{HashSet,HashMap};
use regex::Regex;
use crate::term::{Term,TermId,LetTerm};
use crate::scope::{Scope,ScopeId};
use crate::typ::{Type,InArrow};
use crate::kind::Kind;
use crate::token::{Span,TokenReader,tokenize_string,tokenize_file};
use crate::constant::Constant;
use crate::debug::Error;
use crate::ll::ll1_file;

pub struct TLC {
   pub strict: bool,
   pub poly_bindings: HashMap<(String,Type),TermId>,
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
   pub untyped: bool,
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
   pub parameters: Vec<(String,Type,Kind)>,
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
   pub parameters: Vec<(String,Type,Kind)>,
   pub scope: ScopeId,
   pub inference: Type,
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
                    i.clone(),
                    k
              )).collect::<Vec<String>>().join(","),
              if tr.parameters.len()==0 { "" } else { ">" },
              if let Some(ref ti) = tr.implies { format!(":{:?}",ti) } else { format!("") },
              tr.kind
           ),
           TypeRule::Forall(fr) => write!(f, "forall {}. {:?} :: {:?}", 
              fr.parameters.iter().map(|(i,t,k)| format!("{:?}:{:?}::{:?}",
                    i.clone(),
                    t.clone(),
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
            untyped: true,
         }],
         poly_bindings: HashMap::new(),
         rules: Vec::new(),
         scopes: Vec::new(),
         value_regexes: Vec::new(),
         regexes: Vec::new(),
         hints: HashMap::new(),
         constructors: HashMap::new(),
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
   pub fn dump_scope(&self, s: ScopeId) {
      for (cn,_,ct,_) in self.scopes[s.id].children.iter() {
         println!("{}: {:?}", cn, ct);
      }
      if let Some(p) = self.scopes[s.id].parent {
         println!("-------------------");
         self.dump_scope(p);
      }
   }
   pub fn print_term(&self, t: TermId) -> String {
      match &self.rows[t.id].term {
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
         Term::Ascript(t,tt) => format!("({}:{:?})", self.print_term(*t), tt),
         Term::As(t,tt) => format!("({} as {:?})", self.print_term(*t), tt),
         Term::Match(dv,lrs) => {
            let mut s = "".to_string();
            for (i,(_clr,l,r)) in lrs.iter().enumerate() {
               if i>0 { s += ", "; };
               s += &format!("{} => {}", self.print_term(*l), self.print_term(*r));
            }
            format!("match {} {{ {} }}", self.print_term(*dv), s)
         },
         Term::Tuple(es) => {
            if es.len()==1 { format!("({},)",self.print_term(es[0])) }
            else { format!("({})", es.iter().filter(|e|e.id!=0).map(|e| self.print_term(*e)).collect::<Vec<String>>().join(",")) }
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
   pub fn push_forall(&mut self, globals: ScopeId, axiom: bool, name: Option<String>, quants: Vec<(String,Type,Kind)>,
                             inference: Type, term: Option<TermId>, kind: Kind, span: Span) {
      let mut fa_closed: Vec<(String,HashMap<Type,Kind>,Type,Option<TermId>)> = Vec::new();
      for (qn,qt,qk) in quants.iter() {
         let mut fk = HashMap::new();
         fk.insert(qt.clone(), qk.clone());
         fa_closed.push( (qn.clone(), fk, qt.clone(), None) );
      }
      let fa_scope = self.push_scope(Scope {
         parent: Some(globals),
         children: fa_closed,
      });
      if let Some(t) = term {
         self.untyped(t);
      }
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
      }));
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
      let ast = self.check_toks(globals, tks)?;
      Term::reduce(self, &globals, ast)
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
   pub fn compile_rules(&mut self) -> Result<(),Error> {
      for rule in self.rules.clone().iter() { match rule {
         TypeRule::Forall(fr) => { if self.strict && !fr.axiom {
            if let Some(ref rhs) = fr.rhs {
               self.typeck(&Some(fr.scope), *rhs, Some(fr.inference.clone()))?;
            }
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
   pub fn push_term(&mut self, term: Term, span: &Span) -> TermId {
      let index = self.rows.len();
      let ti = TermId { id: index };
      self.rows.push(Row {
         term: term,
         typ: Type::Any,
         kind: self.term_kind.clone(),
         span: span.clone(),
         untyped: false,
      });
      ti
   }
   pub fn push_scope(&mut self, scope: Scope) -> ScopeId {
      let index = self.scopes.len();
      self.scopes.push(scope);
      ScopeId { id: index }
   }
   pub fn new_scope(&mut self, parent: Option<ScopeId>) -> ScopeId {
      self.push_scope(Scope {
         parent: parent,
         children: Vec::new(),
      })
   }
   pub fn into_ident(&self, n: String) -> String {
      if n.starts_with("$") { n[2..n.len()-1].to_string() }
      else { n }
   }
   pub fn sanityck(&mut self) -> Result<(),Error> {
      for (ri,r) in self.rows.iter().enumerate() {
         if ri==0 { continue; } //first row is nullary and not sane
         if !r.untyped && !r.typ.is_concrete() {
            return Err(Error {
               kind: "Type Error".to_string(),
               rule: format!("inhabited type is not concrete: t#{} {:?} = typeof({})", ri, r.typ, self.print_term(TermId{id:ri})),
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
         Type::MaybeZero(_tt) => Ok(()),
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
         Type::HTuple(bt,_ct) => { self.soundck(bt,span) },
         Type::Product(ts) => { for tc in ts.iter() { self.soundck(tc,span)?; } Ok(()) },
         Type::Named(tn,ts) => {
            if ts.len()==0 { return Ok(()); }
            if !tt.is_concrete() { return Ok(()); }
            if let Some(ti) = self.typedef_index.get(tn) {
            if let TypeRule::Typedef(tr) = &self.rules[*ti].clone() {
            if ts.len()==tr.parameters.len() {
               for (pt,(_bi,bt,_bk)) in std::iter::zip(ts,&tr.parameters) {
                  self.implies(pt, &bt, span)?;
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
         Type::MaybeZero(tt) => Type::MaybeZero(Box::new(self.extend_implied(tt))),
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
         Type::HTuple(bt,ct) => Type::HTuple(Box::new(self.extend_implied(bt)),ct.clone()),
         Type::Product(ts) => Type::Product(ts.iter().map(|tc| self.extend_implied(tc)).collect::<Vec<Type>>()),
         Type::Constant(cv) => Type::Constant(cv.clone())
      }
   }
   pub fn kind(&self, _tt:&Type) -> Kind {
      self.term_kind.clone()
   }
   pub fn is_knormal(&self, k:&Kind) -> bool {
      let ks = k.flatten();
      ks.iter().any(|kf| self.kind_is_normal.contains(kf))
   }
   pub fn is_normal(&self, tt:&Type) -> bool {
      match tt {
         Type::Any => false,
         Type::MaybeZero(tt) => self.is_normal(tt),
         Type::And(ts) => ts.iter().any(|ct|self.is_normal(ct)),
         Type::Named(tn,ts) => self.type_is_normal.contains(&Type::Named(tn.clone(),Vec::new())) &&
                              ts.iter().all(|ct|self.is_normal(ct)),
         Type::Tuple(ts) => ts.iter().all(|ct|self.is_normal(ct)),
         Type::HTuple(bt,_ct) => self.is_normal(bt),
         Type::Product(ts) => ts.iter().all(|ct|self.is_normal(ct)),
         Type::Arrow(p,b) => self.is_normal(p) && self.is_normal(b),
         Type::Ratio(p,b) => self.is_normal(p) && self.is_normal(b),
         Type::Constant(_) => true,
      }
   }
   pub fn narrow(&self, kinds: &HashMap<Type,Kind>, projection: &Kind, tt: &Type) -> Type {
      match tt {
         Type::Any => Type::Any,
         Type::MaybeZero(tt) => {
            let tt = self.narrow(kinds,projection,tt);
            if tt.is_bottom() { return tt.clone(); }
            Type::MaybeZero(Box::new(tt))
         }
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
         Type::HTuple(bt,ct) => {
            let bt = self.narrow(kinds,projection,bt);
            if bt.is_bottom() { return self.bottom_type.clone(); }
            Type::HTuple(Box::new(bt),ct.clone())
         }
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
   pub fn make_scope(&mut self, parent: &Option<ScopeId>, sc: ScopeId, subs: &HashMap<Type,Type>) -> ScopeId {
      let scope = self.scopes[sc.id].clone();
      let mut children = Vec::new();
      for (c_n,c_k,c_t,c_b) in scope.children.iter() {
         let c_t = c_t.substitute(subs);
         let c_b = if let Some(c_b) = c_b {
            let c_b = self.make_template(parent, *c_b, subs);
            self.untyped(c_b);
            Some(c_b)
         } else { None };
         children.push((c_n.clone(),c_k.clone(),c_t,c_b));
      }
      let sc = self.push_scope(Scope {
         parent: parent.clone(),
         children: children,
      });
      sc
   }
   pub fn make_template(&mut self, scope: &Option<ScopeId>, b: TermId, subs: &HashMap<Type,Type>) -> TermId {
      let span = self.rows[b.id].span.clone();
      let rt = match &self.rows[b.id].term.clone() {
         Term::Let(lt) => {
            let mut pars = Vec::new();
            for curr in lt.parameters.iter() {
               let mut ps = Vec::new();
               for (n,t,k) in curr.iter() {
                  ps.push((n.clone(),t.substitute(subs),k.clone()));
               }
               pars.push(ps);
            }
            let scope = self.make_scope(scope, lt.scope,subs);
            let body = if let Some(b) = lt.body {
               Some(self.make_template(&Some(scope), b, subs))
            } else { None };
            let lt = self.push_term(Term::Let(LetTerm {
               is_extern: lt.is_extern,
               scope: scope,
               name: lt.name.clone(),
               parameters: pars,
               body: body,
               rtype: lt.rtype.substitute(subs),
               rkind: lt.rkind.clone(),
            }),&span);
            lt
         },
         Term::Ident(x) => { self.push_term(Term::Ident(x.clone()),&span) },
         Term::Value(x) => { self.push_term(Term::Ident(x.clone()),&span) },
         Term::App(g,x) => {
            let g = self.make_template(scope,*g,subs);
            let x = self.make_template(scope,*x,subs);
            self.push_term(Term::App(g,x),&span)
         },
         Term::Arrow(sc,p,rt,b) => {
            let sc = self.make_scope(scope,*sc,subs);
            let p = self.make_template(&None,*p,subs);
            let b = self.make_template(&Some(sc),*b,subs);
            let rt = if let Some(rt) = rt {
               Some(rt.substitute(subs))
            } else { None };
            self.push_term(Term::Arrow(sc,p,rt,b),&span)
         },
         Term::Block(sid,es) => {
            let sid = self.make_scope(scope,*sid,subs);
            let mut nes = Vec::new();
            for e in es.iter() {
               nes.push(self.make_template(&Some(sid),*e,subs));
            }
            self.push_term(Term::Block(sid,nes),&span)
         },
         Term::Tuple(es) => {
            let mut nes = Vec::new();
            for e in es.iter() {
               nes.push(self.make_template(scope,*e,subs));
            }
            self.push_term(Term::Tuple(nes),&span)
         },
         Term::Ascript(x,tt) => {
            let x = self.make_template(scope,*x,subs);
            let tt = tt.substitute(subs);
            self.push_term(Term::Ascript(x,tt),&span)
         },
         Term::Constructor(cn,fts) => {
            let mut nfts = Vec::new();
            for (f,ft) in fts.iter() {
               let ft = self.make_template(scope,*ft,subs);
               nfts.push((f.clone(),ft));
            }
            self.push_term(Term::Constructor(cn.clone(),nfts),&span)
         },
         Term::As(t,tt) => {
            let t = self.make_template(scope,*t,subs);
            let tt = tt.substitute(subs);
            self.push_term(Term::As(t,tt),&span)
         },
         _ => unimplemented!("Make template: {}", self.print_term(b))
      };
      rt
   }
   pub fn visit(&mut self, scope: &Option<ScopeId>, vt: &Option<TermId>, tt: &Type) -> Result<(),Error> {
      if let Some(vt) = vt {
      if let Term::Let(ref lt) = self.rows[vt.id].term.clone() {
      if let Some(ref _b) = lt.body {
         if self.poly_bindings.contains_key(&(lt.name.clone(),tt.clone())) {
            return Ok(());
         }
         let bt = lt.typeof_binding();
         if bt.is_open() {
            let mut subs = Vec::new();
            let merge = tt.subs_implication_unifier(&mut subs, &bt);
            let subs = {
               let mut hs = HashMap::new();
               for (l,r) in subs.iter() {
                  hs.insert(l.clone(),r.clone());
               }
               hs
            };
            assert!(!merge.is_bottom());
            let template = self.make_template(scope, *vt, &subs);
            self.poly_bindings.insert((lt.name.clone(),tt.clone()),template);
            self.typeck(&Some(lt.scope), template, None)?;
            self.untyped(*vt);
         }
      }}}
      Ok(())
   }
   pub fn typeof_var(&mut self, scope: &Option<ScopeId>, v: &str, implied: &Option<Type>, span: &Span) -> Result<Type,Error> {
      if let Some(sc) = scope {
         let mut candidates = Vec::new();
         let mut matches = Vec::new();
         let ref sc = self.scopes[sc.id].clone();
         for (tn,_tkts,tt,vt) in sc.children.iter() {
            if tn==v {
               //match variable binding if
               //1) binding is not an arrow
               //2) implied => binding

               candidates.push(tt.clone());
               if let Type::Arrow(_tp,_tb) = &tt {
               if let Some(it) = implied {
                  let rt = Type::implies(self, &it, &tt);
                  if rt.is_bottom() { continue; }
                  matches.push(rt.clone());
                  self.visit(scope, vt, &rt)?;
               }} else {
                  matches.push(tt.clone());
                  self.visit(scope, vt, &tt)?;
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
      self.rows[t.id].untyped = true;
      match self.rows[t.id].term.clone() {
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
         Term::As(t,_tt) => {
            self.untyped(t);
         },
         _ => panic!("TODO untype term: {}", self.print_term(t))
      }
   }
   pub fn cast_normal(&mut self, _l_only: &Type, _span: &Span) -> Result<Type,Error> {
      unimplemented!("Reimplement cast normalization")
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
   pub fn cast_into_kind(&mut self, mut _l_only: Type, _into: &Type, _span: &Span) -> Result<Type,Error> {
      unimplemented!("Reimplement cast into kind")
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
            let p = Term::reduce(self, &Some(invariant.scope), invariant.prop)?;
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
   pub fn destructure(&mut self, scope: ScopeId, t: TermId, tt: &Type) -> Result<(),Error> {
      let span = self.rows[t.id].span.clone();
      match (self.rows[t.id].term.clone(),tt) {
         (Term::Value(_),_) => {
         },
         (Term::Ident(tn),_) => {
            if tn != "_" {
               self.scopes[scope.id].children.push((
                  tn.clone(),
                  HashMap::new(),
                  tt.clone(),
                  None
               ));
            }
         },
         (Term::Constructor(cname,_kvs),_) => {
            let ct = if let Some((ct,_tpars,_tkvs)) = self.constructors.get(&cname) {
               ct.clone()
            } else { return Err(Error {
               kind: "Type Error".to_string(),
               rule: format!("type constructor, none found for: {}", self.print_term(t)),
               span: self.rows[t.id].span.clone(),
            }) };
            self.implies(&ct, tt, &span)?;
         },
         (Term::Tuple(vts),Type::Tuple(tts)) if vts.len()==tts.len() => {
            for (cv,ct) in std::iter::zip(vts,tts) {
               self.destructure(scope, cv, ct)?;
            }
         },
         (Term::Tuple(vts),Type::HTuple(bt,_)) => {
            for cv in vts.iter() {
               self.destructure(scope, *cv, bt)?;
            }
         },
         (Term::App(vg,vx),Type::Tuple(tts)) => {
            if let Term::Ident(vg) = self.rows[vg.id].term.clone() {
            if vg == "pos" {
            if let Term::Tuple(vx) = self.rows[vx.id].term.clone() {
            if vx.len()==1 {
            if let Term::Tuple(vx) = self.rows[vx[0].id].term.clone() {
            if vx.len()>0 {
               let mut prefix = None;
               let mut midfix = None;
               let mut suffix = None;
               let mut accept = true;
               for vxt in vx.iter() {
               match self.rows[vxt.id].term.clone() {
                  Term::Tuple(fix) => {
                     if midfix.is_none() && prefix.is_none() { prefix = Some((vxt, fix.clone())); }
                     else if suffix.is_none() { suffix = Some((vxt, fix.clone())); }
                     else { accept = false; }
                  },
                  Term::Ident(fix) => {
                     if midfix.is_none() { midfix = Some((vxt, fix)); }
                     else { accept = false; }
                  },
                  _ => { accept = false; }
               }}
               if accept && (prefix.is_some() || midfix.is_some() || suffix.is_some()) {
                  let mut tts = tts.clone();
                  let mut accept = true;
                  if let Some((_pret,prevs)) = prefix {
                  if prevs.len() <= tts.len() {
                     let pretts = tts[..prevs.len()].to_vec();
                     tts = tts[prevs.len()..].to_vec();
                     for (prevt,prett) in std::iter::zip(prevs,pretts) {
                        self.destructure(scope, prevt, &prett)?;
                     }
                  } else { accept = false; }}
                  if let Some((_suft,sufvs)) = suffix {
                  if sufvs.len() <= tts.len() {
                     let suftts = tts[(tts.len()-sufvs.len())..].to_vec();
                     tts = tts[..(tts.len()-sufvs.len())].to_vec();
                     for (sufvt,suftt) in std::iter::zip(sufvs,suftts) {
                        self.destructure(scope, sufvt, &suftt)?;
                     }
                  } else { accept = false; }}
                  if let Some((midt,_midvs)) = midfix {
                     self.destructure(scope, *midt, &Type::Tuple(tts.clone()))?;
                     tts = Vec::new();
                  }
                  if accept && tts.len() == 0 {
                     return Ok(());
                  }
               }
            }}}}}}
            return Err(Error {
               kind: "Type Error".to_string(),
               rule: format!("destructure app rejected {} : {:?}", self.print_term(t), tt),
               span: self.rows[t.id].span.clone(),
            });
         },
         _ => {
            return Err(Error {
               kind: "Type Error".to_string(),
               rule: format!("destructure rejected {} : {:?}", self.print_term(t), tt),
               span: self.rows[t.id].span.clone(),
            });
         },
      };
      Ok(())
   }
   pub fn typeck(&mut self, scope: &Option<ScopeId>, t: TermId, implied: Option<Type>) -> Result<(),Error> {
      let implied = implied.map(|tt|tt.normalize());
      //TODO: remove clone here because it is bloating the memory footprint
      match self.rows[t.id].term.clone() {
         Term::Project(_v) => panic!("Projection Constants cannot be Values at {:?}", &self.rows[t.id].span),
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
            for (clr,l,r) in lrs.iter() {
               self.destructure(*clr, *l, &self.rows[dv.id].typ.clone())?;
               self.untyped(*l);
               self.typeck(&Some(*clr), *r, implied.clone())?;
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
            if lt.is_extern {
               if let Some(ref b) = lt.body {
                  self.rows[b.id].typ = lt.rtype.clone();
               }
               self.rows[t.id].typ = self.nil_type.clone();
            } else if let Some(ref b) = lt.body {
               let bt = lt.typeof_binding();
               if !bt.is_open() {
                  self.typeck(&Some(lt.scope), *b, Some(lt.rtype.clone()))?;
               } else {
                  self.untyped(t);
               }
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
            let mut r = None;
            for (pat,re) in self.regexes.clone().into_iter() {
               if i==self.bottom_type && re.is_match(&x) {
                  r = Some(re.clone());
                  self.rows[t.id].typ = pat;
                  break;
               } else if let Ok(nt) = self.implies(&i,&pat,&self.rows[t.id].span.clone()) {
                  r = Some(re.clone());
                  self.rows[t.id].typ = nt;
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
               let vt = Term::reduce(self, scope, lhs)?;
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
                     let fat = fa_inference;
                     self.rows[t.id].typ = self.rows[t.id].typ.and( &fat );
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
            let Term::Ascript(pt,ptt) = &self.rows[p.id].term.clone()
            else { return Err(Error {
               kind: "Type Error".to_string(),
               rule: format!("Arrow lhs must be ascripted"),
               span: self.rows[t.id].span.clone(),
            }) };
            self.rows[pt.id].typ = ptt.clone();
            self.rows[p.id].typ = ptt.clone();
            self.typeck(&Some(*sc), b, rt)?;
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
               } else if let Type::HTuple(bt,Constant::Literal(blen)) = self.rows[x.id].typ.clone() {
                  let blen = str::parse::<usize>(&blen).unwrap();
                  if pi>=blen { return Err(Error {
                     kind: "Type Error".to_string(),
                     rule: format!("Cannot project out-of-bounds π{} from type {:?}", pi, &self.rows[x.id].typ),
                     span: self.rows[t.id].span.clone(),
                  }) }
                  self.rows[g.id].typ = *bt.clone();
                  self.rows[t.id].typ = *bt.clone();
               } else { return Err(Error {
                  kind: "Type Error".to_string(),
                  rule: format!("Cannot project π{} from type {:?}", pi, &self.rows[x.id].typ),
                  span: self.rows[t.id].span.clone(),
               }) }
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
      let poly_bindings_l = self.poly_bindings.clone();

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
      self.poly_bindings = poly_bindings_l;

      r?; Ok(())
   }
}
