use std::path::Path;
use pest::Parser;
use pest::iterators::{Pair,Pairs};
use pest::error::{ErrorVariant,InputLocation,LineColLocation};

#[derive(Parser)]
#[grammar = "grammar_tlc.pest"]
struct TlcParser;

pub struct TLC {
   rows: Vec<Row>,
   rules: Vec<TypeRule>,
}

pub struct Row {
   term: Term,
   typ: Typ,
   kind: Kind,
   span: Span,
}

#[derive(Clone)]
pub struct Span {
   filename: String,
   offset_start: usize,
   offset_end: usize,
   linecol_start: (usize,usize),
   linecol_end: (usize,usize),
}

pub struct Error {
   kind: String,
   rule: String,
   span: Span,
   snippet: String,
}
impl Span {
   pub fn snippet(&self) -> String {
      format!("")
   }
}
impl std::fmt::Debug for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "\n{}, expected {}, in {} --> {},{}\n{}\n", self.kind, self.rule, self.span.filename,
               self.span.linecol_start.0, self.span.linecol_start.1, self.span.snippet())
    }
}

//does not implement Clone because scopes are uniquely identified by their id
pub struct Scope {
   id: usize,
   parent: Option<usize>,
   children: Vec<(String,Term)>,
   statements: Vec<Term>,
}

#[derive(Clone)]
pub enum Kind {
   Simple(String,Vec<Kind>),
}

#[derive(Clone)]
pub enum Typ {
   Nil,
   Any,
   Ident(String),
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
           Typ::Ident(x) => write!(f, "{}", x),
           Typ::Or(_) => write!(f, "||"),
           Typ::And(_) => write!(f, "&&"),
           Typ::Arrow(p,b) => write!(f, "({:?})=>({:?})", p, b),
           Typ::Tuple(xs) => write!(f, "(?,?)"),
           Typ::Product(xs) => write!(f, "(?*?)"),
           Typ::Ratio(n,d) => write!(f, "({:?})/({:?})", n, d),
        }
    }
}

#[derive(Clone)]
pub enum TypeRule {
   //type Deca<U::Unit> :: Unit;
   Typedef(String,Vec<(Typ,Kind)>,Option<Kind>),

   //forall A,B,1,2. (A,B,1,2) => (B,1,2) :: A<B,1,2>;
   //forall U,u:Milli<U>. Milli<U> => U = 1000 * u;
   Forall(Vec<(String,Typ,Kind)>, Typ, Option<Typ>, Option<Term>, Option<Kind>),
}

#[derive(Clone)]
pub struct TermId {
   id: usize,
}
impl std::fmt::Debug for TermId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.id)
    }
}

#[derive(Clone)]
pub enum Term {
   Assume, //used to introduce sentinel values
   Nil,
   Ident(String),
   App(TermId,TermId),
   Let(String,TermId,Typ),
   Tuple(Vec<TermId>),
   Block(Vec<TermId>),
   Ascript(TermId,Typ),
}
impl std::fmt::Debug for Term {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
           Term::Assume => write!(f, "$"),
           Term::Nil => write!(f, "()"),
           Term::Ident(x) => write!(f, "{}", x),
           Term::App(g,x) => write!(f, "{:?}({:?})", g, x),
           Term::Let(v,x,t) => write!(f, "let {}: {:?} = {:?}", v, t, x),
           Term::Ascript(t,tt) => write!(f, "{:?}:{:?}", t, tt),
           Term::Tuple(es) => {
              write!(f, "(")?;
              for e in es.iter() {
                 write!(f, "{:?},", e)?;
              }
              write!(f, ")")
           },
           Term::Block(es) => {
              write!(f, "{{")?;
              for e in es.iter() {
                 write!(f, "{:?};", e)?;
              }
              write!(f, "}}")
           },
	}
    }
}

impl TLC {
   pub fn new() -> TLC {
      TLC {
         rows: Vec::new(),
         rules: Vec::new(),
      }
   }

   pub fn parse(&mut self, src:&str) -> Result<TermId,Error> {
      self.parse_doc("[string]", src)
   }
   pub fn parse_file(&mut self, filename:&str) -> Result<TermId,Error> {
      if !Path::new(filename).exists() {
         panic!("parse_file could not find file: '{}'", filename)
      }
      let src = std::fs::read_to_string(filename)
                   .expect("parse_file: Something went wrong reading the file");
      self.parse_doc(filename,&src)
   }
   pub fn parse_doc(&mut self, docname:&str, src:&str) -> Result<TermId,Error> {
      let parse_result = TlcParser::parse(Rule::file, src);
      match parse_result {
        Ok(parse_ast) => self.unparse_file(docname, parse_ast),
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
      self.unparse_ast(fp, ps.peek().unwrap())
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
   pub fn unparse_ast(&mut self, fp:&str, p: Pair<crate::tlc::Rule>) -> Result<TermId,Error> {
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
            let mut es = Vec::new();
            for e in p.into_inner() { match e.as_rule() {
               Rule::EOI => (),
               _ => es.push(self.unparse_ast(fp,e).expect("TLC Grammar Error in rule [file]"))
            }}
            Ok(self.push_term(Term::Block(es), &span))
         },

         //passthrough rules
         Rule::stmt => self.unparse_ast(fp,p.into_inner().next().expect("TLC Grammar Error in rule [stmt]")),
         Rule::term => self.unparse_ast(fp,p.into_inner().next().expect("TLC Grammar Error in rule [term]")),
         Rule::ident_term => self.unparse_ast(fp,p.into_inner().next().expect("TLC Grammar Error in rule [ident_term]")),
         Rule::tuple_term => self.unparse_ast(fp,p.into_inner().next().expect("TLC Grammar Error in rule [tuple_term]")),

         //literal value rules
         Rule::ident => Ok(self.push_term(Term::Ident(p.into_inner().concat()), &span)),

         //complex rules
         Rule::let_stmt => {
            let mut es = p.into_inner();
            Ok({let t = Term::Let(
               es.next().expect("TLC Grammar Error in rule [let_stmt.1]").into_inner().concat(),
               self.unparse_ast(fp,es.next().expect("TLC Grammar Error in rule [let_stmt.2]"))?,
               self.unparse_ast_typ(es.next().expect("TLC Grammar Error in rule [let_stmt.3]"))?,
            ); self.push_term(t, &span)})
         },
         Rule::let_stmt_val => {
            match p.into_inner().next() {
               None => Ok(self.push_term(Term::Assume, &span)),
               Some(e) => self.unparse_ast(fp,e)
            }
         },
         Rule::ascript_term => {
            let mut es = p.into_inner();
            let e = es.next().expect("TLC Grammar Error in rule [ascript_term]");
            match es.next() {
               None => self.unparse_ast(fp,e),
               Some(tt) => Ok({let t = Term::Ascript(
                  self.unparse_ast(fp,e)?, //term
                  self.unparse_ast_typ(tt)? //type
               ); self.push_term(t, &span)}),
            }
         },
         Rule::term_atom => {
            let mut es = p.into_inner();
            let mut e = self.unparse_ast(fp,es.next().expect("TLC Grammar Error in rule [term_atom]"))?;
            for args in es {
               e = {let t = Term::App(
                  e,
                  self.unparse_ast(fp,args)?
               ); self.push_term(t,&span)};
            }
            Ok(e)
         },
         Rule::paren_atom => {
            let es = p.into_inner().map(|e|self.unparse_ast(fp,e).expect("TLC Grammar Error in rule [paren_atom]"))
                      .collect::<Vec<TermId>>();
            if es.len()==0 {
               Ok(self.push_term(Term::Nil, &span))
            } else if es.len()==1 {
               Ok(es[0].clone())
            } else {
               Ok(self.push_term(Term::Tuple(es), &span))
            }
         },

         /* inference rules 
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
                           Some(et) => Some(self.unparse_ast_typ(et)?),
                           None => None
                        }
                     ));
                  },
                  Rule::typ => tt = Some(self.unparse_ast_typ(e)?),
                  Rule::kind => k = Some(self.unparse_ast_kind(e)?),
                  rule => panic!("unexpected forall_stmt rule: {:?}", rule)
               }
            }
            Ok(Term::Forall(
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
                           Some(et) => Some(self.unparse_ast_typ(et)?),
                           None => None
                        }
                     ));
                  },
                  rule => panic!("unexpected typ_stmt rule: {:?}", rule)
               }
            }
            Ok(Term::Typedef(
               self.uuid(),
               t,
               ts
            ))
         },
         */

         rule => panic!("unexpected expr rule: {:?}", rule)
      }
   }
   pub fn unparse_ast_typ(&mut self, p: Pair<crate::tlc::Rule>) -> Result<Typ,Error> {
      match p.as_rule() {
         Rule::ident => Ok(Typ::Ident(p.into_inner().concat())),
         Rule::typ => self.unparse_ast_typ(p.into_inner().next().expect("TLC Grammar Error in rule [typ]")),
         Rule::ident_typ => self.unparse_ast_typ(p.into_inner().next().expect("TLC Grammar Error in rule [ident_typ]")),
         Rule::atom_typ => self.unparse_ast_typ(p.into_inner().next().expect("TLC Grammar Error in rule [atom_typ]")),
         Rule::ident_typ => Ok(Typ::Ident(p.into_inner().concat())),
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
            let mut t = self.unparse_ast_typ(ts.next().expect("TLC Grammar Error in rule [suffix_typ]"))?;
            for t in ts {
              //TODO parameterized types and bracketed types
            }
            Ok(t)
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
         Rule::let_stmt_typ => {
            match p.into_inner().next() {
               None => Ok(Typ::Nil),
               Some(e) => self.unparse_ast_typ(e)
            }
         },
         rule => panic!("unexpected typ rule: {:?}", rule)
      }
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
   pub fn check(&mut self, globals: Option<usize>, src:&str) -> Result<(),Error> {
      let ast = self.parse(src)?;
      let locals = self.desugar(globals, &ast)?;
      self.typecheck(Some(locals), &ast)?;
      self.sanitycheck(Some(locals), &ast)
   }
}
*/
