use std::collections::{HashMap};
use crate::term::{Term,TermId};
use crate::debug::{Error};
use crate::token::{Token,Symbol,span_of};
use crate::scope::{ScopeId,Scope};
use crate::tlc::{TLC,TypeRule,Invariant,Typedef};
use crate::typ::{Type};
use crate::kind::{Kind};

fn peek_is_regex(tokens: &mut Vec<Token>) -> bool {
   if let Some(t) = tokens.get(0) {
      if let Symbol::Regex(_) = t.symbol {
         true
      } else { false }
   } else { false }
}
fn peek_is_typename(tokens: &mut Vec<Token>) -> bool {
   if let Some(t) = tokens.get(0) {
      if let Symbol::Typename(_) = t.symbol {
         true
      } else { false }
   } else { false }
}
fn peek_is(tokens: &mut Vec<Token>, is: &Vec<Symbol>) -> bool {
   if let Some(t) = tokens.get(0) {
      is.contains(&t.symbol)
   } else { false }
}
fn pop_is(rule: &str, tokens: &mut Vec<Token>, is: &Vec<Symbol>) -> Result<Symbol,Error> {
   if let Some(t) = tokens.get(0) {
      if !is.contains(&t.symbol) {
         Err(Error {
            kind: "Parse Error".to_string(),
            rule: format!("unexpected Symbol {:?} in rule {}", &t.symbol, rule),
            span: span_of(tokens),
         })
      } else {
         let t = tokens.remove(0);
         Ok(t.symbol.clone())
      }
   } else { //this branch should hopefully be dead code
      Err(Error {
         kind: "Parse Error".to_string(),
         rule: format!("unexpected End-Of-File in rule {}", rule),
         span: span_of(tokens),
      })
   }
}

pub fn ll1_kind(tlc: &mut TLC, tokens: &mut Vec<Token>) -> Result<Kind,Error> {
   todo!("ll1-kind")
   //Kind1 + Kind2<Kind3,Kind4>
}

pub fn ll1_type_stmt(tlc: &mut TLC, scope: ScopeId, tokens: &mut Vec<Token>) -> Result<TermId,Error> {
   let span = span_of(tokens);
   let mut t = "".to_string();
   let mut normal = false;
   let mut implies = None;
   let mut tiks: Vec<(String,Option<Type>,Kind)> = Vec::new();
   let mut typedef = Vec::new();
   let mut kinds = tlc.term_kind.clone();
   let mut props: Vec<Invariant> = Vec::new();
   let mut constructors: Vec<String> = Vec::new();
   let mut dept: HashMap<String,TermId> = HashMap::new();

   /*
   regex = { "/" ~ (!"/" ~ ANY)+ ~ "/" }
   typedef_branch = { regex | constructor_typedef }
   typedef = { typedef_branch ~ ("|" ~ typedef_branch)* }

   typ_invariant = { ident_typ_kind? ~ ("," ~ ident_typ_kind)* ~ "." ~ term ~ ("|" ~ term)? }
   */
   pop_is("type-stmt", tokens, &vec![Symbol::Type])?;
   if peek_is(tokens, &vec![Symbol::Normal]) {
      pop_is("type-stmt", tokens, &vec![Symbol::Normal])?;
      normal = true;
   }
   if tokens.len()>0 {
      if let Symbol::Typename(tname) = tokens[0].symbol.clone() {
         tokens.remove(0);
         t = tname.clone();
      } else {
         pop_is("type-stmt", tokens, &vec![Symbol::Typename("T".to_string())])?;
      }
   } else {
      pop_is("type-stmt", tokens, &vec![Symbol::Typename("T".to_string())])?;
   }

   if peek_is(tokens, &vec![Symbol::LessThan]) {
      pop_is("type-stmt", tokens, &vec![Symbol::LessThan])?;
      todo!("implement type stmt parameters {}", t);
      //[typ:inf::kind]
      pop_is("type-stmt", tokens, &vec![Symbol::GreaterThan])?;
   }
   let struct_typ = Type::Ident(t.clone(), tiks.iter().map(|(t,_i,_k)|Type::Ident(t.clone(),Vec::new())).collect::<Vec<Type>>());

   if peek_is(tokens, &vec![Symbol::Ascript]) {
      pop_is("type-stmt", tokens, &vec![Symbol::Ascript])?;
      implies = Some( ll1_type(tlc, &mut dept, scope, tokens)? );
   }

   if peek_is(tokens, &vec![Symbol::Is]) {
      pop_is("type-stmt", tokens, &vec![Symbol::Is])?;

      while peek_is_typename(tokens) || peek_is_regex(tokens) || peek_is(tokens, &vec![Symbol::LeftBrace]) {
         let mut tcname = t.clone();
         let mut tcrows = Vec::new();
         if let Symbol::Regex(r) = tokens[0].symbol.clone() {
            tokens.remove(0);
            typedef.push( Typedef::Regex(r.clone()) );
            continue;
         };
         if let Symbol::Typename(tn) = tokens[0].symbol.clone() {
            tokens.remove(0);
            tcname = tn.clone();
         };
         if peek_is(tokens, &vec![Symbol::LeftBrace]) {
            pop_is("type-stmt", tokens, &vec![Symbol::LeftBrace])?;
            while !peek_is(tokens, &vec![Symbol::RightBrace]) {
               if peek_is(tokens, &vec![Symbol::Comma]) {
                  pop_is("type-stmt", tokens, &vec![Symbol::Comma])?;
               }
               let mut ki = "".to_string();
               if tokens.len()>0 {
               if let Symbol::Ident(f) = tokens[0].symbol.clone() {
                  ki = f.clone();
               }}
               pop_is("type-stmt", tokens, &vec![Symbol::Ascript])?;
               let kt = ll1_type(tlc, &mut dept, scope, tokens)?;
               let vn = format!(".{}", ki.clone());
               let vt = tlc.push_term(Term::Ident(vn.clone()),&span);
               tlc.untyped(vt);
               tlc.scopes[scope.id].children.push((
                  vn,
                  HashMap::new(),
                  Type::Arrow(Box::new(struct_typ.clone()),Box::new(kt.clone())),
                  vt
               ));
               tcrows.push((ki,kt));
            }
            pop_is("type-stmt", tokens, &vec![Symbol::RightBrace])?;
         }
         constructors.push(tcname.clone());
         typedef.push( Typedef::Constructor(tcname,tcrows) );

         if peek_is(tokens, &vec![Symbol::Bar]) {
            pop_is("type-stmt", tokens, &vec![Symbol::Bar])?;
         };
      }
   }

   if peek_is(tokens, &vec![Symbol::KAscript]) {
      pop_is("type-stmt", tokens, &vec![Symbol::KAscript])?;
      kinds = ll1_kind(tlc, tokens)?;
   }

   if peek_is(tokens, &vec![Symbol::Where]) {
      pop_is("type-stmt", tokens, &vec![Symbol::Where])?;
      todo!("type stmt invariants")
   }

   if normal {
      if constructors.len()==0 {
         //constructors are preferred normal forms
         tlc.type_is_normal.insert(Type::Ident(t.clone(),Vec::new()));
      }
      for k in kinds.flatten().iter() {
         if k == &tlc.term_kind { continue; } //Term is never normal
         tlc.kind_is_normal.insert(k.clone());
      }
   }
   tlc.typedef_index.insert(t.clone(), tlc.rules.len());
   for c in constructors.iter() {
      if normal {
         tlc.type_is_normal.insert(Type::Ident(c.clone(),Vec::new()));
      }
      if &t==c { continue; } //constructor has same name as type
      tlc.typedef_index.insert(c.clone(), tlc.rules.len());
   }
   for inv in props.iter() {
   if let Term::App(g,x) = &tlc.rows[inv.prop.id].term.clone() {
   if let Term::Ident(gn) = &tlc.rows[g.id].term.clone() {
      let mut xs = Vec::new();
      let mut fkts = HashMap::new();
      match &tlc.rows[x.id].term.clone() {
         Term::Tuple(ts) => {
            for ct in ts.iter() {
               if let Term::Ident(ctn) = &tlc.rows[ct.id].term.clone() {
               if ctn == "tlc" { //replace "tlc" in invariants with this type rule
                  xs.push(Type::Ident(t.clone(),Vec::new()));
                  fkts.insert(xs[xs.len()-1].clone(), tlc.term_kind.clone());
                  continue;
               }}
               let ctt = tlc.rows[ct.id].term.clone();
               xs.push(tlc.push_dep_type(&ctt, *ct));
               fkts.insert(xs[xs.len()-1].clone(), tlc.constant_kind.clone());
            }
         }, pt => {
            let mut is_self = false;
            if let Term::Ident(ctn) = pt {
            if ctn == "self" { //replace "self" in invariants with this type rule
               xs.push(Type::Ident(t.clone(),Vec::new()));
               fkts.insert(xs[xs.len()-1].clone(), tlc.term_kind.clone());
               is_self = true;
            }}
            if !is_self {
               xs.push(tlc.push_dep_type(&pt, *x));
               fkts.insert(xs[xs.len()-1].clone(), tlc.constant_kind.clone());
            }
         }
      }
      let rt = tlc.push_dep_type(&tlc.rows[inv.algs.id].term.clone(), inv.algs);

      let pt = if xs.len()==1 {
         xs[0].clone()
      } else {
         Type::Tuple(xs)
      };
      let ft = Type::Arrow(Box::new(pt),Box::new(rt));
      let vt = tlc.push_term(Term::Ident(gn.clone()),&span);
      tlc.untyped(vt);
      tlc.scopes[scope.id].children.push((gn.clone(), fkts, ft, vt));
   }}}
   tlc.rules.push(TypeRule::Typedef(
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

   /*
            for e in p.into_inner() { match e.as_rule() {
               Rule::typ_inf_kind => {
                  let mut typ = "".to_string();
                  let mut inf = None;
                  let mut kind = tlc.term_kind.clone();
                  for tik in e.into_inner() { match tik.as_rule() {
                     Rule::typvar => { typ = tik.into_inner().concat(); },
                     Rule::typ   => { inf   = Some(tlc.unparse_ast_type(&mut dept,scope,fp,tik,span)?); },
                     Rule::kind   => { kind   = tlc.unparse_ast_kind(scope,fp,tik,span)?; },
                     rule => panic!("unexpected ident_typ_kind rule: {:?}", rule)
                  }}
                  tiks.push((typ,inf,kind));
               },
               Rule::typ_invariant => {
                  let mut itks = Vec::new();
                  let mut prop = None;
                  let mut algs = None;
                  for tip in e.into_inner() { match tip.as_rule() {
                     Rule::ident_typ_kind => {
                        let mut idn = None;
                        let mut inf = None;
                        let mut kind = tlc.term_kind.clone();
                        for itk in tip.into_inner() { match itk.as_rule() {
                           Rule::ident => { idn = Some(itk.into_inner().concat()); },
                           Rule::typ   => { inf   = Some(tlc.unparse_ast_type(&mut dept,scope,fp,itk,span)?); },
                           Rule::kind   => { kind   = tlc.unparse_ast_kind(scope,fp,itk,span)?; },
                           rule => panic!("unexpected ident_typ_kind rule: {:?}", rule)
                        }}
                        itks.push((idn,inf,kind));
                     },
                     Rule::term => {
                        if prop.is_none() {
                           prop = Some(tlc.unparse_ast(scope,fp,tip,span)?);
                        } else {
                           algs = Some(tlc.unparse_ast(scope,fp,tip,span)?);
                        }
                     }
                     rule => panic!("unexpected typ_invariant rule: {:?}", rule)
                  }}
                  let algs = if let Some(a) = algs { a }
                  else { tlc.push_term(Term::Ident("True".to_string()),&span) };
                  props.push(Invariant {
                     itks: itks,
                     prop: prop.expect("TLC Grammar Error in rule [typ_invariant]"),
                     algs: algs,
                  });
               },
               rule => panic!("unexpected typ_stmt rule: {:?}", rule)
            }}
   */
}

pub fn ll1_forall_stmt(_tlc: &mut TLC, _scope: ScopeId, _tokens: &mut Vec<Token>) -> Result<TermId,Error> {
   let mut quants: Vec<(Option<String>,Option<Type>,Kind)> = Vec::new();
   let mut inference  = None;
   let mut term = None;
   let mut kind = self.term_kind.clone();
   let mut dept = HashMap::new();
   todo!("implement ll1_forall_stmt")
   /*
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
   */
}

pub fn ll1_let_stmt(tlc: &mut TLC, scope: ScopeId, tokens: &mut Vec<Token>) -> Result<TermId,Error> {
   let span = span_of(tokens);
   pop_is("let-stmt", tokens, &vec![Symbol::Let])?;
   let ident = if let Some(t) = tokens.get(0) {
      if let Symbol::Ident(id) = tokens.remove(0).symbol {
         id.clone()
      } else {
         pop_is("let-stmt", tokens, &vec![Symbol::Ident("x".to_string())])?;
         unreachable!("let-stmt")
      }
   } else {
      pop_is("let-stmt", tokens, &vec![Symbol::Ident("x".to_string())])?;
      unreachable!("let-stmt")
   };
   let mut pars: Vec<Vec<(Option<String>,Option<Type>,Kind)>> = Vec::new();
   let mut rt = tlc.nil_type.clone();
   let mut rk = tlc.term_kind.clone();
   let mut t: Option<TermId> = None;
   let mut dept = HashMap::new();

   while peek_is(tokens, &vec![Symbol::LeftParen]) {
      pop_is("let-stmt", tokens, &vec![Symbol::LeftParen])?;
      let mut itks = Vec::new();
      while !peek_is(tokens, &vec![Symbol::RightParen]) {
         while peek_is(tokens, &vec![Symbol::Comma]) {
            pop_is("let-stmt", tokens, &vec![Symbol::Comma])?;
         }
         let mut ident = None;
         let mut typ = None;
         let mut kind = tlc.term_kind.clone();
         if tokens.len()>0 {
         if let Symbol::Ident(id) = &tokens[0].symbol.clone() {
            tokens.remove(0);
            ident = Some(id.clone());
         }}
         if peek_is(tokens, &vec![Symbol::Ascript]) {
            pop_is("let-stmt", tokens, &vec![Symbol::Ascript])?;
         }
         if !peek_is(tokens, &vec![Symbol::RightParen,Symbol::Comma,Symbol::KAscript]) {
            typ = Some(ll1_type(tlc, &mut dept, scope, tokens)?);
         }
         if peek_is(tokens, &vec![Symbol::KAscript]) {
            pop_is("let-stmt", tokens, &vec![Symbol::KAscript])?;
            kind = ll1_kind(tlc, tokens)?;
         }
         if let Some(tt) = &typ {
         if tt.is_constant() {
            kind = tlc.constant_kind.clone();
         }};
         itks.push((ident,typ,kind));
      }
      pop_is("let-stmt", tokens, &vec![Symbol::RightParen])?;
      pars.push(itks);
   }

   if peek_is(tokens, &vec![Symbol::Ascript]) {
      pop_is("let-stmt", tokens, &vec![Symbol::Ascript])?;
      rt = ll1_type(tlc, &mut dept, scope, tokens)?;
   }
   if peek_is(tokens, &vec![Symbol::KAscript]) {
      pop_is("let-stmt", tokens, &vec![Symbol::KAscript])?;
      rk = ll1_kind(tlc, tokens)?;
   }
   if peek_is(tokens, &vec![Symbol::Is]) {
      pop_is("let-stmt", tokens, &vec![Symbol::Is])?;
      t = Some(ll1_term(tlc, scope, tokens)?);
   }

   if rt.is_constant() {
      rk = tlc.constant_kind.clone();
   }
   let mut children = Vec::new();
   for itks in pars.iter() {
      for (i,t,k) in itks.iter() {
         let t = t.clone().unwrap_or(tlc.bottom_type.clone()).normalize();
         let mut ks = HashMap::new(); ks.insert(t.clone(),k.clone());
         let vn = i.clone().unwrap_or("_".to_string());
         let vt = tlc.push_term(Term::Ident(vn.clone()),&span);
         tlc.untyped(vt);
         children.push((vn.clone(), ks, t.clone(), vt));
      }
   }
   let mut ft = rt.clone();
   let mut fkts = HashMap::new();
   for itks in pars.iter().rev() {
      let mut ps = Vec::new();
      for (_i,t,k) in itks.iter() {
         let t = t.clone().unwrap_or(tlc.bottom_type.clone()).normalize();
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
   tlc.reduce_type(&HashMap::new(), &mut ft, &span); //destructively reduce constants in type
   ft = ft.normalize();
   let vt = tlc.push_term(Term::Ident(ident.clone()), &span);
   tlc.untyped(vt);
   tlc.scopes[scope.id].children.push((ident.clone(), fkts, ft, vt));
   let inner_scope = tlc.push_scope(Scope {
      parent: Some(scope),
      children: children,
   }, &span);
   Ok(tlc.push_term(Term::Let(inner_scope,ident,pars,t,rt,rk), &span))
}

pub fn ll1_if_term(tlc: &mut TLC, scope: ScopeId, tokens: &mut Vec<Token>) -> Result<TermId,Error> {
   todo!("implement if term")
}

pub fn ll1_logical_term(tlc: &mut TLC, scope: ScopeId, tokens: &mut Vec<Token>) -> Result<TermId,Error> {
   let span = span_of(tokens);
   let mut term = ll1_compare_term(tlc, scope, tokens)?;
   while peek_is(tokens, &vec![Symbol::And,Symbol::Or]) {
      let op = pop_is("logical-term", tokens, &vec![Symbol::And,Symbol::Or])?;
      let op = format!("{}", op);
      let term2 = ll1_compare_term(tlc, scope, tokens)?;
      let t = Term::App(
         tlc.push_term(Term::Ident(op),&span),
         tlc.push_term(Term::Tuple(vec![term,term2]),&span),
      );
      term = tlc.push_term(t,&span);
   }
   Ok(term)
}

pub fn ll1_compare_term(tlc: &mut TLC, scope: ScopeId, tokens: &mut Vec<Token>) -> Result<TermId,Error> {
   let span = span_of(tokens);
   let mut term = ll1_addsub_term(tlc, scope, tokens)?;
   while peek_is(tokens, &vec![Symbol::Equal,Symbol::NotEqual,Symbol::GreaterThan,Symbol::GreaterThanOrEqual,Symbol::LessThan,Symbol::LessThanOrEqual]) {
      let op = pop_is("compare-term", tokens, &vec![Symbol::Equal,Symbol::NotEqual,Symbol::GreaterThan,Symbol::GreaterThanOrEqual,Symbol::LessThan,Symbol::LessThanOrEqual])?;
      let op = format!("{}", op);
      let term2 = ll1_addsub_term(tlc, scope, tokens)?;
      let t = Term::App(
         tlc.push_term(Term::Ident(op),&span),
         tlc.push_term(Term::Tuple(vec![term,term2]),&span),
      );
      term = tlc.push_term(t,&span);
   }
   Ok(term)
}

pub fn ll1_addsub_term(tlc: &mut TLC, scope: ScopeId, tokens: &mut Vec<Token>) -> Result<TermId,Error> {
   let span = span_of(tokens);
   let mut term = ll1_divmul_term(tlc, scope, tokens)?;
   while peek_is(tokens, &vec![Symbol::Plus,Symbol::Minus]) {
      let op = pop_is("addsub-term", tokens, &vec![Symbol::Plus,Symbol::Minus])?;
      let op = format!("{}", op);
      let term2 = ll1_divmul_term(tlc, scope, tokens)?;
      let t = Term::App(
         tlc.push_term(Term::Ident(op),&span),
         tlc.push_term(Term::Tuple(vec![term,term2]),&span),
      );
      term = tlc.push_term(t,&span);
   }
   Ok(term)
}

pub fn ll1_divmul_term(tlc: &mut TLC, scope: ScopeId, tokens: &mut Vec<Token>) -> Result<TermId,Error> {
   let span = span_of(tokens);
   let mut term = ll1_power_term(tlc, scope, tokens)?;
   while peek_is(tokens, &vec![Symbol::Div,Symbol::Mul,Symbol::Mod]) {
      let op = pop_is("divmul-term", tokens, &vec![Symbol::Div,Symbol::Mul,Symbol::Mod])?;
      let op = format!("{}", op);
      let term2 = ll1_power_term(tlc, scope, tokens)?;
      let t = Term::App(
         tlc.push_term(Term::Ident(op),&span),
         tlc.push_term(Term::Tuple(vec![term,term2]),&span),
      );
      term = tlc.push_term(t,&span);
   }
   Ok(term)
}

pub fn ll1_power_term(tlc: &mut TLC, scope: ScopeId, tokens: &mut Vec<Token>) -> Result<TermId,Error> {
   let span = span_of(tokens);
   let mut term = ll1_prefix_term(tlc, scope, tokens)?;
   while peek_is(tokens, &vec![Symbol::Pow]) {
      let op = pop_is("power-term", tokens, &vec![Symbol::Pow])?;
      let op = format!("{}", op);
      let term2 = ll1_prefix_term(tlc, scope, tokens)?;
      let t = Term::App(
         tlc.push_term(Term::Ident(op),&span),
         tlc.push_term(Term::Tuple(vec![term,term2]),&span),
      );
      term = tlc.push_term(t,&span);
   }
   Ok(term)
}

pub fn ll1_prefix_term(tlc: &mut TLC, scope: ScopeId, tokens: &mut Vec<Token>) -> Result<TermId,Error> {
   let span = span_of(tokens);
   let mut ops = Vec::new();
   while peek_is(tokens, &vec![Symbol::Plus,Symbol::Minus]) {
      let op = pop_is("prefix-term", tokens, &vec![Symbol::Plus,Symbol::Minus])?;
      ops.push(op);
   }
   let mut term = ll1_atom_term(tlc, scope, tokens)?;
   while ops.len()>0 {
      let topop = if ops.pop()==Some(Symbol::Plus) { "pos".to_string() } else { "neg".to_string() };
      let t = Term::App(
         tlc.push_term(Term::Ident(topop),&span),
         term
      );
      term = tlc.push_term(t,&span);
   }
   Ok(term)
}

pub fn ll1_tuple_term(tlc: &mut TLC, scope: ScopeId, tokens: &mut Vec<Token>) -> Result<TermId,Error> {
   todo!("implement tuple term")
}

pub fn ll1_value_term(tlc: &mut TLC, scope: ScopeId, tokens: &mut Vec<Token>) -> Result<TermId,Error> {
   let span = span_of(tokens);
   if let Some(sym) = tokens.get(0) {
      if let Symbol::Ident(x) = sym.symbol.clone() {
         tokens.remove(0);
         return Ok(tlc.push_term(Term::Ident(x.clone()), &span))
      } else if let Symbol::Value(x) = sym.symbol.clone() {
         tokens.remove(0);
         return Ok(tlc.push_term(Term::Value(x.clone()), &span))
      } else if let Symbol::Typename(cname) = sym.symbol.clone() {
         tokens.remove(0);
         let kvs = Vec::new();
         //key_value = { ident ~ "=" ~ term }
         //constructor = { typname ~ ("{" ~ (key_value ~ ("," ~ key_value)*)? ~ "}")? }
         return Ok(tlc.push_term(Term::Constructor(
            cname.clone(),
            kvs
         ),&span));
      }
   }
   pop_is("value-term",tokens,&vec![
      Symbol::Ident("x".to_string()),
      Symbol::Typename("A".to_string()),
      Symbol::Value("1".to_string()),
   ])?;
   unreachable!("value-term expected Ident, Typename, or Value")
}

pub fn ll1_field_term(tlc: &mut TLC, scope: ScopeId, tokens: &mut Vec<Token>) -> Result<TermId,Error> {
   todo!("implement field term")
}

pub fn ll1_atom_term(tlc: &mut TLC, scope: ScopeId, tokens: &mut Vec<Token>) -> Result<TermId,Error> {
   let span = span_of(tokens);
   let mut term = if peek_is(tokens, &vec![Symbol::LeftParen]) {
      ll1_tuple_term(tlc, scope, tokens)?
   } else {
      ll1_value_term(tlc, scope, tokens)?
   };
   while peek_is(tokens, &vec![Symbol::LeftParen,Symbol::Dot]) {
      if peek_is(tokens, &vec![Symbol::LeftParen]) {
         let field = ll1_field_term(tlc, scope, tokens)?;
         let t = Term::App(
            field,
            term
         );
         term = tlc.push_term(t,&span);
      } else {
         let args = ll1_tuple_term(tlc, scope, tokens)?;
         let t = Term::App(
            term,
            args
         );
         term = tlc.push_term(t,&span);
      }
   }
   Ok(term)
}

pub fn ll1_expr_term(tlc: &mut TLC, scope: ScopeId, tokens: &mut Vec<Token>) -> Result<TermId,Error> {
   ll1_logical_term(tlc, scope, tokens)
}

pub fn ll1_algebra_term(tlc: &mut TLC, scope: ScopeId, tokens: &mut Vec<Token>) -> Result<TermId,Error> {
   let span = span_of(tokens);
   let mut term = ll1_expr_term(tlc, scope, tokens)?;
   while peek_is(tokens, &vec![Symbol::BackSlash]) {
      pop_is("algebra-term", tokens, &vec![Symbol::BackSlash])?;
      pop_is("algebra-term", tokens, &vec![Symbol::LeftBracket])?;
      let mut a = ll1_expr_term(tlc, scope, tokens)?;
      pop_is("algebra-term", tokens, &vec![Symbol::Bar])?;
      let mut b = ll1_expr_term(tlc, scope, tokens)?;
      pop_is("algebra-term", tokens, &vec![Symbol::RightBracket])?;
      tlc.untyped(a); tlc.unify_varnames(&mut HashMap::new(),&mut a);
      tlc.untyped(b); tlc.unify_varnames(&mut HashMap::new(),&mut b);
      term = {let t = Term::Substitution(
         term,
         a,
         b
      ); tlc.push_term(t,&span)};
   }
   Ok(term)
}

pub fn ll1_paren_type(tlc: &mut TLC, dept: &mut HashMap<String,TermId>, scope: ScopeId, tokens: &mut Vec<Token>) -> Result<Type,Error> {
   pop_is("paren-type", tokens, &vec![Symbol::LeftParen])?;
   let mut ts = Vec::new();
   while !peek_is(tokens, &vec![Symbol::RightParen]) {
      if peek_is(tokens, &vec![Symbol::Comma]) {
         pop_is("paren-type", tokens, &vec![Symbol::Comma])?;
      }
      ts.push( ll1_type(tlc, dept, scope, tokens)? );
   }
   pop_is("paren-type", tokens, &vec![Symbol::RightParen])?;
   if ts.len()==1 {
      Ok(ts[0].clone())
   } else {
      Ok(Type::Tuple(ts))
   }
}

pub fn ll1_typeof_type(tlc: &mut TLC, dept: &mut HashMap<String,TermId>, scope: ScopeId, tokens: &mut Vec<Token>) -> Result<Type,Error> {
   let span = span_of(tokens);
   pop_is("atom-type", tokens, &vec![Symbol::Typeof])?;
   pop_is("atom-type", tokens, &vec![Symbol::LeftParen])?;
   let vt = if tokens.len()>0 {
      if let Symbol::Ident(v) = tokens[0].symbol.clone() {
         tlc.typeof_var(&Some(scope), &v, &None, &span)?
      } else {
         pop_is("atom-type", tokens, &vec![Symbol::Ident("v".to_string())])?;
         unreachable!("atom-type")
      }
   } else {
      pop_is("atom-type", tokens, &vec![Symbol::Ident("v".to_string())])?;
      unreachable!("atom-type")
   };
   pop_is("atom-type", tokens, &vec![Symbol::RightParen])?;
   Ok(vt)
}

pub fn ll1_ident_type(tlc: &mut TLC, dept: &mut HashMap<String,TermId>, scope: ScopeId, tokens: &mut Vec<Token>) -> Result<Type,Error> {
   let tn = if tokens.len()>0 {
      if let Symbol::Typename(tn) = tokens[0].symbol.clone() {
         tokens.remove(0);
         tn.clone()
      } else {
         pop_is("ident-type", tokens, &vec![Symbol::Typename("T".to_string())])?;
         unreachable!("ident-type")
      }
   } else {
      pop_is("ident-type", tokens, &vec![Symbol::Typename("T".to_string())])?;
      unreachable!("ident-type")
   };
   let mut tps = Vec::new();
   if peek_is(tokens, &vec![Symbol::LessThan]) {
      pop_is("ident-type", tokens, &vec![Symbol::LessThan])?;
      while !peek_is(tokens, &vec![Symbol::GreaterThan]) {
         if peek_is(tokens, &vec![Symbol::Comma]) {
            pop_is("ident-type", tokens, &vec![Symbol::Comma])?;
         }
         tps.push( ll1_type(tlc, dept, scope, tokens)? );
      }
      pop_is("ident-type", tokens, &vec![Symbol::GreaterThan])?;
   }
   Ok(Type::Ident(tn,tps))
}

pub fn ll1_dep_type(tlc: &mut TLC, dept: &mut HashMap<String,TermId>, scope: ScopeId, tokens: &mut Vec<Token>) -> Result<Type,Error> {
   let span = span_of(tokens);
   pop_is("dependent-type", tokens, &vec![Symbol::LeftBracket])?;
   let mut t = ll1_term(tlc, scope, tokens)?;
   pop_is("dependent-type", tokens, &vec![Symbol::RightBracket])?;
   tlc.untyped(t);
   tlc.unify_varnames(dept,&mut t);
   let ct = tlc.push_dep_type(&tlc.rows[t.id].term.clone(), t);
   Ok(ct)
}

pub fn ll1_atom_type(tlc: &mut TLC, dept: &mut HashMap<String,TermId>, scope: ScopeId, tokens: &mut Vec<Token>) -> Result<Type,Error> {
   if peek_is(tokens, &vec![Symbol::Question]) {
      pop_is("atom-type", tokens, &vec![Symbol::Question])?;
      Ok(Type::Any)
   } else if peek_is(tokens, &vec![Symbol::LeftParen]) {
      ll1_paren_type(tlc, dept, scope, tokens)
   } else if peek_is(tokens, &vec![Symbol::LeftBracket]) {
      ll1_dep_type(tlc, dept, scope, tokens)
   } else if peek_is(tokens, &vec![Symbol::Typeof]) {
      ll1_typeof_type(tlc, dept, scope, tokens)
   } else {
      ll1_ident_type(tlc, dept, scope, tokens)
   }
}

pub fn ll1_suffix_type(tlc: &mut TLC, dept: &mut HashMap<String,TermId>, scope: ScopeId, tokens: &mut Vec<Token>) -> Result<Type,Error> {
   let span = span_of(tokens);
   let mut base = ll1_atom_type(tlc, dept, scope, tokens)?;
   let mut ts = Vec::new();
   while peek_is(tokens, &vec![Symbol::LeftBracket]) {
      pop_is("suffix-type", tokens, &vec![Symbol::LeftBracket])?;
      if !peek_is(tokens, &vec![Symbol::RightBracket]) {
         ts.push(Some( ll1_term(tlc, scope, tokens)? ));
      } else {
         ts.push(None);
      };
      pop_is("suffix-type", tokens, &vec![Symbol::RightBracket])?;
   }
   for bracketed in ts.iter().rev() {
      let mut dt = if let Some(br) = bracketed {
         *br
      } else {
         tlc.push_term(Term::Ident("length".to_string()), &span)
      };
      tlc.untyped(dt);
      tlc.unify_varnames(dept,&mut dt);
      let ct = tlc.push_dep_type(&tlc.rows[dt.id].term.clone(), dt);
      base = Type::Ident("Tensor".to_string(), vec![
         base,
         ct
      ]);
   }
   Ok(base)
}

pub fn ll1_product_type(tlc: &mut TLC, dept: &mut HashMap<String,TermId>, scope: ScopeId, tokens: &mut Vec<Token>) -> Result<Type,Error> {
   let mut types = vec![ ll1_suffix_type(tlc, dept, scope, tokens)? ];
   while peek_is(tokens, &vec![Symbol::Mul]) {
      pop_is("product-type", tokens, &vec![Symbol::Mul])?;
      types.push( ll1_suffix_type(tlc, dept, scope, tokens)? );
   }
   if types.len()==1 {
      Ok(types[0].clone())
   } else {
      Ok(Type::Product(types))
   }
}

pub fn ll1_ratio_type(tlc: &mut TLC, dept: &mut HashMap<String,TermId>, scope: ScopeId, tokens: &mut Vec<Token>) -> Result<Type,Error> {
   let mut typ = ll1_product_type(tlc, dept, scope, tokens)?;
   if peek_is(tokens, &vec![Symbol::Div]) {
      pop_is("ratio-type", tokens, &vec![Symbol::Div])?;
      let typ2 = ll1_product_type(tlc, dept, scope, tokens)?;
      typ = Type::Ratio(Box::new(typ), Box::new(typ2));
   }
   Ok(typ)
}

pub fn ll1_arrow_type(tlc: &mut TLC, dept: &mut HashMap<String,TermId>, scope: ScopeId, tokens: &mut Vec<Token>) -> Result<Type,Error> {
   let mut typ = ll1_ratio_type(tlc, dept, scope, tokens)?;
   if peek_is(tokens, &vec![Symbol::Arrow]) {
      pop_is("and-type", tokens, &vec![Symbol::Arrow])?;
      let typ2 = ll1_arrow_type(tlc, dept, scope, tokens)?;
      typ = Type::Arrow(Box::new(typ), Box::new(typ2));
   }
   Ok(typ)
}

pub fn ll1_and_type(tlc: &mut TLC, dept: &mut HashMap<String,TermId>, scope: ScopeId, tokens: &mut Vec<Token>) -> Result<Type,Error> {
   let mut types = vec![ ll1_arrow_type(tlc, dept, scope, tokens)? ];
   while peek_is(tokens, &vec![Symbol::Plus]) {
      pop_is("and-type", tokens, &vec![Symbol::Plus])?;
      types.push( ll1_arrow_type(tlc, dept, scope, tokens)? );
   }
   if types.len()==1 {
      Ok(types[0].clone())
   } else {
      Ok(Type::And(types))
   }
}

pub fn ll1_type(tlc: &mut TLC, dept: &mut HashMap<String,TermId>, scope: ScopeId, tokens: &mut Vec<Token>) -> Result<Type,Error> {
   ll1_and_type(tlc, dept, scope, tokens)
}

pub fn ll1_ascript_term(tlc: &mut TLC, scope: ScopeId, tokens: &mut Vec<Token>) -> Result<TermId,Error> {
   let span = span_of(tokens);
   let mut term = ll1_algebra_term(tlc, scope, tokens)?;
   if peek_is(tokens, &vec![Symbol::Ascript]) {
      pop_is("ascript-term", tokens, &vec![Symbol::Ascript])?;
      let at = ll1_type(tlc, &mut HashMap::new(), scope, tokens)?;
      let t = Term::Ascript(
         term,
         at
      );
      term = tlc.push_term(t, &span);
   }
   Ok(term)
}

pub fn ll1_as_term(tlc: &mut TLC, scope: ScopeId, tokens: &mut Vec<Token>) -> Result<TermId,Error> {
   let span = span_of(tokens);
   let mut term = ll1_ascript_term(tlc, scope, tokens)?;
   if peek_is(tokens, &vec![Symbol::As]) {
      pop_is("as-term", tokens, &vec![Symbol::As])?;
      let at = ll1_type(tlc, &mut HashMap::new(), scope, tokens)?;
      let t = Term::As(
         term,
         at
      );
      term = tlc.push_term(t, &span);
   }
   Ok(term)
}

pub fn ll1_term(tlc: &mut TLC, scope: ScopeId, tokens: &mut Vec<Token>) -> Result<TermId,Error> {
   if peek_is(tokens, &vec![Symbol::If]) {
      ll1_if_term(tlc, scope, tokens)
   } else {
      ll1_as_term(tlc, scope, tokens)
   }
}

pub fn ll1_stmt(tlc: &mut TLC, scope: ScopeId, tokens: &mut Vec<Token>) -> Result<TermId,Error> {
   if peek_is(tokens, &vec![Symbol::LeftBrace]) {
      ll1_block_stmt(tlc, scope, tokens)
   } else if peek_is(tokens, &vec![Symbol::Type]) {
      ll1_type_stmt(tlc, scope, tokens)
   } else if peek_is(tokens, &vec![Symbol::Forall]) {
      ll1_forall_stmt(tlc, scope, tokens)
   } else if peek_is(tokens, &vec![Symbol::Let]) {
      ll1_let_stmt(tlc, scope, tokens)
   } else {
      ll1_term(tlc, scope, tokens)
   }
}

pub fn ll1_block_stmt(tlc: &mut TLC, scope: ScopeId, tokens: &mut Vec<Token>) -> Result<TermId,Error> {
   let scope = tlc.push_scope(Scope {
      parent: Some(scope),
      children: Vec::new(),
   }, &span_of(tokens));

   pop_is("block", tokens, &vec![Symbol::LeftBrace])?;
   let mut es = Vec::new();
   while !peek_is(tokens, &vec![Symbol::RightBrace]) {
      es.push( ll1_stmt(tlc, scope, tokens)? );
      while peek_is(tokens, &vec![Symbol::SemiColon]) {
         pop_is("block", tokens, &vec![Symbol::SemiColon])?;
      }
   }
   pop_is("block", tokens, &vec![Symbol::RightBrace])?;

   Ok(tlc.push_term(Term::Block(scope,es), &span_of(tokens)))
}

pub fn ll1_file(tlc: &mut TLC, scope: ScopeId, tokens: &mut Vec<Token>) -> Result<TermId,Error> {
   let mut es = Vec::new();
   while !peek_is(tokens, &vec![Symbol::EOF]) {
      es.push( ll1_stmt(tlc, scope, tokens)? );
      while peek_is(tokens, &vec![Symbol::SemiColon]) {
         pop_is("file", tokens, &vec![Symbol::SemiColon])?;
      }
   }
   pop_is("file", tokens, &vec![Symbol::EOF])?;
   Ok(tlc.push_term(Term::Block(scope,es), &span_of(tokens)))
}

