use std::collections::{HashMap};
use crate::term::{Term,TermId};
use crate::debug::{Error};
use crate::token::{Token,Symbol,span_of};
use crate::scope::{ScopeId,Scope};
use crate::tlc::{TLC};
use crate::typ::{Type};
use crate::kind::{Kind};

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

pub fn ll1_typ_stmt(_tlc: &mut TLC, _scope: ScopeId, _tokens: &mut Vec<Token>) -> Result<TermId,Error> {
   todo!("implement ll1_typ_stmt")
}

pub fn ll1_forall_stmt(_tlc: &mut TLC, _scope: ScopeId, _tokens: &mut Vec<Token>) -> Result<TermId,Error> {
   todo!("implement ll1_forall_stmt")
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
            kind = ll1_kind(tlc, scope, tokens)?;
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
      rk = ll1_kind(tlc, scope, tokens)?;
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
   pop_is("ident-type", tokens, &vec![Symbol::LessThan])?;
   while !peek_is(tokens, &vec![Symbol::GreaterThan]) {
      if peek_is(tokens, &vec![Symbol::Comma]) {
         pop_is("ident-type", tokens, &vec![Symbol::Comma])?;
      }
      tps.push( ll1_type(tlc, dept, scope, tokens)? );
   }
   pop_is("ident-type", tokens, &vec![Symbol::GreaterThan])?;
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

pub fn ll1_kind(tlc: &mut TLC, scope: ScopeId, tokens: &mut Vec<Token>) -> Result<Kind,Error> {
   todo!("implement kind")
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
      ll1_typ_stmt(tlc, scope, tokens)
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

