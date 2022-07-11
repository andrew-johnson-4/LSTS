use std::collections::{HashMap};
use crate::term::{Term,TermId};
use crate::debug::{Error};
use crate::token::{Token,Symbol,span_of};
use crate::scope::{ScopeId,Scope};
use crate::tlc::{TLC};
use crate::typ::{Type};

fn peek_is(tokens: &mut Vec<Token>, is: &Vec<Symbol>) -> bool {
   if let Some(t) = tokens.get(0) {
      is.contains(&t.symbol)
   } else { false }
}
fn pop_is(rule: &str, tokens: &mut Vec<Token>, is: &Vec<Symbol>) -> Result<(),Error> {
   if let Some(t) = tokens.get(0) {
      if !is.contains(&t.symbol) {
         return Err(Error {
            kind: "Parse Error".to_string(),
            rule: format!("unexpected Symbol {:?} in rule {}", &t.symbol, rule),
            span: span_of(tokens),
         })
      }
   } else { //this branch should hopefully be dead code
      return Err(Error {
         kind: "Parse Error".to_string(),
         rule: format!("unexpected End-Of-File in rule {}", rule),
         span: span_of(tokens),
      })
   }
   Ok(())
}

pub fn ll1_typ_stmt(_tlc: &mut TLC, _scope: ScopeId, _tokens: &mut Vec<Token>) -> Result<TermId,Error> {
   todo!("implement ll1_typ_stmt")
}
pub fn ll1_forall_stmt(_tlc: &mut TLC, _scope: ScopeId, _tokens: &mut Vec<Token>) -> Result<TermId,Error> {
   todo!("implement ll1_forall_stmt")
}

pub fn ll1_if_term(tlc: &mut TLC, scope: ScopeId, tokens: &mut Vec<Token>) -> Result<TermId,Error> {
   todo!("implement if term")
}

pub fn ll1_expr_term(tlc: &mut TLC, scope: ScopeId, tokens: &mut Vec<Token>) -> Result<TermId,Error> {
   todo!("implement expr term")
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

pub fn ll1_type(tlc: &mut TLC, dept: &mut HashMap<String,TermId>, scope: ScopeId, tokens: &mut Vec<Token>) -> Result<Type,Error> {
   todo!("implement type")
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
   }
   pop_is("block", tokens, &vec![Symbol::RightBrace])?;

   Ok(tlc.push_term(Term::Block(scope,es), &span_of(tokens)))
}

pub fn ll1_file(tlc: &mut TLC, scope: ScopeId, tokens: &mut Vec<Token>) -> Result<TermId,Error> {
   let mut es = Vec::new();
   while !peek_is(tokens, &vec![Symbol::EOF]) {
      es.push( ll1_stmt(tlc, scope, tokens)? );
   }
   pop_is("file", tokens, &vec![Symbol::EOF])?;
   Ok(tlc.push_term(Term::Block(scope,es), &span_of(tokens)))
}

