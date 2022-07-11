use crate::term::{Term,TermId};
use crate::debug::{Error};
use crate::token::{Token,Symbol,span_of};
use crate::scope::{ScopeId,Scope};
use crate::tlc::{TLC};

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
pub fn ll1_term(_tlc: &mut TLC, _scope: ScopeId, _tokens: &mut Vec<Token>) -> Result<TermId,Error> {
   todo!("implement ll1_term")
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

