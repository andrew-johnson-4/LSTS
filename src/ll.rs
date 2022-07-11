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
   } else {
      return Err(Error {
         kind: "Parse Error".to_string(),
         rule: format!("unexpected End-Of-File in rule {}", rule),
         span: span_of(tokens),
      })
   }
   Ok(())
}

pub fn ll1_stmt(_tlc: &mut TLC, _scope: ScopeId, _tokens: &mut Vec<Token>) -> Result<TermId,Error> {
   todo!("implement ll1_stmt")
}

pub fn ll1_file(tlc: &mut TLC, scope: ScopeId, tokens: &mut Vec<Token>) -> Result<TermId,Error> {
   let mut es = Vec::new();
   while tokens.len()>0 {
      es.push( ll1_stmt(tlc, scope, tokens)? );
   }
   Ok(tlc.push_term(Term::Block(scope,es), &span_of(tokens)))
}

pub fn ll1_block(tlc: &mut TLC, scope: ScopeId, tokens: &mut Vec<Token>) -> Result<TermId,Error> {
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
