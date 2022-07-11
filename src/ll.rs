use crate::term::{Term,TermId};
use crate::debug::{Error};
use crate::token::{Token,span_of};
use crate::scope::{ScopeId};
use crate::tlc::{TLC};

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
