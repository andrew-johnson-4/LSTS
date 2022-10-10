use std::collections::HashMap;
use crate::typ::Type;
use crate::kind::Kind;
use crate::term::TermId;

#[derive(Clone, Copy, Debug)]
pub struct ScopeId {
   pub id: usize,
}

#[derive(Clone)]
pub struct Scope {
   pub parent: Option<ScopeId>,
   pub children: Vec<(String,HashMap<Type,Kind>,Type,Option<TermId>)>,
}
