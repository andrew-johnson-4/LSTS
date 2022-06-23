use crate::typ::Type;
use crate::kind::Kind;

#[derive(Clone, Copy)]
pub struct ScopeId {
   pub id: usize,
}

#[derive(Clone)]
pub struct Scope {
   pub parent: Option<ScopeId>,
   pub children: Vec<(String,Vec<(Type,Kind)>,Type)>,
}
