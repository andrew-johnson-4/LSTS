use crate::typ::Type;
use crate::kind::Kind;

#[derive(Clone, Copy)]
pub struct ScopeId {
   pub id: usize,
}

//does not implement Clone because scopes are uniquely identified by their id
pub struct Scope {
   pub parent: Option<ScopeId>,
   pub children: Vec<(String,Vec<(Type,Kind)>,Type)>,
}
