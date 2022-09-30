use crate::typ::Type;
use crate::kind::Kind;
use crate::scope::ScopeId;

#[derive(Clone,Copy,Eq,PartialEq,Ord,PartialOrd,Hash)]
pub struct TermId {
   pub id: usize,
}
//does not implement Clone because terms are uniquely identified by their id
#[derive(Clone)] //clone seems to be needed to deconflict mutable borrows :(
pub enum Term {
   Ident(String),
   Value(String),
   Arrow(TermId,TermId),
   App(TermId,TermId),
   Let(ScopeId,String,Vec<Vec<(Option<String>,Option<Type>,Kind)>>,Option<TermId>,Type,Kind),
   Tuple(Vec<TermId>),
   Block(ScopeId,Vec<TermId>),
   Ascript(TermId,Type),
   As(TermId,Type),
   Constructor(String,Vec<(String,TermId)>),
   Substitution(TermId,TermId,TermId),
   RuleApplication(TermId,String),
}
