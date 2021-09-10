use std::collections::hash_map::DefaultHasher;
use std::hash::{Hash, Hasher};

pub struct Judgements {
   pub lines: Vec<Type>
}
impl Judgements {
   pub fn hash(&self) -> u64 {
      let mut hasher = DefaultHasher::new();
      self.lines.hash(&mut hasher);
      hasher.finish()
   }
   pub fn zero() -> Judgements {
      Judgements {
         lines: Vec::new()
      }
   }
   pub fn normalize(mut self) -> Judgements {
      let mut last_hash = Judgements::zero().hash();
      let mut this_hash = self.hash();
      while last_hash != this_hash {
         let ts = self.lines.clone();
         for l in self.lines.iter() {
         }
         self.lines = ts;
         last_hash = this_hash;
         this_hash = self.hash();
      }
      self
   }
}

#[derive(Hash,Clone)]
pub enum Type {
   Ground(String),
   Var(String),
   Ascript(Box<Type>,Box<Type>),
   Sub(String,Box<Type>),
}
impl std::fmt::Display for Type {
   fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
      match self {
         Type::Ground(g) => write!(f, "{}", g),
         Type::Var(v) => write!(f, "'{}", v),
         Type::Ascript(l,r) => write!(f, "{}:{}", l, r),
         Type::Sub(_v,s) => write!(f, "{}", s),
      }
   }
}
pub fn ground(s: &str) -> Type {
   Type::Ground(s.to_string())
}
pub fn var(s: &str) -> Type {
   Type::Var(s.to_string())
}
pub fn ascript(l: Type, r: Type) -> Type {
   Type::Ascript(Box::new(l),Box::new(r))
}

pub fn declare<I>(ds: I) -> Judgements
where
    I: IntoIterator<Item = Type>,
{
   let mut ts = Vec::new();
   for d in ds.into_iter() {
      ts.push(d);
   }
   Judgements { lines:ts }
}
