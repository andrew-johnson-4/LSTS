use std::collections::HashMap;
use std::collections::hash_map::DefaultHasher;
use std::hash::{Hash, Hasher};
use std::sync::atomic::{AtomicUsize, Ordering};

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
         let mut ts = self.lines.clone();

         //gather type ascriptions to be substituted
         let mut ascripts: HashMap<String,Box<Type>> = HashMap::new();
         for l in self.lines.iter() {
            match l {
               Type::Ascript(v,vt) => {
                  ascripts.insert(v.to_string(), vt.clone());
               }, _ => {}
            }
         }

         //substitute type ascriptions over variables
         for (li,l) in self.lines.iter().enumerate() {
            match l {
               Type::Var(v) => {
                  if let Some(vt) = ascripts.get(v) {
                     ts[li] = Type::Sub(v.to_string(),vt.clone());
                  }
               }, _ => {}
            }
         }

         //recalculate hash
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
   Arrow(Box<Type>,Box<Type>),
   Ascript(String,Box<Type>),
   Sub(String,Box<Type>),
   ForAll(usize,Vec<String>),
   Exists(usize,Vec<String>),
   End(usize),
}
impl std::fmt::Display for Type {
   fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
      match self {
         Type::Ground(g) => write!(f, "{}", g),
         Type::Var(v) => write!(f, "'{}", v),
         Type::Arrow(l,r) => write!(f, "{} -> {}", l, r), //TODO disambiguate nesting of arrows
         Type::Ascript(l,r) => write!(f, "{}:{}", l, r),
         Type::Sub(_v,s) => write!(f, "{}", s),
         Type::ForAll(_,vs) => write!(f, "forall {}", vs.iter().map(|v| format!("'{}",v)).collect::<Vec<String>>().join(",")),
         Type::Exists(_,vs) => write!(f, "exists {}", vs.iter().map(|v| format!("'{}",v)).collect::<Vec<String>>().join(",")),
         Type::End(t) => write!(f, "end {}", t),
      }
   }
}
pub fn ground(s: &str) -> Type {
   Type::Ground(s.to_string())
}
pub fn var(s: &str) -> Type {
   Type::Var(s.to_string())
}
//create a unique number
pub fn unique_ordinal() -> usize {
   static COUNTER: AtomicUsize = AtomicUsize::new(0);
   COUNTER.fetch_add(1, Ordering::Relaxed)
}
pub fn uvar() -> Type {
   Type::Var(format!("uvar_{}", unique_ordinal()))
}
pub fn arrow(l: Type, r: Type) -> Type {
   Type::Arrow(Box::new(l),Box::new(r))
}
pub fn ascript(l: &str, r: Type) -> Type {
   Type::Ascript(l.to_string(),Box::new(r))
}
pub fn forall<I,S>(scope: &mut usize, vs: I) -> Type
where
    S: Into<String>,
    I: IntoIterator<Item = S>,
{
   *scope = unique_ordinal();
   Type::ForAll(*scope, vs.into_iter().map(|s| s.into()).collect::<Vec<String>>())
}
pub fn exists<I,S>(scope: &mut usize, vs: I) -> Type
where
    S: Into<String>,
    I: IntoIterator<Item = S>,
{
   *scope = unique_ordinal();
   Type::Exists(*scope, vs.into_iter().map(|s| s.into()).collect::<Vec<String>>())
}
pub fn end(scope: usize) -> Type {
   Type::End(scope)
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
