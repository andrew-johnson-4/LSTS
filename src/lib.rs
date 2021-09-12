use std::collections::HashMap;
use std::collections::hash_map::DefaultHasher;
use std::hash::{Hash, Hasher};
use std::sync::atomic::{AtomicUsize, Ordering};
use std::rc::Rc;

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

#[derive(Clone)]
pub struct Typefun {
   f: Rc<dyn Fn(Vec<Box<Type>>) -> Box<Type>>
}
impl Hash for Typefun {
   fn hash<H: Hasher>(&self, state: &mut H) {
      std::ptr::hash(&*(self.f), state)
   }
}

#[derive(Hash,Clone)]
pub enum Type {
   True,
   False,
   Ground(String),
   Param(String,Vec<Box<Type>>),
   Var(String),
   Arrow(Box<Type>,Box<Type>),
   Or(Vec<Box<Type>>),
   Ascript(String,Box<Type>),
   Sub(String,Box<Type>),
   ForAll(usize,Vec<String>,Box<Type>),
   Exists(usize,Vec<String>,Box<Type>),
   End(usize),
   Typedef(String,Box<Type>),
   Typefun(String,Typefun),
   Typecall(String,Vec<Box<Type>>),
   Eq(Box<Type>,Box<Type>),
}
impl std::fmt::Display for Type {
   fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
      match self {
         Type::True => write!(f, "T"),
         Type::False => write!(f, "F"),
         Type::Ground(g) => write!(f, "{}", g),
         Type::Param(g,gs) => write!(f, "{}<{}>", g, gs.iter().map(|s| s.to_string()).collect::<Vec<String>>().join(",") ),
         Type::Var(v) => write!(f, "'{}", v),
         Type::Arrow(l,r) => write!(f, "{} -> {}", l, r), //TODO disambiguate nesting of arrows
         Type::Or(os) => write!(f, "{}", os.iter().map(|o| o.to_string()).collect::<Vec<String>>().join(" | ")),
         Type::Ascript(l,r) => write!(f, "{}:{}", l, r),
         Type::Sub(_v,s) => write!(f, "{}", s),
         Type::ForAll(_,vs,tt) => write!(f, "forall {}. {}", vs.iter().map(|v| format!("'{}",v)).collect::<Vec<String>>().join(","), tt),
         Type::Exists(_,vs,tt) => write!(f, "exists {}. {}", vs.iter().map(|v| format!("'{}",v)).collect::<Vec<String>>().join(","), tt),
         Type::End(t) => write!(f, "end {}", t),
         Type::Typedef(t,b) => write!(f, "{} = {}", t, b),
         Type::Typefun(n,_) => write!(f, "\\{}", n),
         Type::Typecall(n,ps) => write!(f, "\\{}({})", n, ps.iter().map(|p| p.to_string()).collect::<Vec<String>>().join(",")),
         Type::Eq(l,r) => write!(f, "{} == {}", l, r),
      }
   }
}
pub fn ttrue() -> Type {
   Type::True
}
pub fn tfalse() -> Type {
   Type::False
}
pub fn ground(s: &str) -> Type {
   Type::Ground(s.to_string())
}
pub fn param<I>(g: &str, gs: I) -> Type
where
    I: IntoIterator<Item = Type>,
{
    Type::Param(g.to_string(), gs.into_iter().map(|s| Box::new(s)).collect::<Vec<Box<Type>>>())
}
pub fn or<I>(os: I) -> Type
where
    I: IntoIterator<Item = Type>,
{
    Type::Or(os.into_iter().map(|s| Box::new(s)).collect::<Vec<Box<Type>>>())
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
pub fn forall<I,S>(scope: &mut usize, vs: I, tt: Type) -> Type
where
    S: Into<String>,
    I: IntoIterator<Item = S>,
{
   *scope = unique_ordinal();
   Type::ForAll(*scope, vs.into_iter().map(|s| s.into()).collect::<Vec<String>>(), Box::new(tt))
}
pub fn exists<I,S>(scope: &mut usize, vs: I, tt: Type) -> Type
where
    S: Into<String>,
    I: IntoIterator<Item = S>,
{
   *scope = unique_ordinal();
   Type::Exists(*scope, vs.into_iter().map(|s| s.into()).collect::<Vec<String>>(), Box::new(tt))
}
pub fn typecall<A>(f: &str, vs: A) -> Type
where
    A: IntoIterator<Item = Type>,
{
   Type::Typecall(f.to_string(), vs.into_iter().map(|t| Box::new(t)).collect::<Vec<Box<Type>>>())
}
pub fn end(scope: usize) -> Type {
   Type::End(scope)
}
pub fn typedef(n: &str, t: Type) -> Type {
   Type::Typedef(n.to_string(), Box::new(t))
}
pub fn typefun<F>(n: &str, f: F) -> Type
where
    F: 'static + Fn(Vec<Box<Type>>) -> Box<Type>
{
   Type::Typefun(n.to_string(), Typefun { f:Rc::new(f) })
}
pub fn eq(l: Type, r: Type) -> Type {
   Type::Eq(Box::new(l), Box::new(r))
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
