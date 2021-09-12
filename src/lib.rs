#![feature(box_patterns)]
use std::collections::HashMap;
use std::collections::hash_map::DefaultHasher;
use std::hash::{Hash, Hasher};
use std::sync::atomic::{AtomicUsize, Ordering};
use std::rc::Rc;

pub struct Judgements {
   pub lines: Vec<Box<Type>>
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
      let mut fresh = true;
      while fresh {
         fresh = false;

         //gather type ascriptions to be substituted
         //gather type functions to be applied
         let mut ascripts: HashMap<String,Box<Type>> = HashMap::new();
         let mut typefuns: HashMap<String,Typefun> = HashMap::new();
         for l in self.lines.iter() {
            match l {
               box Type::Ascript(v,vt) => {
                  ascripts.insert(v.to_string(), vt.clone());
               }, box Type::Typefun(v,vt) => {
                  typefuns.insert(v.to_string(), vt.clone());
               }, _ => {}
            }
         }

         //substitute type ascriptions over variables
         for li in 0..self.lines.len() {
            let (normalized, changed, result) = Type::normalize(&self.lines[li], &ascripts, &typefuns);
            if normalized {
               if changed {
                  fresh = true;
               }
               self.lines[li] = result;
            }
         }
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
   Ground(&'static str),
   Param(&'static str,Vec<Box<Type>>),
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
impl Type {
   pub fn normalize(tt: &Box<Type>,
                    ascripts: &HashMap<String,Box<Type>>,
                    typefuns: &HashMap<String,Typefun>) -> (bool, bool, Box<Type>) {
      match tt {
         box Type::True |
         box Type::False | 
         box Type::Ground(_) => (true, false, tt.clone()),
         box Type::Arrow(l,r) => {
            let (ln,lc,lt) = Type::normalize(&l, ascripts, typefuns);
            let (rn,rc,rt) = Type::normalize(&r, ascripts, typefuns);
            (ln&&rn, lc||rc, arrow(lt,rt))
         }
         box Type::Var(v) => {
            if let Some(vt) = ascripts.get(v) {
              Type::normalize(&vt, ascripts, typefuns)
            } else {
              (false, false, tt.clone())
            }
         },
         _ => (false, false, tt.clone()),
      }
   }
}

pub fn ttrue() -> Box<Type> {
   Box::new(Type::True)
}
pub fn tfalse() -> Box<Type> {
   Box::new(Type::False)
}
pub fn ground(s: &'static str) -> Box<Type> {
   Box::new(Type::Ground(s))
}
pub fn param<I>(g: &'static str, gs: I) -> Box<Type>
where
    I: IntoIterator<Item = Box<Type>>,
{
    Box::new(Type::Param(g, gs.into_iter().collect::<Vec<Box<Type>>>()))
}
pub fn or<I>(os: I) -> Box<Type>
where
    I: IntoIterator<Item = Box<Type>>,
{
    Box::new(Type::Or(os.into_iter().collect::<Vec<Box<Type>>>()))
}
pub fn var(s: &str) -> Box<Type> {
   Box::new(Type::Var(s.to_string()))
}
//create a unique number
pub fn unique_ordinal() -> usize {
   static COUNTER: AtomicUsize = AtomicUsize::new(0);
   COUNTER.fetch_add(1, Ordering::Relaxed)
}
pub fn uvar() -> Box<Type> {
   Box::new(Type::Var(format!("uvar_{}", unique_ordinal())))
}
pub fn arrow(l: Box<Type>, r: Box<Type>) -> Box<Type> {
   Box::new(Type::Arrow(l,r))
}
pub fn ascript(l: &str, r: Box<Type>) -> Box<Type> {
   Box::new(Type::Ascript(l.to_string(),r))
}
pub fn forall<I,S>(scope: &mut usize, vs: I, tt: Box<Type>) -> Box<Type>
where
    S: Into<String>,
    I: IntoIterator<Item = S>,
{
   *scope = unique_ordinal();
   Box::new(Type::ForAll(*scope, vs.into_iter().map(|s| s.into()).collect::<Vec<String>>(), tt))
}
pub fn exists<I,S>(scope: &mut usize, vs: I, tt: Box<Type>) -> Box<Type>
where
    S: Into<String>,
    I: IntoIterator<Item = S>,
{
   *scope = unique_ordinal();
   Box::new(Type::Exists(*scope, vs.into_iter().map(|s| s.into()).collect::<Vec<String>>(), tt))
}
pub fn typecall<A>(f: &str, vs: A) -> Box<Type>
where
    A: IntoIterator<Item = Box<Type>>,
{
   Box::new(Type::Typecall(f.to_string(), vs.into_iter().collect::<Vec<Box<Type>>>()))
}
pub fn end(scope: usize) -> Box<Type> {
   Box::new(Type::End(scope))
}
pub fn typedef(n: &str, t: Box<Type>) -> Box<Type> {
   Box::new(Type::Typedef(n.to_string(), t))
}
pub fn typefun<F>(n: &str, f: F) -> Box<Type>
where
    F: 'static + Fn(Vec<Box<Type>>) -> Box<Type>
{
   Box::new(Type::Typefun(n.to_string(), Typefun { f:Rc::new(f) }))
}
pub fn eq(l: Box<Type>, r: Box<Type>) -> Box<Type> {
   Box::new(Type::Eq(l, r))
}

pub fn declare<I>(ds: I) -> Judgements
where
    I: IntoIterator<Item = Box<Type>>,
{
   let mut ts = Vec::new();
   for d in ds.into_iter() {
      ts.push(d);
   }
   Judgements { lines:ts }
}
