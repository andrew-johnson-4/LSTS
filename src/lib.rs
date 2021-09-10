pub struct Judgements {
   pub lines: Vec<Type>
}
impl Judgements {
   pub fn normalize(self) -> Judgements {
      self
   }
}

pub enum Type {
   Ground(String),
   Var(String),
   Ascript(Box<Type>,Box<Type>),
}
impl std::fmt::Display for Type {
   fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
      match self {
         Type::Ground(g) => write!(f, "{}", g),
         Type::Var(v) => write!(f, "'{}", v),
         Type::Ascript(l,r) => write!(f, "{}:{}", l, r),
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
