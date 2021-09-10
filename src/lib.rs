pub struct Judgements {
   pub lines: Vec<Type>
}
impl Judgements {
   pub fn normalize(self) -> Judgements {
      self
   }
}

pub enum Type {
   Ground(String)
}
impl std::fmt::Display for Type {
   fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
      match self {
         Type::Ground(g) => write!(f, "{}", g)
      }
   }
}
pub fn ground(s: &str) -> Type {
   Type::Ground(s.to_string())
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
