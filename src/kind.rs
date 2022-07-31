
/// All Kinds are Named Strings with optional Parameters.
#[derive(Clone,Eq,PartialEq,Ord,PartialOrd,Hash)]
pub enum Kind {
   Nil,
   Named(String,Vec<Kind>),
   And(Vec<Kind>),
}

impl std::fmt::Debug for Kind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
           Kind::Nil => {
              write!(f, "Nil")
           },
           Kind::Named(k,ps) => {
              if ps.len()==0 { write!(f, "{}", k) }
              else { write!(f, "{}<{:?}>", k, ps.iter().map(|p|format!("{:?}",p)).collect::<Vec<String>>().join(",")) }
           },
           Kind::And(ks) => {
              write!(f, "{}",
                 ks.iter().map(|k| format!("{:?}",k))
                   .collect::<Vec<String>>().join(" + "))
           },
        }
    }
}

impl Kind {
   pub fn and(mut ks: Vec<Kind>) -> Kind {
      ks.retain(|k| k != &Kind::Nil);
      ks.sort();
      ks.dedup();
      if ks.len()==0 { Kind::Nil }
      else if ks.len()==1 { ks[0].clone() }
      else { Kind::And(ks) }
   }
   pub fn has(&self, other: &Kind) -> bool {
      if other == &Kind::Nil { return true; }
      let ls = self.flatten();
      let rs = other.flatten();
      rs.iter().all(|rk| ls.contains(rk))
   }
   pub fn flatten(&self) -> Vec<Kind> {
      match self {
         Kind::Nil => vec![],
         Kind::And(ks) => ks.clone(),
         _ => vec![self.clone()],
      }
   }
   pub fn first(&self) -> Kind {
      match self {
         Kind::And(ks) => ks[0].clone(),
         _ => self.clone()
      }
   }
}

