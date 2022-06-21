
#[derive(Clone,Eq,PartialEq,Ord,PartialOrd,Hash)]
pub enum Kind {
   Nil,
   Simple(String,Vec<Kind>),
   And(Vec<Kind>),
}

impl std::fmt::Debug for Kind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
           Kind::Nil => {
              write!(f, "Nil")
           },
           Kind::Simple(k,ps) => {
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
   pub fn has(&self, other: &Kind) -> bool {
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
}

