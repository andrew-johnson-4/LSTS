use crate::tlc::TLC;

use lambda_mountain::Rhs;

#[derive(Clone,Eq,PartialEq,Ord,PartialOrd,Hash)]
pub enum Constant {
   Literal(String),
   Tuple(Vec<Constant>),
}

impl std::fmt::Debug for Constant {
   fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
      match self {
        Constant::Literal(s) => write!(f, "{}", s),
        Constant::Tuple(ts) => write!(f, "({})", ts.iter()
           .map(|t|format!("{:?}",t)).collect::<Vec<String>>()
           .join(",") ),
      }
   }
}

impl Constant {
   pub fn parse(_tlc: &TLC, v: &str) -> Option<Constant> {
      Some(Constant::Literal(v.to_string()))
   }
   pub fn from_value(v: Rhs) -> Constant {
      match v {
         Rhs::App(vs) => Constant::Tuple(vs.iter().map(|v| Constant::from_value(v.clone())).collect::<Vec<Constant>>()),
         Rhs::Variable(l) => Constant::Literal(l.clone()),
         Rhs::Literal(l) => Constant::Literal(l.clone()),
         t => unimplemented!("Constant::from_value {}", t)
      }
   }
}
