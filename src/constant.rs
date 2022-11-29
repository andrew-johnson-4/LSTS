use crate::tlc::TLC;

#[derive(Clone,Eq,PartialEq,Ord,PartialOrd,Hash)]
pub enum Constant {
   NaN,
   Boolean(bool),
   Integer(i64),
   Op(String),
   Tuple(Vec<Constant>),
}

impl std::fmt::Debug for Constant {
   fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
      match self {
        Constant::NaN => write!(f, "NaN"),
        Constant::Boolean(c) => write!(f, "{}", if *c {"True"} else {"False"}),
        Constant::Integer(i) => write!(f, "{}", i),
        Constant::Op(op) => write!(f, "{}", op),
        Constant::Tuple(ts) => write!(f, "({})", ts.iter()
           .map(|t|format!("{:?}",t)).collect::<Vec<String>>()
           .join(",") ),
      }
   }
}

impl Constant {
   pub fn parse(tlc: &TLC, v: &str) -> Constant {
      unimplemented!("Constant::parse({})", v)
   }
}
