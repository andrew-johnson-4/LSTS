use crate::tlc::TLC;
use l1_ir::value::{Tag,Value};

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
   pub fn from_value(v: Value) -> Constant {
      match v.tag() {
         Tag::Unit => Constant::Tuple(Vec::new()),
         Tag::U8 => Constant::Literal(format!("{:?}",v)),
         Tag::U64 => Constant::Literal(format!("{:?}",v)),
         Tag::I64 => Constant::Literal(format!("{:?}",v)),
         Tag::Tuple => Constant::Literal(format!("{:?}",v)),
         Tag::String => Constant::Literal(format!("{:?}",v)),
         t => unimplemented!("Constant::from_value Tag: {:?}", t)
      }
   }
}
