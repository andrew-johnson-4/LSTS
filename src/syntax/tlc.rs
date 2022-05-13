pub struct TLC;

pub struct ParseError {
   filename: String,
   start: (usize,usize),
   end: (usize,usize)
}
pub enum Expr {
   Empty,
}

impl TLC {
   pub fn parse(src:&str) -> Result<Expr,ParseError> {
      Ok(Expr::Empty)
   }
}
