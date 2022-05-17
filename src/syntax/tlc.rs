use pest::Parser;

#[derive(Parser)]
#[grammar = "tlc.pest"]
struct TlcParser;

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
      let pr = TlcParser::parse(Rule::ident_list, src);
      match pr {
        Ok(_) => Ok(Expr::Empty),
        Err(pe) => Err(ParseError { filename:"[string]".to_string(), start:(0,0), end:(0,0) })
      } 
   }
}
