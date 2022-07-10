use crate::debug::Error;
use std::rc::Rc;

#[derive(Clone)]
pub struct Span {
   pub filename: Rc<String>,
   pub offset_start: usize,
   pub offset_end: usize,
   pub linecol_start: (usize,usize),
   pub linecol_end: (usize,usize),
}

pub fn span_of(ts: &Vec<Token>) -> Span {
   if ts.len()==0 {
      Span {
         filename: Rc::new("NULL String".to_string()),
         offset_start: 0,
         offset_end: 0,
         linecol_start: (0,0),
         linecol_end: (0,0),
      }
   } else {
      Span {
         filename: ts[0].span.filename.clone(),
         offset_start: ts[0].span.offset_start,
         offset_end: ts[ts.len()-1].span.offset_end,
         linecol_start: ts[0].span.linecol_start.clone(),
         linecol_end: ts[ts.len()-1].span.linecol_end.clone(),
      }
   }
}

pub struct Token {
   pub symbol: Symbol,
   pub span: Span,
}

pub enum Symbol {
   Equal,
   NotEqual,
   GreaterThan,
   GreaterThanOrEqual,
   LessThan,
   LessThanOrEqual,
}

pub fn tokenize(source_name:String, source: &str) -> Result<Vec<Token>,Error> {
   let filename = Rc::new(source_name);
   let mut source_index = 0;
   Err(Error{
      kind: "Tokenization Error".to_string(),
      rule: format!("Unexpected Character '{}'", if source_index<source.len() 
           { (source.as_bytes()[source_index] as char).to_string() }
      else {"EOF".to_string()} ),
      span: Span {
         filename: filename.clone(),
         offset_start: 0,
         offset_end: 0,
         linecol_start: (0,0),
         linecol_end: (0,0),
      },
   })
}
