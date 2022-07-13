/*
use lsts::token::{Symbol,tokenize_file};

#[test]
fn check_file_tokens() {
   let mut tks = tokenize_file("preludes/si.tlc").unwrap();
   while let Ok(Some(t)) = tks.peek() {
      eprintln!("{:?} {} {},{}", t.symbol, t.span.filename, t.span.linecol_start.0, t.span.linecol_start.1);
      if let Ok(Some(t)) = tks.take() {
         eprintln!("{:?} {} {},{}", t.symbol, t.span.filename, t.span.linecol_start.0, t.span.linecol_start.1);
      }
      if t.symbol == Symbol::EOF { break; }
   }
   assert!(false);
}
*/

