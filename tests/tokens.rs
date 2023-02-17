/* TODO FIXME use new literal syntax
use lsts::tlc::TLC;
use lsts::token::{Symbol,tokenize_string};

#[test]
fn tokenize_literals() {
   let mut tlc = TLC::new();
   let mut tks = tokenize_string(&mut tlc, "[string]", r#"literal 'a' 'b'bs "c" "d"ds [e-f] [g-h]gh is,"#).unwrap();
   assert!( tks.take().unwrap().unwrap().symbol == Symbol::Literal );
   assert!( tks.take().unwrap().unwrap().symbol == Symbol::LiteralC('a',"".to_string()) );
   assert!( tks.take().unwrap().unwrap().symbol == Symbol::LiteralC('b',"bs".to_string()) );
   assert!( tks.take().unwrap().unwrap().symbol == Symbol::LiteralS("c".to_string(),"".to_string()) );
   assert!( tks.take().unwrap().unwrap().symbol == Symbol::LiteralS("d".to_string(),"ds".to_string()) );
   assert!( tks.take().unwrap().unwrap().symbol == Symbol::LiteralR(vec![('e','f')],"".to_string()) );
   assert!( tks.take().unwrap().unwrap().symbol == Symbol::LiteralR(vec![('g','h')],"gh".to_string()) );
   assert!( tks.take().unwrap().unwrap().symbol == Symbol::LiteralV("is".to_string()) );
   assert!( tks.take().unwrap().unwrap().symbol == Symbol::Comma );
   assert!( tks.take().unwrap().unwrap().symbol == Symbol::EOF );
}
*/
