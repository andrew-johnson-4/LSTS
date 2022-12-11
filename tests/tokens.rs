use lsts::tlc::TLC;
use lsts::token::{Symbol,tokenize_string};

#[test]
fn tokenize_literals() {
   let mut tlc = TLC::new();
   let mut tks = tokenize_string(&mut tlc, "[string]", r#"literal 'a' 'b'b "c" "d"d [e-f] [g-h]gh i"#).unwrap();
   assert!( tks.take().unwrap().unwrap().symbol == Symbol::Literal );
   assert!( tks.take().unwrap().unwrap().symbol == Symbol::LiteralC('a',"".to_string()) );
   assert!( tks.take().unwrap().unwrap().symbol == Symbol::LiteralC('b',"b".to_string()) );
   assert!( tks.take().unwrap().unwrap().symbol == Symbol::LiteralS("c".to_string(),"".to_string()) );
   assert!( tks.take().unwrap().unwrap().symbol == Symbol::LiteralS("d".to_string(),"d".to_string()) );
   assert!( tks.take().unwrap().unwrap().symbol == Symbol::LiteralR(vec![('e','f')],"".to_string()) );
   assert!( tks.take().unwrap().unwrap().symbol == Symbol::LiteralR(vec![('g','h')],"gh".to_string()) );
   assert!( tks.take().unwrap().unwrap().symbol == Symbol::LiteralV("i".to_string()) );
   assert!( tks.take().unwrap().unwrap().symbol == Symbol::EOF );
}
