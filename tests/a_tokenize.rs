use lsts::tlc::TLC;
use lsts::token::{Symbol,tokenize_string};

#[test]
fn tokenize_literals() {
   let mut tlc = TLC::new();
   let mut tks = tokenize_string(&mut tlc, "[string]", r#"f"abc{d}{e:F}gh""#).unwrap();
   assert_eq!( tks.take().unwrap().unwrap().symbol, Symbol::Literal );
   assert_eq!( tks.take().unwrap().unwrap().symbol, Symbol::LiteralS("abc".to_string()) );
   assert_eq!( tks.take().unwrap().unwrap().symbol, Symbol::LeftBrace );
   assert_eq!( tks.take().unwrap().unwrap().symbol, Symbol::Ident("d".to_string()) );
   assert_eq!( tks.take().unwrap().unwrap().symbol, Symbol::RightBrace );
   assert_eq!( tks.take().unwrap().unwrap().symbol, Symbol::LeftBrace );
   assert_eq!( tks.take().unwrap().unwrap().symbol, Symbol::Ident("e".to_string()) );
   assert_eq!( tks.take().unwrap().unwrap().symbol, Symbol::Ascript );
   assert_eq!( tks.take().unwrap().unwrap().symbol, Symbol::Typename("F".to_string()) );
   assert_eq!( tks.take().unwrap().unwrap().symbol, Symbol::RightBrace );
   assert_eq!( tks.take().unwrap().unwrap().symbol, Symbol::LiteralS("gh".to_string()) );
   assert_eq!( tks.take().unwrap().unwrap().symbol, Symbol::Literal );
   assert_eq!( tks.take().unwrap().unwrap().symbol, Symbol::EOF );
}
