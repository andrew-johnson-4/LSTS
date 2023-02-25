use lsts::tlc::TLC;

#[test]
fn parse_simplytyped() {
   let mut tlc = TLC::new().strict();
   assert!( tlc.parse_str(None,"type Aa;").is_ok() );
   //Term kinds cannot be normal
   assert!( tlc.parse_str(None,"type normal Aa;").is_err() );
}
