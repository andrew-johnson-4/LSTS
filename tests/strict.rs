use lsts::tlc::TLC;

#[test]
fn parse_simplytyped() {
   let mut tlc = TLC::new();
   assert!( tlc.parse("type A;").is_ok() );
   assert!( tlc.parse("type normal A;").is_err() );
}
