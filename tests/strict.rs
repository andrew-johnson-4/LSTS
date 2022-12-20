use lsts::tlc::TLC;

#[test]
fn parse_simplytyped() {
   let mut tlc = TLC::new();
   assert!( tlc.parse_str(None,"type A;").is_ok() );
   assert!( tlc.parse_str(None,"type normal A;").is_err() );
}
