use lsts::syntax::TLC;

fn rust_simplytyped() {
   assert!(TLC::parse("let t: T").is_ok());
   assert!(TLC::parse("let t: 1").is_ok());
   assert!(TLC::parse("let t: T<1,2>").is_ok());
   assert!(TLC::parse("let t: [1]").is_ok());
   assert!(TLC::parse("let t: 1->2").is_ok());
}
