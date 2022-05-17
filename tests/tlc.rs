use lsts::syntax::TLC;


#[test]
fn parse_simplytyped() {
   assert!(TLC::parse("let t: T").is_ok());
   assert!(TLC::parse("let t: 1").is_ok());
   assert!(TLC::parse("a()").is_ok());
   assert!(TLC::parse("a(b)").is_ok());
   assert!(TLC::parse("a(b,c)").is_ok());
   assert!(TLC::parse("let t: ()").is_ok());
   assert!(TLC::parse("let t: (A)").is_ok());
   assert!(TLC::parse("let t: (A,B)").is_ok());
   assert!(TLC::parse("let t: T<1,2>").is_ok());
   assert!(TLC::parse("let t: [1]").is_ok());
   assert!(TLC::parse("let t: ()->A").is_ok());
   assert!(TLC::parse("let t: A->B").is_ok());
   assert!(TLC::parse("let t: (A)->B").is_ok());
   assert!(TLC::parse("let t: (A,B)->B").is_ok());
   assert!(TLC::parse("let t: A|B").is_ok());
   assert!(TLC::parse("let a: A; let a: B").is_ok());
   assert!(TLC::parse("let a: A; let b: A").is_ok());
}
