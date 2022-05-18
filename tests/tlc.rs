use lsts::syntax::tlc::TLC;

#[test]
fn parse_simplytyped() {
   TLC::check("a").unwrap();
   TLC::check("a()").unwrap();
   TLC::check("a(b)").unwrap();
   TLC::check("a(b,c)").unwrap();
   TLC::check("let t: T").unwrap();
   TLC::check("let t: 1").unwrap();
   TLC::check("let t: ()").unwrap();
   TLC::check("let t: (A)").unwrap();
   TLC::check("let t: (A,B)").unwrap();
   TLC::check("let t: T<1,2>").unwrap();
   TLC::check("let t: [1]").unwrap();
   TLC::check("let t: ()->A").unwrap();
   TLC::check("let t: A->B").unwrap();
   TLC::check("let t: (A)->B").unwrap();
   TLC::check("let t: (A,B)->C").unwrap();
   TLC::check("let t: A|B").unwrap();
   TLC::check("let a: A; let a: B").unwrap();
   TLC::check("let a: A; let b: A").unwrap();
}
