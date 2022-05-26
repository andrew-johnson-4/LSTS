use lsts::syntax::tlc::TLC;

#[test]
fn parse_simplytyped() {
   let mut tlc = TLC::new();
   tlc.parse("a").unwrap();
   tlc.parse("a()").unwrap();
   tlc.parse("a(b)").unwrap();
   tlc.parse("a(b,c)").unwrap();
   tlc.parse("let t: ?").unwrap();
   tlc.parse("let t: T").unwrap();
   tlc.parse("let t: 1").unwrap();
   tlc.parse("let t: ()").unwrap();
   tlc.parse("let t: (A)").unwrap();
   tlc.parse("let t: (A,B)").unwrap();
   tlc.parse("let t: T<1,2>").unwrap();
   tlc.parse("let t: ?[1]").unwrap();
   tlc.parse("let t: ()->A").unwrap();
   tlc.parse("let t: A->B").unwrap();
   tlc.parse("let t: (A)->B").unwrap();
   tlc.parse("let t: (A,B)->C").unwrap();
   tlc.parse("let t: A|B").unwrap();
   tlc.parse("let a: A; let a: B").unwrap();
   tlc.parse("let a: A; let b: A").unwrap();
}

#[test]
fn check_simplytyped() {
   let mut tlc = TLC::new();
   let global_scope = tlc.load_file(None, "tests/tlc/prelude.tlc").unwrap();

   //type A is undefined
   tlc.check(Some(global_scope), "type A; let a: A").unwrap();
   tlc.check(Some(global_scope), "let a: A").unwrap_err();

   //unexpected argument B to function A -> B
   tlc.check(Some(global_scope), "type A; type B; let a: A->B; let b: A; a(b)").unwrap();
   tlc.check(Some(global_scope), "type A; type B; let a: A->B; let b: B; a(b)").unwrap_err();
}
