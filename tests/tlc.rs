use lsts::syntax::tlc::TLC;

#[test]
fn parse_simplytyped() {
   let mut tlc = TLC::new();
   tlc.check("a").unwrap();
   tlc.check("a()").unwrap();
   tlc.check("a(b)").unwrap();
   tlc.check("a(b,c)").unwrap();
   tlc.check("let t: ?").unwrap();
   tlc.check("let t: T").unwrap();
   tlc.check("let t: 1").unwrap();
   tlc.check("let t: ()").unwrap();
   tlc.check("let t: (A)").unwrap();
   tlc.check("let t: (A,B)").unwrap();
   tlc.check("let t: T<1,2>").unwrap();
   tlc.check("let t: ?[1]").unwrap();
   tlc.check("let t: ()->A").unwrap();
   tlc.check("let t: A->B").unwrap();
   tlc.check("let t: (A)->B").unwrap();
   tlc.check("let t: (A,B)->C").unwrap();
   tlc.check("let t: A|B").unwrap();
   tlc.check("let a: A; let a: B").unwrap();
   tlc.check("let a: A; let b: A").unwrap();
}

#[test]
fn check_simplytyped() {
   let mut tlc = TLC::new();
   tlc.load_file("tests/tlc/prelude.tlc").unwrap();
   tlc.check("let a: A->B; let b: A; a(b)").unwrap();
   tlc.check("let a: A->B; let b: B; a(b)").unwrap_err();
}
