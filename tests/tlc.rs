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
   tlc.check("(a: A->B)(b: A)").unwrap();
   tlc.check("(a: A->B)(b: B)").unwrap_err();
}
