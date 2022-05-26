use lsts::syntax::tlc::TLC;

#[test]
fn parse_simplytyped() {
   let mut tlc = TLC::new();
   tlc.check(None, "a").unwrap();
   tlc.check(None, "a()").unwrap();
   tlc.check(None, "a(b)").unwrap();
   tlc.check(None, "a(b,c)").unwrap();
   tlc.check(None, "let t: ?").unwrap();
   tlc.check(None, "let t: T").unwrap();
   tlc.check(None, "let t: 1").unwrap();
   tlc.check(None, "let t: ()").unwrap();
   tlc.check(None, "let t: (A)").unwrap();
   tlc.check(None, "let t: (A,B)").unwrap();
   tlc.check(None, "let t: T<1,2>").unwrap();
   tlc.check(None, "let t: ?[1]").unwrap();
   tlc.check(None, "let t: ()->A").unwrap();
   tlc.check(None, "let t: A->B").unwrap();
   tlc.check(None, "let t: (A)->B").unwrap();
   tlc.check(None, "let t: (A,B)->C").unwrap();
   tlc.check(None, "let t: A|B").unwrap();
   tlc.check(None, "let a: A; let a: B").unwrap();
   tlc.check(None, "let a: A; let b: A").unwrap();
}

#[test]
fn check_simplytyped() {
   let mut tlc = TLC::new();
   let global_scope = tlc.load_file(None, "tests/tlc/prelude.tlc").unwrap();
   tlc.check(Some(global_scope), "let a: A->B; let b: A; a(b)").unwrap();
   tlc.check(Some(global_scope), "let a: A->B; let b: B; a(b)").unwrap_err();
}
