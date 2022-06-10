use lsts::tlc::TLC;

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
   tlc.parse("let f()").unwrap();
   tlc.parse("let f(a)").unwrap();
   tlc.parse("let f(a: A)").unwrap();
   tlc.parse("let f(a: A::Term)").unwrap();
   tlc.parse("let f():A").unwrap();
   tlc.parse("let f()::Term").unwrap();
   tlc.parse("type A").unwrap();
   tlc.parse("forall A,B:C::D. (A,B)").unwrap();
   tlc.parse("forall A,B:C::D. (A,B) :: R").unwrap();
   tlc.parse("forall A,B:C::D. (A,B) => C :: R").unwrap();
   tlc.parse("{a; b}").unwrap();
}

#[test]
fn check_simplytyped() {
   //type A is undefined
   let mut tlc = TLC::new();
   tlc.check(None, "type A; let a: A").unwrap();

   let mut tlc = TLC::new();
   tlc.check(None, "let a: A").unwrap_err();

   //unexpected argument B to function A -> B
   let mut tlc = TLC::new();
   tlc.check(None, "type A; type B; let a: A->B; let b: A; a(b)").unwrap();

   let mut tlc = TLC::new();
   tlc.check(None, "type A; type B; let a: A->B; let b: B; a(b)").unwrap_err();
}

/*
#[test]
fn check_traitstyped() {
   //(Meter/Second) * Second = Meter
   //It should be noted that all units are rational.
   let mut tlc = TLC::new();
   let global_scope = tlc.load_file(None, "tests/prelude.tlc").unwrap();
   tlc.check(Some(global_scope), "let x: Real + Meter/Second = 1.2; let y: Real + Second = 3.4; let z: Real + Meter = x * y;").unwrap();

   let mut tlc = TLC::new();
   let global_scope = tlc.load_file(None, "tests/prelude.tlc").unwrap();
   tlc.check(Some(global_scope), "let x: Real + (Meter*Meter)/(Second*Second) = 1.2; let y: Real + Second = 3.4; let z: Real + Meter = x * x;").unwrap();

   let mut tlc = TLC::new();
   let global_scope = tlc.load_file(None, "tests/prelude.tlc").unwrap();
   tlc.check(Some(global_scope), "let x: Real + Meter/Second = 1.2; let y: Real + Second = 3.4; let z: Real + Meter = x;").unwrap_err();

   let mut tlc = TLC::new();
   let global_scope = tlc.load_file(None, "tests/prelude.tlc").unwrap();
   tlc.check(Some(global_scope), "let x: Real + Meter/Second = 1.2; let y: Real + Second = 3.4; let z: Real + Meter = y;").unwrap_err();

   let mut tlc = TLC::new();
   let global_scope = tlc.load_file(None, "tests/prelude.tlc").unwrap();
   tlc.check(Some(global_scope), "let x: Real + Meter/Second = 1.2; let y: Real + Second = 3.4; let z: Real + Meter = x * x;").unwrap_err();

   let mut tlc = TLC::new();
   let global_scope = tlc.load_file(None, "tests/prelude.tlc").unwrap();
   tlc.check(Some(global_scope), "let x: Real + Meter/Second = 1.2; let y: Real + Second = 3.4; let z: Real + Meter = y * y;").unwrap_err();

   let mut tlc = TLC::new();
   let global_scope = tlc.load_file(None, "tests/prelude.tlc").unwrap();
   tlc.check(Some(global_scope), "let x: Real + Meter/Second = 1.2; let y: Real + Second = 3.4; let z: Real + Meter = x * y * x;").unwrap_err();
}
*/
