use lsts::tlc::TLC;

#[test]
fn parse_simplytyped() {
   let mut tlc = TLC::new();
   tlc.parse_str(None,"a;").unwrap();
   tlc.parse_str(None,"a();").unwrap();
   tlc.parse_str(None,"a(b);").unwrap();
   tlc.parse_str(None,"a(b,c);").unwrap();
   tlc.parse_str(None,"a.f;").unwrap();
   tlc.parse_str(None,"a.f(b);").unwrap();
   tlc.parse_str(None,"a.f(b,c);").unwrap();
   tlc.parse_str(None,"let t: ?;").unwrap();
   tlc.parse_str(None,"let t: T;").unwrap();
   tlc.parse_str(None,"let t: ();").unwrap();
   tlc.parse_str(None,"let t: (A);").unwrap();
   tlc.parse_str(None,"let t: (A,B);").unwrap();
   tlc.parse_str(None,"let t: T<A,B>;").unwrap();
   tlc.parse_str(None,"let t: ()->A;").unwrap();
   tlc.parse_str(None,"let t: A->B;").unwrap();
   tlc.parse_str(None,"let t: (A)->B;").unwrap();
   tlc.parse_str(None,"let t: (A,B)->C;").unwrap();
   tlc.parse_str(None,"let a: A; let a: B;").unwrap();
   tlc.parse_str(None,"let a: A; let b: A;").unwrap();
   tlc.parse_str(None,"let f();").unwrap();
   tlc.parse_str(None,"let f(a:A);").unwrap();
   tlc.parse_str(None,"let f(a:A,b:B);").unwrap();
   tlc.parse_str(None,"let f(a:A);").unwrap();
   tlc.parse_str(None,"let f(a:A::Term);").unwrap();
   tlc.parse_str(None,"let f():A;").unwrap();
   tlc.parse_str(None,"let f()::Term;").unwrap();
   tlc.parse_str(None,"type A;").unwrap();
   tlc.parse_str(None,"forall :A,:B::C. (A,B);").unwrap();
   tlc.parse_str(None,"forall :A,:B::C. (A,B) :: R;").unwrap();
   tlc.parse_str(None,"forall :A,:B::C. (A,B) => C :: R;").unwrap();
   tlc.parse_str(None,"{a; b;};").unwrap();

   //Type Names, like Ab, are always valid constants, even without a prelude
   tlc.parse_str(None,"let t: T[Ab];").unwrap();
}
