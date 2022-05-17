use lsts::syntax::tlc::{TlcError,TlcExpr,TLC};

#[test]
fn parse_simplytyped() {
   TLC::check("let t: T");
   TLC::check("let t: 1");
   TLC::check("a");
   TLC::check("a()");
   TLC::check("a(b)");
   TLC::check("a(b,c)");
   TLC::check("let t: ()");
   TLC::check("let t: (A)");
   TLC::check("let t: (A,B)");
   TLC::check("let t: T<1,2>");
   TLC::check("let t: [1]");
   TLC::check("let t: ()->A");
   TLC::check("let t: A->B");
   TLC::check("let t: (A)->B");
   TLC::check("let t: (A,B)->C");
   TLC::check("let t: A|B");
   TLC::check("let a: A; let a: B");
   TLC::check("let a: A; let b: A");
}
