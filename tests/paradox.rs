use lsts::tlc::TLC;

#[test]
fn check_curry_paradox() {
   //Curry's Paradox
   let mut tlc = TLC::new();
   eprintln!("{:?}",
      tlc.check(None, "type A; forall :B. A => B").unwrap_err()
   );
   //reject: (A,B) do not share a domain (Term,Nil)

   let mut tlc = TLC::new();
   tlc.check(None, "type A; forall :B::Term. A => B").unwrap();
   //accept: (A,B) share a domain (Term,Term)
}
