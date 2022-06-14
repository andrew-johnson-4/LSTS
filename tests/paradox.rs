use lsts::tlc::TLC;

#[test]
fn check_curry_paradox() {
   let mut tlc = TLC::new();

   //Curry's Paradox
   eprintln!("{:?}",
      tlc.check(None, "type A; forall :B. A => B").unwrap_err()
   );
   //reject: (A,B) do not share a domain (Term,Nil)

   tlc.check(None, "type A; forall :B::Term. A => B").unwrap();
   //accept: (A,B) share a domain (Term,Term)

   tlc.check(None, "type A; forall :B. A => B :: Term").unwrap();
   //accept: (A,B) share a domain (Term,Term)
}
