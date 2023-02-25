use lsts::tlc::TLC;

#[test]
fn check_contradictions() {
   let mut tlc = TLC::new();
   let l1 = tlc.import_file(None, "preludes/l1.tlc").unwrap();

   //Boolean Soundness
   //True and False are constructors of the Boolean type
   //it is therefore unsound to have a term that is both True and False
   tlc.check(Some(l1), "let a:True;").unwrap();
   tlc.check(Some(l1), "let a:False;").unwrap();
   tlc.check(Some(l1), "let a:True+False;").unwrap_err();
}

/* TODO FIXME: define prime with invariants on integers in L1
#[test]
fn check_prime_factors() {
   let mut tlc = TLC::new();
   let l1 = tlc.import_file(None, "preludes/l1.tlc").unwrap();

   tlc.check(Some(l1), "let x:Prime = 0;").unwrap_err();
   tlc.check(Some(l1), "let x:Prime = 1;").unwrap_err();
   tlc.check(Some(l1), "let x:Prime = 2;").unwrap();
   tlc.check(Some(l1), "let x:Prime = 3;").unwrap();
   tlc.check(Some(l1), "let x:Prime = 4;").unwrap_err();
}
*/
