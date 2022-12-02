use lsts::tlc::TLC;

#[test]
fn check_contradictions() {
   let mut tlc = TLC::new();
   let si = tlc.import_file(None, "preludes/si.tlc").unwrap();

   //Boolean Soundness
   //True and False are constructors of the Boolean type
   //it is therefore unsound to have a term that is both True and False
   tlc.check(Some(si), "let a:True;").unwrap();
   tlc.check(Some(si), "let a:False;").unwrap();
   tlc.check(Some(si), "let a:True+False;").unwrap_err();
}

/*
#[test]
fn check_prime_factors() {
   let mut tlc = TLC::new();
   let si = tlc.import_file(None, "preludes/si.tlc").unwrap();

   tlc.check(Some(si), "let x:Prime = 0").unwrap_err();
   tlc.check(Some(si), "let x:Prime = 1").unwrap_err();
   tlc.check(Some(si), "let x:Prime = 2").unwrap();
   tlc.check(Some(si), "let x:Prime = 3").unwrap();
   tlc.check(Some(si), "let x:Prime = 4").unwrap_err();
}
*/
