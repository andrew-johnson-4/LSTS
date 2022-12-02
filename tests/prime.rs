use lsts::tlc::TLC;

#[test]
fn check_properties_of_primes() {
   let mut tlc = TLC::new();
   let si = tlc.import_file(None, "preludes/si.tlc").unwrap();

   tlc.check(Some(si), "let p:Prime;").unwrap();
   tlc.check(Some(si), "let p:Prime = 0;").unwrap_err();
   tlc.check(Some(si), "let p:Prime = 1;").unwrap_err();
   tlc.check(Some(si), "let p:Prime = 2;").unwrap();
   tlc.check(Some(si), "let p:Prime = 3;").unwrap();
   tlc.check(Some(si), "let p:Prime = 4;").unwrap_err();
   tlc.check(Some(si), "let p:Prime = 5;").unwrap();
   tlc.check(Some(si), "let p:Prime = -1;").unwrap_err();
}
