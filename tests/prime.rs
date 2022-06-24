use lsts::tlc::TLC;

#[test]
fn check_properties_of_primes() {
   let mut tlc = TLC::new();
   let si = tlc.import_file(None, "preludes/si.tlc").unwrap();

   tlc.check(Some(si), "let p:Prime").unwrap();
}
