use lsts::tlc::TLC;

#[test]
fn check_unification_clobbering() {
   let mut tlc = TLC::new();
   let si = tlc.import_file(None, "preludes/si.tlc").unwrap();

   tlc.check(Some(si), "-(-(1)) @reduce :[1];").unwrap();
   tlc.check(Some(si), "-(-1) @reduce :[1];").unwrap();
   tlc.check(Some(si), "-(-2) @reduce :[2];").unwrap();
   tlc.check(Some(si), "-(-1) @reduce :[-1];").unwrap_err();
}
