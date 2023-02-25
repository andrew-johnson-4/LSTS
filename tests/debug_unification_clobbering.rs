use lsts::tlc::TLC;

#[test]
fn check_unification_clobbering() {
   let mut tlc = TLC::new();
   let l1 = tlc.import_file(None, "preludes/l1.tlc").unwrap();

   tlc.check(Some(l1), "-(-(1)) @reduce :[1];").unwrap();
   tlc.check(Some(l1), "-(-1) @reduce :[1];").unwrap();
   tlc.check(Some(l1), "-(-2) @reduce :[2];").unwrap();
   tlc.check(Some(l1), "-(-1) @reduce :[-1];").unwrap_err();
}
