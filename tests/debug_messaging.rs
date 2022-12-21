use lsts::tlc::TLC;

#[test]
fn check_lookup_failure_messaging() {
   let mut tlc = TLC::new();
   let l1 = tlc.import_file(None, "preludes/l1.tlc").unwrap();

   let _error = tlc.check(Some(l1), "x:Number").unwrap_err();
}
