use lsts::tlc::TLC;

#[test]
fn one_plus_one_equals_two() {
   let mut tlc = TLC::new().strict();
   let alg = tlc.import_file(None, "preludes/algebra.tlc").unwrap();

   tlc.check(Some(alg), "1 + 1 == 2 : [True]").unwrap();
   tlc.check(Some(alg), "1 + 2 == 3 : [True]").unwrap_err();
}
