use lsts::tlc::TLC;

#[test]
fn value_has_self() {
   let mut tlc = TLC::new().strict();
   let alg = tlc.import_file(None, "preludes/algebra.tlc").unwrap();

   tlc.check(Some(alg), "0 @reduce : [0]").unwrap();
   tlc.check(Some(alg), "1 @reduce : [1]").unwrap();
   tlc.check(Some(alg), "0 @reduce : [1]").unwrap_err();

   tlc.check(Some(alg), "True @reduce : [True]").unwrap();
   tlc.check(Some(alg), "False @reduce : [False]").unwrap();
   tlc.check(Some(alg), "True @reduce : [False]").unwrap_err();
}

#[test]
fn one_plus_one_equals_two() {
   let mut tlc = TLC::new().strict();
   let alg = tlc.import_file(None, "preludes/algebra.tlc").unwrap();

   tlc.check(Some(alg), "1 + 1 == 2 @reduce : [True]").unwrap();
   tlc.check(Some(alg), "1 + 2 == 2 @reduce : [True]").unwrap_err();
}
