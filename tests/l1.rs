use lsts::tlc::TLC;

#[test]
fn l1_homogenous_tuples() {
   let mut tlc = TLC::new();
   let alg = tlc.import_file(None, "preludes/l1.tlc").unwrap();

   tlc.check(Some(alg), "(): Integer[0];").unwrap();
   tlc.check(Some(alg), "(1): Integer[0];").unwrap_err();
   tlc.check(Some(alg), "(1): Integer;").unwrap();
   tlc.check(Some(alg), "(1,): Integer[0];").unwrap_err();
   tlc.check(Some(alg), "(1,): Integer[1];").unwrap();
   tlc.check(Some(alg), "(1,2): Integer[1];").unwrap_err();
   tlc.check(Some(alg), "(1,2): Integer[2];").unwrap();
   tlc.check(Some(alg), "(1,2,3): Integer[2];").unwrap_err();
   tlc.check(Some(alg), "(1,2,3): Integer[3];").unwrap();
   tlc.check(Some(alg), "((1,2,3): Integer[3]).0 @reduce :[1];").unwrap();
   tlc.check(Some(alg), "((1,2,3): Integer[3]).1 @reduce :[2];").unwrap();
   tlc.check(Some(alg), "((1,2,3): Integer[3]).2 @reduce :[3];").unwrap();
   tlc.check(Some(alg), "((1,2,3): Integer[3])[-1] @reduce :[1];").unwrap_err();
   tlc.check(Some(alg), "((1,2,3): Integer[3])[0] @reduce :[1];").unwrap();
   tlc.check(Some(alg), "((1,2,3): Integer[3])[1] @reduce :[2];").unwrap();
   tlc.check(Some(alg), "((1,2,3): Integer[3])[2] @reduce :[3];").unwrap();
   tlc.check(Some(alg), "((1,2,3): Integer[3])[3] @reduce :[3];").unwrap_err();
}
