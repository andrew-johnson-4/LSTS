use lsts::tlc::TLC;

#[test]
fn value_has_self() {
   let mut tlc = TLC::new();
   let alg = tlc.import_file(None, "preludes/l1.tlc").unwrap();

   tlc.check(Some(alg), "[1] :Tensor<Integer>;").unwrap();
   tlc.check(Some(alg), "[1,2] :Tensor<Integer>;").unwrap();
   tlc.check(Some(alg), "[1,2,3] :Tensor<Integer>;").unwrap();
   tlc.check(Some(alg), "[] :Tensor<Integer>;").unwrap();
   tlc.check(Some(alg), "[1] :Tensor<Boolean>;").unwrap_err();
}
