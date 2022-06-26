use lsts::tlc::TLC;

#[test]
fn check_tensor_syntax() {
   let mut tlc = TLC::new();
   let si = tlc.import_file(None, "preludes/si.tlc").unwrap();

   tlc.check(Some(si), "let a:Number[]").unwrap();
   tlc.check(Some(si), "let a:Number[1]").unwrap();
   tlc.check(Some(si), "let a:Number[1][2]").unwrap();
   tlc.check(Some(si), "let a:Number[1][]").unwrap();
   tlc.check(Some(si), "let a:Number[][2]").unwrap();
}

#[test]
fn check_tensor_sugar() {
   let mut tlc = TLC::new();
   let si = tlc.import_file(None, "preludes/si.tlc").unwrap();

   tlc.check(Some(si), "let a:Number[]; a: Tensor<Number,?>").unwrap();
   tlc.check(Some(si), "let a:Number[1]; a: Tensor<Number,[1]>").unwrap();
   tlc.check(Some(si), "let a:Number[1][2]; a: Tensor<Tensor<Number,[2]>,[1]>").unwrap();
   tlc.check(Some(si), "let a:Number[1][]; a: Tensor<Tensor<Number,[1]>,?>").unwrap();
   tlc.check(Some(si), "let a:Number[][2]; a: Tensor<Tensor<Number,?>,[2]>").unwrap();
}

#[test]
fn check_tensor_invariants() {
   let mut tlc = TLC::new();
   let si = tlc.import_file(None, "preludes/si.tlc").unwrap();

   tlc.check(Some(si), "let a:Number[1]; a.length==0: [True]").unwrap_err();
   tlc.check(Some(si), "let a:Number[1]; a.length==0: [False]").unwrap();
   tlc.check(Some(si), "let a:Number[1]; a.length==1: [True]").unwrap();
   tlc.check(Some(si), "let a:Number[1]; a.length==1: [False]").unwrap_err();
}
