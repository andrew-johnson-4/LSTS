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
   tlc.check(Some(si), "let a:Number[][2]").unwrap();
   tlc.check(Some(si), "let a:Number[?]").unwrap_err();
   tlc.check(Some(si), "let a:Number[()]").unwrap_err();
}
