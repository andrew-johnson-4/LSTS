use lsts::tlc::TLC;

#[test]
fn check_type_equality() {
   let mut tlc = TLC::new();
   let si = tlc.compile_file(None, "preludes/si.tlc").unwrap();
   tlc.check(Some(si), "let x:Metre=2; let y:Metre=x;").unwrap();

   let mut tlc = TLC::new();
   let si = tlc.compile_file(None, "preludes/si.tlc").unwrap();
   tlc.check(Some(si), "let x:Metre=2; let y:Second=x;").unwrap_err();
}
