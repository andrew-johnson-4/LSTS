use lsts::tlc::TLC;

#[test]
fn check_prelex() {
   let mut tlc = TLC::new();

   tlc.check(None, "type Integer = /^[0-9]+$/; 1 : Integer;").unwrap();
   tlc.check(None, "type Integer = /^[0-9]+$/; 12 : Integer;").unwrap();
   tlc.check(None, "type Integer = /^[0-9]+$/; 1.2 : Integer;").unwrap_err();
   tlc.check(None, "type Float = /^[0-9]+[.0-9]*$/; 1.2 : Float;").unwrap();
   tlc.check(None, "type Float = /^[0-9]+[.0-9]*$/; -1.2 : Float;").unwrap_err();
}
