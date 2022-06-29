use lsts::tlc::TLC;

#[test]
fn check_precondition() {
   let mut tlc = TLC::new();
   let si = tlc.import_file(None, "preludes/si.tlc").unwrap();

   tlc.check(Some(si), "let a:Odd = 0;").unwrap_err();
   tlc.check(Some(si), "let a:Odd = 1;").unwrap();
   tlc.check(Some(si), "let a:Odd = 2;").unwrap_err();

   tlc.check(Some(si), "let a:Even = 0;").unwrap();
   tlc.check(Some(si), "let a:Even = 1;").unwrap_err();
   tlc.check(Some(si), "let a:Even = 2;").unwrap();

   tlc.check(Some(si), "let a:Prime = 0;").unwrap_err();
   tlc.check(Some(si), "let a:Prime = 1;").unwrap_err();
   tlc.check(Some(si), "let a:Prime = 2;").unwrap();
   tlc.check(Some(si), "let a:Prime = 3;").unwrap();
   tlc.check(Some(si), "let a:Prime = 4;").unwrap_err();
   tlc.check(Some(si), "let a:Prime = 5;").unwrap();
   tlc.check(Some(si), "let a:Prime = 6;").unwrap_err();
}

#[test]
fn check_postcondition() {
   let mut tlc = TLC::new();
   let si = tlc.import_file(None, "preludes/si.tlc").unwrap();

   tlc.check(Some(si), "let a:Odd; a%2:[0]").unwrap_err();
   tlc.check(Some(si), "let a:Odd; a%2:[1]").unwrap();

   tlc.check(Some(si), "let a:Even; a%2:[0]").unwrap();
   tlc.check(Some(si), "let a:Even; a%2:[1]").unwrap_err();
}
