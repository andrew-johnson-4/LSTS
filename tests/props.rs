use lsts::tlc::TLC;

#[test]
fn check_contradictions() {
   let mut tlc = TLC::new();
   let si = tlc.import_file(None, "preludes/si.tlc").unwrap();

   //Boolean Soundness
   //True and False are constructors of the Boolean type
   //it is therefore unsound to have a term that is both True and False
   tlc.check(Some(si), "let a:True").unwrap();
   tlc.check(Some(si), "let a:False").unwrap();
   tlc.check(Some(si), "let a:True+False").unwrap_err();
}
