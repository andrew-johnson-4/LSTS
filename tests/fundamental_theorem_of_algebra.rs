use lsts::tlc::TLC;

#[test]
fn check_fundamental_theorem() {
   let mut tlc = TLC::new();
   let sa = tlc.import_str(None, r#"
"#).unwrap();

   //prove that
   tlc.check(Some(sa), r#"
"#).unwrap();
}
