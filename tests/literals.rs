/* TODO FIXME update to use L1IR for @reduce
use lsts::tlc::TLC;

#[test]
fn check_gradual_typing() {
   let mut tlc = TLC::new();
   let l1 = tlc.import_file(None, "preludes/l1.tlc").unwrap();

   tlc.check(Some(l1), r#"(literal "0":Binary) @reduce;"#).unwrap();
   tlc.check(Some(l1), r#"(literal "1":Binary) @reduce;"#).unwrap();
   tlc.check(Some(l1), r#"(literal "01":Binary) @reduce;"#).unwrap_err();
   tlc.check(Some(l1), r#"(literal "10":Binary) @reduce;"#).unwrap();
}
*/
