use lsts::tlc::TLC;

#[test]
fn check_simple_block() {
   let mut tlc = TLC::new();
   let si = tlc.import_file(None, "preludes/si.tlc").unwrap();

   tlc.check(Some(si), "{}").unwrap();
   tlc.check(Some(si), "{();}").unwrap();
   tlc.check(Some(si), "{();()}").unwrap();
   tlc.check(Some(si), "{};").unwrap();
   tlc.check(Some(si), "{();};").unwrap();
   tlc.check(Some(si), "{();()};").unwrap();
}
