use lsts::tlc::TLC;

#[test]
fn check_simple_block() {
   let mut tlc = TLC::new();
   let l1 = tlc.import_file(None, "preludes/l1.tlc").unwrap();

   tlc.check(Some(l1), "{};").unwrap();
   tlc.check(Some(l1), "{();};").unwrap();
   tlc.check(Some(l1), "{();();};").unwrap();
   tlc.check(Some(l1), "{};").unwrap();
   tlc.check(Some(l1), "{();};").unwrap();
   tlc.check(Some(l1), "{();();};").unwrap();
}
