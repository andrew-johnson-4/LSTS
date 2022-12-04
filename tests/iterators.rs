use lsts::tlc::TLC;

#[test]
fn check_constant_literals() {
   let mut tlc = TLC::new();
   let l1 = tlc.import_file(None, "preludes/l1.tlc").unwrap();

   tlc.check(Some(l1), "(for x:Integer yield x);").unwrap();
   tlc.check(Some(l1), "(for x:Integer if x>2 yield x);").unwrap();

   tlc.check(Some(l1), "[for x:Integer yield x];").unwrap();
   tlc.check(Some(l1), "[for x:Integer if x>2 yield x];").unwrap();

   tlc.check(Some(l1), "{for x:Integer yield x};").unwrap();
   tlc.check(Some(l1), "{for x:Integer if x>2 yield x};").unwrap();

   tlc.check(Some(l1), "{for x:Integer yield x=x^2};").unwrap();
   tlc.check(Some(l1), "{for x:Integer if x>2 yield x=x^2};").unwrap();
}
