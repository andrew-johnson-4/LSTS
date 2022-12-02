use lsts::tlc::TLC;

#[test]
fn check_simple_block() {
   let mut tlc = TLC::new();
   let si = tlc.import_file(None, "preludes/si.tlc").unwrap();

   tlc.check(Some(si), "{};").unwrap();
   tlc.check(Some(si), "{();};").unwrap();
   tlc.check(Some(si), "{();();};").unwrap();
   tlc.check(Some(si), "{};").unwrap();
   tlc.check(Some(si), "{();};").unwrap();
   tlc.check(Some(si), "{();();};").unwrap();
}

#[test]
fn check_simple_while() {
   let mut tlc = TLC::new();
   let si = tlc.import_file(None, "preludes/si.tlc").unwrap();

   tlc.check(Some(si), "while (True) {};").unwrap();
   tlc.check(Some(si), "while (True) {();};").unwrap();
   tlc.check(Some(si), "while (True) {();();};").unwrap();
}

#[test]
fn check_simple_loop() {
   let mut tlc = TLC::new();
   let si = tlc.import_file(None, "preludes/si.tlc").unwrap();

   tlc.check(Some(si), "loop {} while (True);").unwrap();
   tlc.check(Some(si), "loop {();} while (True);").unwrap();
   tlc.check(Some(si), "loop {();();} while (True);").unwrap();
}

/*
#[test]
fn check_simple_for() {
   let mut tlc = TLC::new();
   let si = tlc.import_file(None, "preludes/si.tlc").unwrap();

   tlc.check(Some(si), "let xs:Integer[]; for (x in xs) {}").unwrap();
   tlc.check(Some(si), "let xs:Integer[]; for (x in xs) {();}").unwrap();
   tlc.check(Some(si), "let xs:Integer[]; for (x in xs) {();()}").unwrap();
}
*/
