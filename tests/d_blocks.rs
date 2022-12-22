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

/* TODO: revist why these even exist vs iterators
#[test]
fn check_simple_while() {
   let mut tlc = TLC::new();
   let l1 = tlc.import_file(None, "preludes/l1.tlc").unwrap();

   tlc.check(Some(l1), "while (True) {};").unwrap();
   tlc.check(Some(l1), "while (True) {();};").unwrap();
   tlc.check(Some(l1), "while (True) {();();};").unwrap();
}

#[test]
fn check_simple_loop() {
   let mut tlc = TLC::new();
   let l1 = tlc.import_file(None, "preludes/l1.tlc").unwrap();

   tlc.check(Some(l1), "loop {} while (True);").unwrap();
   tlc.check(Some(l1), "loop {();} while (True);").unwrap();
   tlc.check(Some(l1), "loop {();();} while (True);").unwrap();
}

#[test]
fn check_simple_for() {
   let mut tlc = TLC::new();
   let l1 = tlc.import_file(None, "preludes/l1.tlc").unwrap();

   tlc.check(Some(l1), "let xs:Integer[]; for (x in xs) {}").unwrap();
   tlc.check(Some(l1), "let xs:Integer[]; for (x in xs) {();}").unwrap();
   tlc.check(Some(l1), "let xs:Integer[]; for (x in xs) {();()}").unwrap();
}
*/
