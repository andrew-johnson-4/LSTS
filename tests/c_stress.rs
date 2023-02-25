use lsts::tlc::TLC;

#[test]
fn parse_1() {
   let mut tlc = TLC::new();
   let l1 = tlc.import_file(None, "preludes/l1.tlc").unwrap();
   tlc.import_file(Some(l1), "tests/stress/1.tlc").unwrap();
}

#[test]
fn parse_10() {
   let mut tlc = TLC::new();
   let l1 = tlc.import_file(None, "preludes/l1.tlc").unwrap();
   tlc.import_file(Some(l1), "tests/stress/10.tlc").unwrap();
}

/* TODO FIXME move typechecking off the stack onto the heap
#[test]
fn parse_100() {
   let mut tlc = TLC::new();
   let l1 = tlc.import_file(None, "preludes/l1.tlc").unwrap();
   tlc.import_file(Some(l1), "tests/stress/100.tlc").unwrap();
}

#[test]
fn parse_1k() {
   let mut tlc = TLC::new();
   let l1 = tlc.import_file(None, "preludes/l1.tlc").unwrap();
   tlc.import_file(Some(l1), "tests/stress/1k.tlc").unwrap();
}

use std::path::Path;
#[test]
fn parse_10k() {
   let mut tlc = TLC::new();
   let l1 = tlc.import_file(None, "preludes/l1.tlc").unwrap();
   if Path::new("tests/stress/10k.tlc").exists() {
      tlc.import_file(Some(l1), "tests/stress/10k.tlc").unwrap();
   }
}
*/
