use lsts::tlc::TLC;
use std::path::Path;

#[test]
fn parse_1() {
   let mut tlc = TLC::new();
   let si = tlc.import_file(None, "preludes/si.tlc").unwrap();
   if Path::new("tests/stress/1.tlc").exists() {
      tlc.import_file(Some(si), "tests/stress/1.tlc").unwrap();
   }
}

#[test]
fn parse_1k() {
   let mut tlc = TLC::new();
   let si = tlc.import_file(None, "preludes/si.tlc").unwrap();
   if Path::new("tests/stress/1.tlc").exists() {
      tlc.import_file(Some(si), "tests/stress/1k.tlc").unwrap();
   }
}