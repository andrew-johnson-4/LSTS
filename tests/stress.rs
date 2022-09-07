/* TODO remove
use lsts::tlc::TLC;

#[test]
fn parse_1() {
   let mut tlc = TLC::new();
   let si = tlc.import_file(None, "preludes/si.tlc").unwrap();
   tlc.import_file(Some(si), "tests/stress/1.tlc").unwrap();
}

#[test]
fn parse_10() {
   let mut tlc = TLC::new();
   let si = tlc.import_file(None, "preludes/si.tlc").unwrap();
   tlc.import_file(Some(si), "tests/stress/10.tlc").unwrap();
}

#[test]
fn parse_100() {
   let mut tlc = TLC::new();
   let si = tlc.import_file(None, "preludes/si.tlc").unwrap();
   tlc.import_file(Some(si), "tests/stress/100.tlc").unwrap();
}
*/

/*
#[test]
fn parse_1k() {
   let mut tlc = TLC::new();
   let si = tlc.import_file(None, "preludes/si.tlc").unwrap();
   tlc.import_file(Some(si), "tests/stress/1k.tlc").unwrap();
}

#[test]
fn parse_10k() {
   let mut tlc = TLC::new();
   let si = tlc.import_file(None, "preludes/si.tlc").unwrap();
   if Path::new("tests/stress/10k.tlc").exists() {
      tlc.import_file(Some(si), "tests/stress/10k.tlc").unwrap();
   }
}
*/
