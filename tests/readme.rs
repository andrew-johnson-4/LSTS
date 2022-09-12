/* TODO unquote
use lsts::tlc::TLC;

#[test]
fn check_example() {
   let mut tlc = TLC::new();
   let si = tlc.import_file(None, "preludes/si.tlc").unwrap();

   //this is the example from the README file
   tlc.check(Some(si), "let v: Kilo<Metre>/Second = 123.456; let s: Minute = 78.9; let d: Mile = (v as (Mile/Minute)) * s;").unwrap();
}
*/
