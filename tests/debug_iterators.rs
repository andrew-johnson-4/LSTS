/* TODO FIXME use L1 and @reduce
use lsts::tlc::TLC;

#[test]
fn check_iterator_syntax() {
   let mut tlc = TLC::new();
   let l1 = tlc.import_file(None, "preludes/l1.tlc").unwrap();

   tlc.check(Some(l1), "for x:Integer in range(2) yield x;").unwrap();
   tlc.check(Some(l1), "for x:Integer in range(2) if x>2 yield x;").unwrap();

   tlc.check(Some(l1), "(for x:Integer in range(2) yield x);").unwrap();
   tlc.check(Some(l1), "(for x:Integer in range(2) if x>2 yield x);").unwrap();

   tlc.check(Some(l1), "[for x:Integer in range(2) yield x];").unwrap();
   tlc.check(Some(l1), "[for x:Integer in range(2) if x>2 yield x];").unwrap();

   tlc.check(Some(l1), "set([for x:Integer in range(2) yield x]);").unwrap();
   tlc.check(Some(l1), "set([for x:Integer in range(2) if x>2 yield x]);").unwrap();

   tlc.check(Some(l1), "map([for x:Integer in range(2) yield (x,x^2)]);").unwrap();
   tlc.check(Some(l1), "map([for x:Integer in range(2) if x>2 yield (x,x^2)]);").unwrap();
}
*/
