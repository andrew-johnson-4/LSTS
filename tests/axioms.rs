/* TODO implement literal math in strict mode
use lsts::tlc::TLC;

#[test]
fn algebra1() {
   let mut tlc = TLC::new().strict();
   let alg = tlc.import_file(None, "preludes/algebra.tlc").unwrap();

   tlc.check(Some(alg), "1 + 2 * 3 == 1 + 2 * 3 @reflexive : [True];").unwrap();
   tlc.check(Some(alg), "2 * 3 + 1 == 1 + 2 * 3 @reflexive : [True];").unwrap_err();
}

#[test]
fn unfounded1() {
   let mut tlc = TLC::new().strict();
   let alg = tlc.import_file(None, "preludes/algebra.tlc").unwrap();

   tlc.check(Some(alg), "axiom  @true. [True] = 1; 1 @true : [True];").unwrap();
   tlc.check(Some(alg), "forall @true. [True] = 1; 1 @true : [True];").unwrap_err();
}
*/
