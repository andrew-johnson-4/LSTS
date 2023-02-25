/* TODO FIXME prove infinitude of primes in strict mode
use lsts::tlc::TLC;

#[test]
fn check_infinitude_of_primes() {
   let mut tlc = TLC::new();
   let si = tlc.import_file(None, "preludes/si.tlc").unwrap();

   //prove that there are an infinite number of primes
   tlc.check(Some(si), r#"
   let primes:Prime[];           //assume there are a finite number of primes
   //let p = primes.product + 1; //let p be the product of all primes + 1
   //forall d:primes. p%d == 1;    //p mod d, forall d in primes list, is 1
   "#).unwrap();

   //primes.product % d = 0            //by factorization
   //(primes.product + 1) % d = 1      //by modulo arithmetic
}
*/
