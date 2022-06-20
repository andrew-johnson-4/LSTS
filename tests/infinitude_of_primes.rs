use lsts::tlc::TLC;

#[test]
fn check_infinitude_of_primes() {
   let mut tlc = TLC::new();
   let sa = tlc.import_str(None, r#"
   //define a prime as being indivisible by any other integer
   type Prime:Integer where d:Prime, n:Integer if (n>1). d%n != 0;
"#).unwrap();

   //prove that there are an infinite number of primes
   tlc.check(Some(sa), r#"
   //let primes:Prime[];           //assume there are a finite number of primes
   //let p = primes.product() + 1; //let p be the product of all primes + 1
   //forall d:primes. p%d == 1;    //p mod d, forall d in primes list, is 1
   "#).unwrap();
}
