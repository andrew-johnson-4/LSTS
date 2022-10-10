use lsts::tlc::TLC;

#[test]
fn check_statements() {
   let mut tlc = TLC::new();
   let si = tlc.import_file(None, "preludes/si.tlc").unwrap();

   tlc.check(Some(si), "forall x:Even. Odd = x + 1").unwrap();
   tlc.check(Some(si), "forall x:Even. Odd = x - 1").unwrap();
   tlc.check(Some(si), "forall x:Odd. Even = x + 1").unwrap();
   tlc.check(Some(si), "forall x:Odd. Even = x - 1").unwrap();
}

#[test]
fn check_hints() {
   let mut tlc = TLC::new();
   let si = tlc.import_file(None, "preludes/si.tlc").unwrap();

   tlc.check(Some(si), "forall @inc_even x:Even. Odd = x + 1; (8: Even) + 1 @inc_even : Odd;").unwrap();
   tlc.check(Some(si), "forall @dec_even x:Even. Odd = x - 1; (8: Even) - 1 @dec_even : Odd;").unwrap();
   tlc.check(Some(si), "forall @inc_odd x:Odd. Even = x + 1; (9: Odd) + 1 @inc_odd : Even;").unwrap();
   tlc.check(Some(si), "forall @dec_odd x:Odd. Even = x - 1; (9: Odd) - 1 @dec_odd : Even;").unwrap();

   tlc.check(Some(si), "forall @inc_even x:Even. Odd = x + 1; (8: Even) + 2 @inc_even : Odd;").unwrap_err();
   tlc.check(Some(si), "forall @dec_even x:Even. Odd = x - 1; (8: Even) - 2 @dec_even : Odd;").unwrap_err();
   tlc.check(Some(si), "forall @inc_odd x:Odd. Even = x + 1; (9: Odd) + 2 @inc_odd : Even;").unwrap_err();
   tlc.check(Some(si), "forall @dec_odd x:Odd. Even = x - 1; (9: Odd) - 2 @dec_odd : Even;").unwrap_err();
}

/* TODO: strict mode checks internal validity of statements
#[test]
fn check_strict_statements() {
   let mut tlc = TLC::new().strict();
   let si = tlc.import_file(None, "preludes/si.tlc").unwrap();

   tlc.check(Some(si), "forall x:Even. Odd = x + 1").unwrap();
   tlc.check(Some(si), "forall x:Even. Odd = x - 1").unwrap();
   tlc.check(Some(si), "forall x:Odd. Even = x + 1").unwrap();
   tlc.check(Some(si), "forall x:Odd. Even = x - 1").unwrap();

   tlc.check(Some(si), "forall x:Even. Odd = x + 2").unwrap_err();
   tlc.check(Some(si), "forall x:Even. Odd = x - 2").unwrap_err();
   tlc.check(Some(si), "forall x:Odd. Even = x + 2").unwrap_err();
   tlc.check(Some(si), "forall x:Odd. Even = x - 2").unwrap_err();
}
*/
