use lsts::tlc::TLC;

#[test]
fn check_statements() {
   let mut tlc = TLC::new();
   let si = tlc.import_file(None, "preludes/l1.tlc").unwrap();

   tlc.check(Some(si), "forall x:Even. Odd = x + 1;").unwrap();
   tlc.check(Some(si), "forall x:Even. Odd = x - 1;").unwrap();
   tlc.check(Some(si), "forall x:Odd. Even = x + 1;").unwrap();
   tlc.check(Some(si), "forall x:Odd. Even = x - 1;").unwrap();
}

/* TODO FIXME apply statements to suggest hints
#[test]
fn check_hints() {
   let mut tlc = TLC::new();
   let si = tlc.import_file(None, "preludes/l1.tlc").unwrap();

   tlc.check(Some(si), "forall @inc_even x:Even. Odd = x + 1; (8: Even) + 1 @inc_even : Odd;").unwrap();
   tlc.check(Some(si), "forall @dec_even x:Even. Odd = x - 1; (8: Even) - 1 @dec_even : Odd;").unwrap();
   tlc.check(Some(si), "forall @inc_odd x:Odd. Even = x + 1; (9: Odd) + 1 @inc_odd : Even;").unwrap();
   tlc.check(Some(si), "forall @dec_odd x:Odd. Even = x - 1; (9: Odd) - 1 @dec_odd : Even;").unwrap();

   tlc.check(Some(si), "forall @inc_even x:Even. Odd = x + 1; (8: Even) + 2 @inc_even : Odd;").unwrap_err();
   tlc.check(Some(si), "forall @dec_even x:Even. Odd = x - 1; (8: Even) - 2 @dec_even : Odd;").unwrap_err();
   tlc.check(Some(si), "forall @inc_odd x:Odd. Even = x + 1; (9: Odd) + 2 @inc_odd : Even;").unwrap_err();
   tlc.check(Some(si), "forall @dec_odd x:Odd. Even = x - 1; (9: Odd) - 2 @dec_odd : Even;").unwrap_err();
}

#[test]
fn check_pun() {
   let mut tlc = TLC::new();
   let si = tlc.import_file(None, "preludes/l1.tlc").unwrap();

   tlc.check(Some(si), "forall @ch x:Even. [True] = x; forall @ch x:Odd. [True] = x; (8: Even) @ch : [True];").unwrap();
   tlc.check(Some(si), "forall @ch x:Even. [True] = x; forall @ch x:Odd. [True] = x; (3: Odd) @ch : [True];").unwrap();
   tlc.check(Some(si), "forall @ch x:Even. [True] = x; forall @ch x:Odd. [True] = x; (8: Integer) @ch : [True];").unwrap_err();
}
*/

/* TODO FIXME: strict mode checks internal validity of statements
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
