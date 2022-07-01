use lsts::tlc::TLC;

#[test]
fn check_precedence() {
   let mut tlc = TLC::new();
   let si = tlc.import_file(None, "preludes/si.tlc").unwrap();

   tlc.check(Some(si), "3 / 1 ^ 2: [3]").unwrap();
   tlc.check(Some(si), "3 / 1 * 2 + 2 - 1 * 2 % 3: [6]").unwrap();

   tlc.check(Some(si), "1^2: [1]").unwrap();
   tlc.check(Some(si), "1^2 / 3: [0]").unwrap();
   tlc.check(Some(si), "1^2 / 3 * 4: [0]").unwrap();
   tlc.check(Some(si), "1^2 / 3 * 4 + 5: [5]").unwrap();
   tlc.check(Some(si), "1^2 / 3 * 4 + 5 < 6: [True]").unwrap();
   tlc.check(Some(si), "1^2 / 3 * 4 + 5 < 6 && 7 == 8 - 1: [True]").unwrap();
   tlc.check(Some(si), "1^2 / 3 * 4 + 5 < 6 && 7 == 8 - 1: [True]").unwrap();
   tlc.check(Some(si), "1^2 / 3 * 4 + 5 < 6 && 7 == 8 - 1: [True]").unwrap();
   tlc.check(Some(si), "1^2 / 3 * 4 + 5 < 6 && 7 == 8 - 1: [True]").unwrap();
   tlc.check(Some(si), "1^2 / 3 * 4 + 5 < 6 && 7 == 8 - 1: [True]").unwrap();
}

