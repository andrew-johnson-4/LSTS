use lsts::tlc::TLC;

#[test]
fn check_precedence() {
   let mut tlc = TLC::new();
   let si = tlc.import_file(None, "preludes/l1.tlc").unwrap();

   tlc.check(Some(si), "3 / 1 * 2 + 2 - 1 % 2 @reduce: [7];").unwrap();

   tlc.check(Some(si), "3 / 1 ^ 2 @reduce: [3];").unwrap();
   tlc.check(Some(si), "3 / 1 + 2 @reduce: [5];").unwrap();
   tlc.check(Some(si), "3 / 1 * 2 @reduce: [6];").unwrap();
   tlc.check(Some(si), "3 / 1 * 2 + 2 - 1 % 2 @reduce: [7];").unwrap();
   tlc.check(Some(si), "3 / 1 * 2 + 2 - 1 * 2 % 3 @reduce: [6];").unwrap();

   tlc.check(Some(si), "1^2 @reduce: [1];").unwrap();
   tlc.check(Some(si), "1^2 / 3 @reduce: [0];").unwrap();
   tlc.check(Some(si), "1^2 / 3 * 4 @reduce: [0];").unwrap();
   tlc.check(Some(si), "1^2 / 3 * 4 + 5 @reduce: [5];").unwrap();
   tlc.check(Some(si), "1^2 / 3 * 4 + 5 < 6 @reduce: [1];").unwrap();
   tlc.check(Some(si), "1^2 / 3 * 4 + 5 < 6 && 7 == 8 - 1 @reduce: [1];").unwrap();
   tlc.check(Some(si), "1^2 / 3 * 4 + 5 < 6 && 7 == 8 - 1 @reduce: [1];").unwrap();
   tlc.check(Some(si), "1^2 / 3 * 4 + 5 < 6 && 7 == 8 - 1 @reduce: [1];").unwrap();
   tlc.check(Some(si), "1^2 / 3 * 4 + 5 < 6 && 7 == 8 - 1 @reduce: [1];").unwrap();
   tlc.check(Some(si), "1^2 / 3 * 4 + 5 < 6 && 7 == 8 - 1 @reduce: [1];").unwrap();
}
