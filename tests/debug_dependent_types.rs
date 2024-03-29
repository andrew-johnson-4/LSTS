use lsts::tlc::TLC;

#[test]
fn check_constant_equivalence() {
   let mut tlc = TLC::new();
   let l1 = tlc.import_file(None, "preludes/l1.tlc").unwrap();

   tlc.check(Some(l1), "0 @reduce :[0];").unwrap();
   tlc.check(Some(l1), "1 @reduce :[1];").unwrap();
   tlc.check(Some(l1), "2 @reduce :[2];").unwrap();
   tlc.check(Some(l1), "0 @reduce :[1];").unwrap_err();
   tlc.check(Some(l1), "0 @reduce :[2];").unwrap_err();
   tlc.check(Some(l1), "1 @reduce :[0];").unwrap_err();
   tlc.check(Some(l1), "-1 @reduce :[-1];").unwrap();
   tlc.check(Some(l1), "-1 @reduce :[0];").unwrap_err();

   tlc.check(Some(l1), "(1,3,5).0 @reduce :[1];").unwrap();
   tlc.check(Some(l1), "(1,3,5).1 @reduce :[3];").unwrap();
   tlc.check(Some(l1), "(1,3,5).2 @reduce :[5];").unwrap();

   tlc.check(Some(l1), "+0 @reduce :[0];").unwrap();
   tlc.check(Some(l1), "+ 0 @reduce :[0];").unwrap();
   tlc.check(Some(l1), "+ +0 @reduce :[0];").unwrap();
   tlc.check(Some(l1), "+1 @reduce :[1];").unwrap();
   tlc.check(Some(l1), "+ 1 @reduce :[1];").unwrap();
   tlc.check(Some(l1), "+ +1 @reduce :[1];").unwrap();
   tlc.check(Some(l1), "+ 1 @reduce :[1];").unwrap();
   tlc.check(Some(l1), "+ +1 @reduce :[1];").unwrap();

   tlc.check(Some(l1), "- 0 @reduce :[0];").unwrap();
   tlc.check(Some(l1), "- -0 @reduce :[0];").unwrap();
   tlc.check(Some(l1), "-1 @reduce :[-1];").unwrap();
   tlc.check(Some(l1), "- 1 @reduce :[-1];").unwrap();
   tlc.check(Some(l1), "- -1 @reduce :[1];").unwrap();
   tlc.check(Some(l1), "- 1 @reduce :[1];").unwrap_err();
   tlc.check(Some(l1), "- -1 @reduce :[-1];").unwrap_err();

   tlc.check(Some(l1), "True @reduce :[1];").unwrap();
   tlc.check(Some(l1), "False @reduce :[0];").unwrap();

   tlc.check(Some(l1), "not(True) @reduce :[0];").unwrap();
   tlc.check(Some(l1), "not(not(True)) @reduce :[1];").unwrap();
   tlc.check(Some(l1), "not(False) @reduce :[1];").unwrap();
   tlc.check(Some(l1), "not(not(False)) @reduce :[0];").unwrap();

   tlc.check(Some(l1), "(if True then 0 else 1) @reduce :[0];").unwrap();
   tlc.check(Some(l1), "(if False then 0 else 1) @reduce :[1];").unwrap();

   tlc.check(Some(l1), "(True && True && True) @reduce :[1];").unwrap();
   tlc.check(Some(l1), "(True && True && False) @reduce :[0];").unwrap();
   tlc.check(Some(l1), "(True && False && True) @reduce :[0];").unwrap();
   tlc.check(Some(l1), "(True && False && False) @reduce :[0];").unwrap();
   tlc.check(Some(l1), "(False && True && True) @reduce :[0];").unwrap();
   tlc.check(Some(l1), "(False && True && False) @reduce :[0];").unwrap();
   tlc.check(Some(l1), "(False && False && True) @reduce :[0];").unwrap();
   tlc.check(Some(l1), "(False && False && False) @reduce :[0];").unwrap();

   tlc.check(Some(l1), "(True || True || True) @reduce :[1];").unwrap();
   tlc.check(Some(l1), "(True || True || False) @reduce :[1];").unwrap();
   tlc.check(Some(l1), "(True || False || True) @reduce :[1];").unwrap();
   tlc.check(Some(l1), "(True || False || False) @reduce :[1];").unwrap();
   tlc.check(Some(l1), "(False || True || True) @reduce :[1];").unwrap();
   tlc.check(Some(l1), "(False || True || False) @reduce :[1];").unwrap();
   tlc.check(Some(l1), "(False || False || True) @reduce :[1];").unwrap();
   tlc.check(Some(l1), "(False || False || False) @reduce :[0];").unwrap();

   tlc.check(Some(l1), "(0 == 0) @reduce :[1];").unwrap();
   tlc.check(Some(l1), "(0 == 1) @reduce :[0];").unwrap();
   tlc.check(Some(l1), "(1 == 0) @reduce :[0];").unwrap();
   tlc.check(Some(l1), "(1 == 1) @reduce :[1];").unwrap();
   tlc.check(Some(l1), "(2 == 1) @reduce :[0];").unwrap();
   tlc.check(Some(l1), "(1 == 2) @reduce :[0];").unwrap();
   tlc.check(Some(l1), "(10 == 1) @reduce :[0];").unwrap();
   tlc.check(Some(l1), "(1 == 11) @reduce :[0];").unwrap();
   tlc.check(Some(l1), "(345 == 67) @reduce :[0];").unwrap();
   tlc.check(Some(l1), "(-5 == 67) @reduce :[0];").unwrap();
   tlc.check(Some(l1), "(-5 == -5) @reduce :[1];").unwrap();

   tlc.check(Some(l1), "(0 != 0) @reduce :[0];").unwrap();
   tlc.check(Some(l1), "(0 != 1) @reduce :[1];").unwrap();
   tlc.check(Some(l1), "(1 != 0) @reduce :[1];").unwrap();
   tlc.check(Some(l1), "(1 != 1) @reduce :[0];").unwrap();
   tlc.check(Some(l1), "(2 != 1) @reduce :[1];").unwrap();
   tlc.check(Some(l1), "(1 != 2) @reduce :[1];").unwrap();
   tlc.check(Some(l1), "(10 != 1) @reduce :[1];").unwrap();
   tlc.check(Some(l1), "(1 != 11) @reduce :[1];").unwrap();
   tlc.check(Some(l1), "(345 != 67) @reduce :[1];").unwrap();
   tlc.check(Some(l1), "(-5 != 67) @reduce :[1];").unwrap();
   tlc.check(Some(l1), "(-5 != -5) @reduce :[0];").unwrap();

   tlc.check(Some(l1), "(0 < 0) @reduce :[0];").unwrap();
   tlc.check(Some(l1), "(0 < 1) @reduce :[1];").unwrap();
   tlc.check(Some(l1), "(1 < 0) @reduce :[0];").unwrap();
   tlc.check(Some(l1), "(1 < 1) @reduce :[0];").unwrap();
   tlc.check(Some(l1), "(2 < 1) @reduce :[0];").unwrap();
   tlc.check(Some(l1), "(1 < 2) @reduce :[1];").unwrap();
   tlc.check(Some(l1), "(10 < 1) @reduce :[0];").unwrap();
   tlc.check(Some(l1), "(1 < 11) @reduce :[1];").unwrap();
   tlc.check(Some(l1), "(345 < 67) @reduce :[0];").unwrap();
   tlc.check(Some(l1), "(67 < 345) @reduce :[1];").unwrap();
   tlc.check(Some(l1), "(-5 < 67) @reduce :[1];").unwrap();
   tlc.check(Some(l1), "(-5 < -5) @reduce :[0];").unwrap();

   tlc.check(Some(l1), "(0 <= 1) @reduce :[1];").unwrap();
   tlc.check(Some(l1), "(1 <= 0) @reduce :[0];").unwrap();
   tlc.check(Some(l1), "(1 <= 1) @reduce :[1];").unwrap();
   tlc.check(Some(l1), "(2 <= 1) @reduce :[0];").unwrap();
   tlc.check(Some(l1), "(1 <= 2) @reduce :[1];").unwrap();
   tlc.check(Some(l1), "(10 <= 1) @reduce :[0];").unwrap();
   tlc.check(Some(l1), "(1 <= 11) @reduce :[1];").unwrap();
   tlc.check(Some(l1), "(345 <= 67) @reduce :[0];").unwrap();
   tlc.check(Some(l1), "(-5 <= 67) @reduce :[1];").unwrap();
   tlc.check(Some(l1), "(-5 <= -5) @reduce :[1];").unwrap();

   tlc.check(Some(l1), "(0 > 0) @reduce :[0];").unwrap();
   tlc.check(Some(l1), "(0 > 1) @reduce :[0];").unwrap();
   tlc.check(Some(l1), "(1 > 0) @reduce :[1];").unwrap();
   tlc.check(Some(l1), "(1 > 1) @reduce :[0];").unwrap();
   tlc.check(Some(l1), "(2 > 1) @reduce :[1];").unwrap();
   tlc.check(Some(l1), "(1 > 2) @reduce :[0];").unwrap();
   tlc.check(Some(l1), "(10 > 1) @reduce :[1];").unwrap();
   tlc.check(Some(l1), "(1 > 11) @reduce :[0];").unwrap();
   tlc.check(Some(l1), "(345 > 67) @reduce :[1];").unwrap();
   tlc.check(Some(l1), "(-5 > 67) @reduce :[0];").unwrap();
   tlc.check(Some(l1), "(-5 > -5) @reduce :[0];").unwrap();

   tlc.check(Some(l1), "(0 >= 0) @reduce :[1];").unwrap();
   tlc.check(Some(l1), "(0 >= 1) @reduce :[0];").unwrap();
   tlc.check(Some(l1), "(1 >= 0) @reduce :[1];").unwrap();
   tlc.check(Some(l1), "(1 >= 1) @reduce :[1];").unwrap();
   tlc.check(Some(l1), "(2 >= 1) @reduce :[1];").unwrap();
   tlc.check(Some(l1), "(1 >= 2) @reduce :[0];").unwrap();
   tlc.check(Some(l1), "(10 >= 1) @reduce :[1];").unwrap();
   tlc.check(Some(l1), "(1 >= 11) @reduce :[0];").unwrap();
   tlc.check(Some(l1), "(345 >= 67) @reduce :[1];").unwrap();
   tlc.check(Some(l1), "(-5 >= 67) @reduce :[0];").unwrap();
   tlc.check(Some(l1), "(-5 >= -5) @reduce :[1];").unwrap();

   tlc.check(Some(l1), "(0 + 0) @reduce :[0];").unwrap();
   tlc.check(Some(l1), "(0 + 1) @reduce :[1];").unwrap();
   tlc.check(Some(l1), "(1 + 0) @reduce :[1];").unwrap();
   tlc.check(Some(l1), "(1 + 1) @reduce :[2];").unwrap();
   tlc.check(Some(l1), "(2 + 1) @reduce :[3];").unwrap();
   tlc.check(Some(l1), "(1 + 2) @reduce :[3];").unwrap();
   tlc.check(Some(l1), "(7 + 8) @reduce :[15];").unwrap();
   tlc.check(Some(l1), "(10 + 1) @reduce :[11];").unwrap();
   tlc.check(Some(l1), "(1 + 11) @reduce :[12];").unwrap();
   tlc.check(Some(l1), "(2 + 12) @reduce :[14];").unwrap();
   tlc.check(Some(l1), "(25 + 125) @reduce :[150];").unwrap();
   tlc.check(Some(l1), "(1 + 999) @reduce :[1000];").unwrap();
   tlc.check(Some(l1), "(999 + 1) @reduce :[1000];").unwrap();
   tlc.check(Some(l1), "(-5 + 67) @reduce :[62];").unwrap();
   tlc.check(Some(l1), "(-5 + -5) @reduce :[-10];").unwrap();
   tlc.check(Some(l1), "(345 + 67) @reduce :[412];").unwrap();

   tlc.check(Some(l1), "(0 - 0) @reduce :[0];").unwrap();
   tlc.check(Some(l1), "(8 - 3) @reduce :[5];").unwrap();
   tlc.check(Some(l1), "(12 - 3) @reduce :[9];").unwrap();
   tlc.check(Some(l1), "(123 - 45) @reduce :[78];").unwrap();
   tlc.check(Some(l1), "(-5 - 67) @reduce :[-72];").unwrap();
   tlc.check(Some(l1), "(-5 - -5) @reduce :[0];").unwrap();
   tlc.check(Some(l1), "(0 - 0) @reduce :[0];").unwrap();
   tlc.check(Some(l1), "(1 - 0) @reduce :[1];").unwrap();
   tlc.check(Some(l1), "(1 - 1) @reduce :[0];").unwrap();
   tlc.check(Some(l1), "(2 - 1) @reduce :[1];").unwrap();
   tlc.check(Some(l1), "(10 - 1) @reduce :[9];").unwrap();
   tlc.check(Some(l1), "(345 - 67) @reduce :[278];").unwrap();

   tlc.check(Some(l1), "(0 / 1) @reduce :[0];").unwrap();
   tlc.check(Some(l1), "(1 / 1) @reduce :[1];").unwrap();
   tlc.check(Some(l1), "(2 / 1) @reduce :[2];").unwrap();
   tlc.check(Some(l1), "(1 / 2) @reduce :[0];").unwrap();
   tlc.check(Some(l1), "(1 / 11) @reduce :[0];").unwrap();
   tlc.check(Some(l1), "(4 / 123) @reduce :[0];").unwrap();
   tlc.check(Some(l1), "(10 / 1) @reduce :[10];").unwrap();
   tlc.check(Some(l1), "(345 / 67) @reduce :[5];").unwrap();
   tlc.check(Some(l1), "(-5 / 67) @reduce :[0];").unwrap();
   tlc.check(Some(l1), "(-5 / -5) @reduce :[1];").unwrap();

   tlc.check(Some(l1), "(0 % 1) @reduce :[0];").unwrap();
   tlc.check(Some(l1), "(1 % 1) @reduce :[0];").unwrap();
   tlc.check(Some(l1), "(2 % 1) @reduce :[0];").unwrap();
   tlc.check(Some(l1), "(1 % 2) @reduce :[1];").unwrap();
   tlc.check(Some(l1), "(1 % 11) @reduce :[1];").unwrap();
   tlc.check(Some(l1), "(4 % 123) @reduce :[4];").unwrap();
   tlc.check(Some(l1), "(10 % 1) @reduce :[0];").unwrap();
   tlc.check(Some(l1), "(345 % 67) @reduce :[10];").unwrap();
   tlc.check(Some(l1), "(-5 % 67) @reduce :[-5];").unwrap();
   tlc.check(Some(l1), "(-5 % -5) @reduce :[0];").unwrap();
}
