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

   tlc.check(Some(l1), "0 .binary @reduce :[0];").unwrap();
   tlc.check(Some(l1), "1 .binary @reduce :[1];").unwrap();
   tlc.check(Some(l1), "2 .binary @reduce :[10];").unwrap();
   tlc.check(Some(l1), "3 .binary @reduce :[11];").unwrap();
   tlc.check(Some(l1), "4 .binary @reduce :[100];").unwrap();
   tlc.check(Some(l1), "5 .binary @reduce :[101];").unwrap();
   tlc.check(Some(l1), "6 .binary @reduce :[110];").unwrap();
   tlc.check(Some(l1), "7 .binary @reduce :[111];").unwrap();
   tlc.check(Some(l1), "8 .binary @reduce :[1000];").unwrap();
   tlc.check(Some(l1), "9 .binary @reduce :[1001];").unwrap();
   tlc.check(Some(l1), "10 .binary @reduce :[1010];").unwrap();
   tlc.check(Some(l1), "11 .binary @reduce :[1011];").unwrap();
   tlc.check(Some(l1), "12 .binary @reduce :[1100];").unwrap();
   tlc.check(Some(l1), "13 .binary @reduce :[1101];").unwrap();
   tlc.check(Some(l1), "14 .binary @reduce :[1110];").unwrap();
   tlc.check(Some(l1), "15 .binary @reduce :[1111];").unwrap();
   tlc.check(Some(l1), "16 .binary @reduce :[10000];").unwrap();
   tlc.check(Some(l1), "23 .binary @reduce :[10111];").unwrap();
   tlc.check(Some(l1), "45 .binary @reduce :[101101];").unwrap();
   tlc.check(Some(l1), "67 .binary @reduce :[1000011];").unwrap();
   tlc.check(Some(l1), "89 .binary @reduce :[1011001];").unwrap();
   tlc.check(Some(l1), "-107 .binary @reduce :[-1101011];").unwrap();
   tlc.check(Some(l1), "-23 .binary @reduce :[-10111];").unwrap();
   tlc.check(Some(l1), "-45 .binary @reduce :[-101101];").unwrap();
   tlc.check(Some(l1), "-67 .binary @reduce :[-1000011];").unwrap();
   tlc.check(Some(l1), "-89 .binary @reduce :[-1011001];").unwrap();
   tlc.check(Some(l1), "-107 .binary @reduce :[-1101011];").unwrap();

   tlc.check(Some(l1), "+0 @reduce :[0];").unwrap();
   tlc.check(Some(l1), "+ 0 @reduce :[0];").unwrap();
   tlc.check(Some(l1), "+ +0 @reduce :[0];").unwrap();
   tlc.check(Some(l1), "+1 @reduce :[1];").unwrap();
   tlc.check(Some(l1), "+ 1 @reduce :[1];").unwrap();
   tlc.check(Some(l1), "+ +1 @reduce :[1];").unwrap();
   tlc.check(Some(l1), "+ 1 @reduce :[1];").unwrap();
   tlc.check(Some(l1), "+ +1 @reduce :[1];").unwrap();

   tlc.check(Some(l1), "-0 @reduce :[0];").unwrap();
   tlc.check(Some(l1), "- 0 @reduce :[0];").unwrap();
   tlc.check(Some(l1), "- -0 @reduce :[0];").unwrap();
   tlc.check(Some(l1), "-1 @reduce :[-1];").unwrap();
   tlc.check(Some(l1), "- 1 @reduce :[-1];").unwrap();
   tlc.check(Some(l1), "- -1 @reduce :[1];").unwrap();
   tlc.check(Some(l1), "- 1 @reduce :[1];").unwrap_err();
   tlc.check(Some(l1), "- -1 @reduce :[-1];").unwrap_err();

   tlc.check(Some(l1), "True @reduce :[True];").unwrap();
   tlc.check(Some(l1), "False @reduce :[False];").unwrap();

   tlc.check(Some(l1), "not(True) @reduce :[False];").unwrap();
   tlc.check(Some(l1), "not(not(True)) @reduce :[True];").unwrap();
   tlc.check(Some(l1), "not(False) @reduce :[True];").unwrap();
   tlc.check(Some(l1), "not(not(False)) @reduce :[False];").unwrap();

   tlc.check(Some(l1), "(if True then 0 else 1) @reduce :[0];").unwrap();
   tlc.check(Some(l1), "(if False then 0 else 1) @reduce :[1];").unwrap();

   tlc.check(Some(l1), "(True && True && True) @reduce :[True];").unwrap();
   tlc.check(Some(l1), "(True && True && False) @reduce :[False];").unwrap();
   tlc.check(Some(l1), "(True && False && True) @reduce :[False];").unwrap();
   tlc.check(Some(l1), "(True && False && False) @reduce :[False];").unwrap();
   tlc.check(Some(l1), "(False && True && True) @reduce :[False];").unwrap();
   tlc.check(Some(l1), "(False && True && False) @reduce :[False];").unwrap();
   tlc.check(Some(l1), "(False && False && True) @reduce :[False];").unwrap();
   tlc.check(Some(l1), "(False && False && False) @reduce :[False];").unwrap();

   tlc.check(Some(l1), "(True || True || True) @reduce :[True];").unwrap();
   tlc.check(Some(l1), "(True || True || False) @reduce :[True];").unwrap();
   tlc.check(Some(l1), "(True || False || True) @reduce :[True];").unwrap();
   tlc.check(Some(l1), "(True || False || False) @reduce :[True];").unwrap();
   tlc.check(Some(l1), "(False || True || True) @reduce :[True];").unwrap();
   tlc.check(Some(l1), "(False || True || False) @reduce :[True];").unwrap();
   tlc.check(Some(l1), "(False || False || True) @reduce :[True];").unwrap();
   tlc.check(Some(l1), "(False || False || False) @reduce :[False];").unwrap();

   tlc.check(Some(l1), "(0 == 0) @reduce :[True];").unwrap();
   tlc.check(Some(l1), "(0 == 1) @reduce :[False];").unwrap();
   tlc.check(Some(l1), "(1 == 0) @reduce :[False];").unwrap();
   tlc.check(Some(l1), "(1 == 1) @reduce :[True];").unwrap();
   tlc.check(Some(l1), "(2 == 1) @reduce :[False];").unwrap();
   tlc.check(Some(l1), "(1 == 2) @reduce :[False];").unwrap();
   tlc.check(Some(l1), "(10 == 1) @reduce :[False];").unwrap();
   tlc.check(Some(l1), "(1 == 11) @reduce :[False];").unwrap();
   tlc.check(Some(l1), "(0 == -1) @reduce :[False];").unwrap();
   tlc.check(Some(l1), "(-1 == 5) @reduce :[False];").unwrap();
   tlc.check(Some(l1), "(345 == 67) @reduce :[False];").unwrap();
   tlc.check(Some(l1), "(890 == -5) @reduce :[False];").unwrap();
   tlc.check(Some(l1), "(-123 == 45) @reduce :[False];").unwrap();

   tlc.check(Some(l1), "(0 != 0) @reduce :[False];").unwrap();
   tlc.check(Some(l1), "(0 != 1) @reduce :[True];").unwrap();
   tlc.check(Some(l1), "(1 != 0) @reduce :[True];").unwrap();
   tlc.check(Some(l1), "(1 != 1) @reduce :[False];").unwrap();
   tlc.check(Some(l1), "(2 != 1) @reduce :[True];").unwrap();
   tlc.check(Some(l1), "(1 != 2) @reduce :[True];").unwrap();
   tlc.check(Some(l1), "(10 != 1) @reduce :[True];").unwrap();
   tlc.check(Some(l1), "(1 != 11) @reduce :[True];").unwrap();
   tlc.check(Some(l1), "(0 != -1) @reduce :[True];").unwrap();
   tlc.check(Some(l1), "(-1 != 5) @reduce :[True];").unwrap();
   tlc.check(Some(l1), "(345 != 67) @reduce :[True];").unwrap();
   tlc.check(Some(l1), "(890 != -5) @reduce :[True];").unwrap();
   tlc.check(Some(l1), "(-123 != 45) @reduce :[True];").unwrap();

   tlc.check(Some(l1), "(0 < 0) @reduce :[False];").unwrap();
   tlc.check(Some(l1), "(0 < 1) @reduce :[True];").unwrap();
   tlc.check(Some(l1), "(1 < 0) @reduce :[False];").unwrap();
   tlc.check(Some(l1), "(1 < 1) @reduce :[False];").unwrap();
   tlc.check(Some(l1), "(2 < 1) @reduce :[False];").unwrap();
   tlc.check(Some(l1), "(1 < 2) @reduce :[True];").unwrap();
   tlc.check(Some(l1), "(10 < 1) @reduce :[False];").unwrap();
   tlc.check(Some(l1), "(1 < 11) @reduce :[True];").unwrap();
   tlc.check(Some(l1), "(0 < -1) @reduce :[False];").unwrap();
   tlc.check(Some(l1), "(-1 < 5) @reduce :[True];").unwrap();
   tlc.check(Some(l1), "(345 < 67) @reduce :[False];").unwrap();
   tlc.check(Some(l1), "(67 < 345) @reduce :[True];").unwrap();
   tlc.check(Some(l1), "(890 < -5) @reduce :[False];").unwrap();
   tlc.check(Some(l1), "(-123 < 45) @reduce :[True];").unwrap();
   tlc.check(Some(l1), "(-123 < -45) @reduce :[True];").unwrap();

   tlc.check(Some(l1), "(0 <= 1) @reduce :[True];").unwrap();
   tlc.check(Some(l1), "(1 <= 0) @reduce :[False];").unwrap();
   tlc.check(Some(l1), "(1 <= 1) @reduce :[True];").unwrap();
   tlc.check(Some(l1), "(2 <= 1) @reduce :[False];").unwrap();
   tlc.check(Some(l1), "(1 <= 2) @reduce :[True];").unwrap();
   tlc.check(Some(l1), "(10 <= 1) @reduce :[False];").unwrap();
   tlc.check(Some(l1), "(1 <= 11) @reduce :[True];").unwrap();
   tlc.check(Some(l1), "(0 <= -1) @reduce :[False];").unwrap();
   tlc.check(Some(l1), "(-1 <= 5) @reduce :[True];").unwrap();
   tlc.check(Some(l1), "(345 <= 67) @reduce :[False];").unwrap();
   tlc.check(Some(l1), "(890 <= -5) @reduce :[False];").unwrap();
   tlc.check(Some(l1), "(-123 <= 45) @reduce :[True];").unwrap();
   tlc.check(Some(l1), "(-123 <= -45) @reduce :[True];").unwrap();

   tlc.check(Some(l1), "(0 > 0) @reduce :[False];").unwrap();
   tlc.check(Some(l1), "(0 > 1) @reduce :[False];").unwrap();
   tlc.check(Some(l1), "(1 > 0) @reduce :[True];").unwrap();
   tlc.check(Some(l1), "(1 > 1) @reduce :[False];").unwrap();
   tlc.check(Some(l1), "(2 > 1) @reduce :[True];").unwrap();
   tlc.check(Some(l1), "(1 > 2) @reduce :[False];").unwrap();
   tlc.check(Some(l1), "(10 > 1) @reduce :[True];").unwrap();
   tlc.check(Some(l1), "(1 > 11) @reduce :[False];").unwrap();
   tlc.check(Some(l1), "(0 > -1) @reduce :[True];").unwrap();
   tlc.check(Some(l1), "(-1 > 5) @reduce :[False];").unwrap();
   tlc.check(Some(l1), "(345 > 67) @reduce :[True];").unwrap();
   tlc.check(Some(l1), "(890 > -5) @reduce :[True];").unwrap();
   tlc.check(Some(l1), "(-123 > 45) @reduce :[False];").unwrap();
   tlc.check(Some(l1), "(-123 > -45) @reduce :[False];").unwrap();

   tlc.check(Some(l1), "(0 >= 0) @reduce :[True];").unwrap();
   tlc.check(Some(l1), "(0 >= 1) @reduce :[False];").unwrap();
   tlc.check(Some(l1), "(1 >= 0) @reduce :[True];").unwrap();
   tlc.check(Some(l1), "(1 >= 1) @reduce :[True];").unwrap();
   tlc.check(Some(l1), "(2 >= 1) @reduce :[True];").unwrap();
   tlc.check(Some(l1), "(1 >= 2) @reduce :[False];").unwrap();
   tlc.check(Some(l1), "(10 >= 1) @reduce :[True];").unwrap();
   tlc.check(Some(l1), "(1 >= 11) @reduce :[False];").unwrap();
   tlc.check(Some(l1), "(0 >= -1) @reduce :[True];").unwrap();
   tlc.check(Some(l1), "(-1 >= 5) @reduce :[False];").unwrap();
   tlc.check(Some(l1), "(345 >= 67) @reduce :[True];").unwrap();
   tlc.check(Some(l1), "(890 >= -5) @reduce :[True];").unwrap();
   tlc.check(Some(l1), "(-123 >= 45) @reduce :[False];").unwrap();
   tlc.check(Some(l1), "(-123 > -45) @reduce :[False];").unwrap();

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

   /*
   tlc.check(Some(l1), "((0:Whole) - (0:Whole)) @reduce :[0];").unwrap();
   tlc.check(Some(l1), "((8:Whole) - (3:Whole)) @reduce :[5];").unwrap();
   tlc.check(Some(l1), "((12:Whole) - (3:Whole)) @reduce :[9];").unwrap();
   tlc.check(Some(l1), "((123:Whole) - (45:Whole)) @reduce :[78];").unwrap();

   tlc.check(Some(l1), "(0 + -1) @reduce :[-1];").unwrap();
   tlc.check(Some(l1), "(-1 + 5) @reduce :[4];").unwrap();
   tlc.check(Some(l1), "(345 + 67) @reduce :[412];").unwrap();
   tlc.check(Some(l1), "(890 + -5) @reduce :[885];").unwrap();
   tlc.check(Some(l1), "(-123 + 45) @reduce :[-78];").unwrap();

   tlc.check(Some(l1), "(0 - 0) @reduce :[0];").unwrap();
   tlc.check(Some(l1), "(0 - 1) @reduce :[-1];").unwrap();
   tlc.check(Some(l1), "(1 - 0) @reduce :[1];").unwrap();
   tlc.check(Some(l1), "(1 - 1) @reduce :[0];").unwrap();
   tlc.check(Some(l1), "(2 - 1) @reduce :[1];").unwrap();
   tlc.check(Some(l1), "(1 - 2) @reduce :[-1];").unwrap();
   tlc.check(Some(l1), "(10 - 1) @reduce :[9];").unwrap();
   tlc.check(Some(l1), "(1 - 11) @reduce :[-10];").unwrap();
   tlc.check(Some(l1), "(0 - -1) @reduce :[1];").unwrap();
   tlc.check(Some(l1), "(-1 - 5) @reduce :[-6];").unwrap();
   tlc.check(Some(l1), "(345 - 67) @reduce :[278];").unwrap();
   tlc.check(Some(l1), "(890 - -5) @reduce :[895];").unwrap();
   tlc.check(Some(l1), "(-123 - 45) @reduce :[-168];").unwrap();

   tlc.check(Some(l1), "(0 / 0) @reduce :[0];").unwrap_err();
   tlc.check(Some(l1), "(0 / 1) @reduce :[0];").unwrap();
   tlc.check(Some(l1), "(1 / 0) @reduce :[0];").unwrap_err();
   tlc.check(Some(l1), "(1 / 1) @reduce :[1];").unwrap();
   tlc.check(Some(l1), "(2 / 1) @reduce :[2];").unwrap();
   tlc.check(Some(l1), "(1 / 2) @reduce :[0];").unwrap();
   tlc.check(Some(l1), "(1 / 11) @reduce :[0];").unwrap();
   tlc.check(Some(l1), "(4 / 123) @reduce :[0];").unwrap();
   tlc.check(Some(l1), "(10 / 1) @reduce :[10];").unwrap();
   tlc.check(Some(l1), "(0 / -1) @reduce :[0];").unwrap();
   tlc.check(Some(l1), "(-1 / 5) @reduce :[0];").unwrap();
   tlc.check(Some(l1), "(345 / 67) @reduce :[5];").unwrap();
   tlc.check(Some(l1), "(890 / -5) @reduce :[-178];").unwrap();
   tlc.check(Some(l1), "(-123 / 45) @reduce :[-2];").unwrap();
   tlc.check(Some(l1), "(-5 / -6) @reduce :[0];").unwrap();

   tlc.check(Some(l1), "(0 % 0) @reduce :[0];").unwrap_err();
   tlc.check(Some(l1), "(0 % 1) @reduce :[0];").unwrap();
   tlc.check(Some(l1), "(1 % 0) @reduce :[0];").unwrap_err();
   tlc.check(Some(l1), "(1 % 1) @reduce :[0];").unwrap();
   tlc.check(Some(l1), "(2 % 1) @reduce :[0];").unwrap();
   tlc.check(Some(l1), "(1 % 2) @reduce :[1];").unwrap();
   tlc.check(Some(l1), "(1 % 11) @reduce :[1];").unwrap();
   tlc.check(Some(l1), "(4 % 123) @reduce :[4];").unwrap();
   tlc.check(Some(l1), "(10 % 1) @reduce :[0];").unwrap();
   tlc.check(Some(l1), "(0 % -1) @reduce :[0];").unwrap();
   tlc.check(Some(l1), "(-1 % 5) @reduce :[4];").unwrap();
   tlc.check(Some(l1), "(345 % 67) @reduce :[10];").unwrap();
   tlc.check(Some(l1), "(890 % -5) @reduce :[0];").unwrap();
   tlc.check(Some(l1), "(-123 % 45) @reduce :[12];").unwrap();
   tlc.check(Some(l1), "(-5 % -6) @reduce :[-5];").unwrap();

   tlc.check(Some(l1), "(0 ^ 0) @reduce :[0];").unwrap_err();
   tlc.check(Some(l1), "(0 ^ 1) @reduce :[0];").unwrap();
   tlc.check(Some(l1), "(1 ^ 0) @reduce :[0];").unwrap_err();
   tlc.check(Some(l1), "(1 ^ 1) @reduce :[0];").unwrap();
   tlc.check(Some(l1), "(2 ^ 1) @reduce :[0];").unwrap();
   tlc.check(Some(l1), "(1 ^ 2) @reduce :[1];").unwrap();
   tlc.check(Some(l1), "(1 ^ 11) @reduce :[1];").unwrap();
   tlc.check(Some(l1), "(4 ^ 123) @reduce :[4];").unwrap();
   tlc.check(Some(l1), "(10 ^ 1) @reduce :[0];").unwrap();
   tlc.check(Some(l1), "(0 ^ -1) @reduce :[0];").unwrap();
   tlc.check(Some(l1), "(-1 ^ 5) @reduce :[4];").unwrap();
   tlc.check(Some(l1), "(345 ^ 67) @reduce :[10];").unwrap();
   tlc.check(Some(l1), "(890 ^ -5) @reduce :[0];").unwrap();
   tlc.check(Some(l1), "(-123 ^ 45) @reduce :[12];").unwrap();
   tlc.check(Some(l1), "(-5 ^ -6) @reduce :[-5];").unwrap();
   */
}

