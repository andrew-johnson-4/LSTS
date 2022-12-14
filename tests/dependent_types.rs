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
   tlc.check(Some(l1), "(1 < 1) @reduce :[True];").unwrap();
   tlc.check(Some(l1), "(2 < 1) @reduce :[False];").unwrap();
   tlc.check(Some(l1), "(1 < 2) @reduce :[True];").unwrap();
   tlc.check(Some(l1), "(10 < 1) @reduce :[False];").unwrap();
   tlc.check(Some(l1), "(1 < 11) @reduce :[True];").unwrap();
   tlc.check(Some(l1), "(0 < -1) @reduce :[False];").unwrap();
   tlc.check(Some(l1), "(-1 < 5) @reduce :[True];").unwrap();
   tlc.check(Some(l1), "(345 < 67) @reduce :[False];").unwrap();
   tlc.check(Some(l1), "(890 < -5) @reduce :[False];").unwrap();
   tlc.check(Some(l1), "(-123 < 45) @reduce :[True];").unwrap();

   tlc.check(Some(l1), "(0 <= 0) @reduce :[False];").unwrap();
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

   tlc.check(Some(l1), "(0 > 0) @reduce :[False];").unwrap();
   tlc.check(Some(l1), "(0 > 1) @reduce :[True];").unwrap();
   tlc.check(Some(l1), "(1 > 0) @reduce :[False];").unwrap();
   tlc.check(Some(l1), "(1 > 1) @reduce :[True];").unwrap();
   tlc.check(Some(l1), "(2 > 1) @reduce :[False];").unwrap();
   tlc.check(Some(l1), "(1 > 2) @reduce :[True];").unwrap();
   tlc.check(Some(l1), "(10 > 1) @reduce :[False];").unwrap();
   tlc.check(Some(l1), "(1 > 11) @reduce :[True];").unwrap();
   tlc.check(Some(l1), "(0 > -1) @reduce :[False];").unwrap();
   tlc.check(Some(l1), "(-1 > 5) @reduce :[True];").unwrap();
   tlc.check(Some(l1), "(345 > 67) @reduce :[False];").unwrap();
   tlc.check(Some(l1), "(890 > -5) @reduce :[False];").unwrap();
   tlc.check(Some(l1), "(-123 > 45) @reduce :[True];").unwrap();

   tlc.check(Some(l1), "(0 >= 0) @reduce :[False];").unwrap();
   tlc.check(Some(l1), "(0 >= 1) @reduce :[True];").unwrap();
   tlc.check(Some(l1), "(1 >= 0) @reduce :[False];").unwrap();
   tlc.check(Some(l1), "(1 >= 1) @reduce :[True];").unwrap();
   tlc.check(Some(l1), "(2 >= 1) @reduce :[False];").unwrap();
   tlc.check(Some(l1), "(1 >= 2) @reduce :[True];").unwrap();
   tlc.check(Some(l1), "(10 >= 1) @reduce :[False];").unwrap();
   tlc.check(Some(l1), "(1 >= 11) @reduce :[True];").unwrap();
   tlc.check(Some(l1), "(0 >= -1) @reduce :[False];").unwrap();
   tlc.check(Some(l1), "(-1 >= 5) @reduce :[True];").unwrap();
   tlc.check(Some(l1), "(345 >= 67) @reduce :[False];").unwrap();
   tlc.check(Some(l1), "(890 >= -5) @reduce :[False];").unwrap();
   tlc.check(Some(l1), "(-123 >= 45) @reduce :[True];").unwrap();

   tlc.check(Some(l1), "(0 + 0) @reduce :[0];").unwrap();
   tlc.check(Some(l1), "(0 + 1) @reduce :[1];").unwrap();
   tlc.check(Some(l1), "(1 + 0) @reduce :[1];").unwrap();
   tlc.check(Some(l1), "(1 + 1) @reduce :[2];").unwrap();
   tlc.check(Some(l1), "(2 + 1) @reduce :[3];").unwrap();
   tlc.check(Some(l1), "(1 + 2) @reduce :[3];").unwrap();
   tlc.check(Some(l1), "(10 + 1) @reduce :[11];").unwrap();
   tlc.check(Some(l1), "(1 + 11) @reduce :[12];").unwrap();
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

   /*
   tlc.check(Some(l1), "let x:[0*0]; x: [0];").unwrap();
   tlc.check(Some(l1), "let x:[0*1]; x: [0];").unwrap();
   tlc.check(Some(l1), "let x:[1*0]; x: [0];").unwrap();
   tlc.check(Some(l1), "let x:[1*1]; x: [1];").unwrap();
   tlc.check(Some(l1), "let x:[2*1]; x: [2];").unwrap();
   tlc.check(Some(l1), "let x:[1*2]; x: [2];").unwrap();
   tlc.check(Some(l1), "let x:[2*2]; x: [4];").unwrap();
   tlc.check(Some(l1), "let x:[2*2]; x: [5];").unwrap_err();
   tlc.check(Some(l1), "let x:[1*3]; x: [4];").unwrap_err();

   tlc.check(Some(l1), "let x:[0/0]; x: [NaN];").unwrap();
   tlc.check(Some(l1), "let x:[0/1]; x: [0];").unwrap();
   tlc.check(Some(l1), "let x:[1/0]; x: [NaN];").unwrap();
   tlc.check(Some(l1), "let x:[1/1]; x: [1];").unwrap();
   tlc.check(Some(l1), "let x:[2/1]; x: [2];").unwrap();
   tlc.check(Some(l1), "let x:[1/2]; x: [0];").unwrap();
   tlc.check(Some(l1), "let x:[2/2]; x: [1];").unwrap();
   tlc.check(Some(l1), "let x:[3/2]; x: [1];").unwrap();
   tlc.check(Some(l1), "let x:[4/2]; x: [2];").unwrap();
   tlc.check(Some(l1), "let x:[5/2]; x: [2];").unwrap();
   tlc.check(Some(l1), "let x:[5/0]; x: [2];").unwrap_err();
   tlc.check(Some(l1), "let x:[5/2]; x: [3];").unwrap_err();

   tlc.check(Some(l1), "let x:[0%0]; x: [NaN];").unwrap();
   tlc.check(Some(l1), "let x:[0%1]; x: [0];").unwrap();
   tlc.check(Some(l1), "let x:[1%0]; x: [NaN];").unwrap();
   tlc.check(Some(l1), "let x:[1%1]; x: [0];").unwrap();
   tlc.check(Some(l1), "let x:[2%1]; x: [0];").unwrap();
   tlc.check(Some(l1), "let x:[1%2]; x: [1];").unwrap();
   tlc.check(Some(l1), "let x:[2%2]; x: [0];").unwrap();
   tlc.check(Some(l1), "let x:[3%2]; x: [1];").unwrap();
   tlc.check(Some(l1), "let x:[4%2]; x: [0];").unwrap();
   tlc.check(Some(l1), "let x:[5%2]; x: [1];").unwrap();
   tlc.check(Some(l1), "let x:[5%2]; x: [7];").unwrap_err();
   tlc.check(Some(l1), "let x:[0%1]; x: [0];").unwrap();
   tlc.check(Some(l1), "let x:[0%0]; x: [0];").unwrap_err();

   tlc.check(Some(l1), "let x:[1^0]; x: [1];").unwrap();
   tlc.check(Some(l1), "let x:[1^0]; x: [2];").unwrap_err();
   tlc.check(Some(l1), "let x:[1^1]; x: [1];").unwrap();
   tlc.check(Some(l1), "let x:[1^1]; x: [2];").unwrap_err();
   tlc.check(Some(l1), "let x:[1^2]; x: [1];").unwrap();
   tlc.check(Some(l1), "let x:[1^2]; x: [2];").unwrap_err();
   tlc.check(Some(l1), "let x:[2^0]; x: [1];").unwrap();
   tlc.check(Some(l1), "let x:[2^0]; x: [2];").unwrap_err();
   tlc.check(Some(l1), "let x:[2^1]; x: [1];").unwrap_err();
   tlc.check(Some(l1), "let x:[2^1]; x: [2];").unwrap();
   tlc.check(Some(l1), "let x:[2^2]; x: [1];").unwrap_err();
   tlc.check(Some(l1), "let x:[2^2]; x: [2];").unwrap_err();
   tlc.check(Some(l1), "let x:[2^2]; x: [4];").unwrap();
   */
}

