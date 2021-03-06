use lsts::tlc::TLC;

#[test]
fn check_constant_equivalence() {
   let mut tlc = TLC::new();
   let si = tlc.import_file(None, "preludes/si.tlc").unwrap();

   tlc.check(Some(si), "0: [0]").unwrap();
   tlc.check(Some(si), "1: [1]").unwrap();
   tlc.check(Some(si), "2: [2]").unwrap();
   tlc.check(Some(si), "0: [1]").unwrap_err();
   tlc.check(Some(si), "0: [2]").unwrap_err();
   tlc.check(Some(si), "1: [0]").unwrap_err();

   tlc.check(Some(si), "let x: [False]; x: [False]").unwrap();
   tlc.check(Some(si), "let x: [False]; x: [True]").unwrap_err();
   tlc.check(Some(si), "let x: [True]; x: [False]").unwrap_err();
   tlc.check(Some(si), "let x: [True]; x: [True]").unwrap();
   tlc.check(Some(si), "let x: [0]; x: [False]").unwrap_err();
   tlc.check(Some(si), "let x: [1]; x: [True]").unwrap_err();

   tlc.check(Some(si), "let x:[-0]; x: [0]").unwrap();
   tlc.check(Some(si), "let x:[-0]; x: [-0]").unwrap();
   tlc.check(Some(si), "let x:[-1]; x: [-1]").unwrap();
   tlc.check(Some(si), "let x:[-2]; x: [-2]").unwrap();
   tlc.check(Some(si), "let x:[-2]; x: [2]").unwrap_err();
   tlc.check(Some(si), "let x:[-(-0)]; x: [0]").unwrap();
   tlc.check(Some(si), "let x:[-(-1)]; x: [1]").unwrap();
   tlc.check(Some(si), "let x:[-(-2)]; x: [2]").unwrap();
   tlc.check(Some(si), "let x:[-(-1)]; x: [-1]").unwrap_err();

   tlc.check(Some(si), "let x:[not(True)]; x: [True]").unwrap_err();
   tlc.check(Some(si), "let x:[not(True)]; x: [False]").unwrap();
   tlc.check(Some(si), "let x:[not(False)]; x: [True]").unwrap();
   tlc.check(Some(si), "let x:[not(False)]; x: [False]").unwrap_err();

   tlc.check(Some(si), r#"let x:[$"if"(True,1,False)]; x: [1]"#).unwrap();
   tlc.check(Some(si), r#"let x:[$"if"(True,1,False)]; x: [True]"#).unwrap_err();
   tlc.check(Some(si), r#"let x:[$"if"(True,2,False)]; x: [2]"#).unwrap();
   tlc.check(Some(si), r#"let x:[$"if"(True,2,False)]; x: [3]"#).unwrap_err();
   tlc.check(Some(si), r#"let x:[$"if"(False,1,False)]; x: [False]"#).unwrap();
   tlc.check(Some(si), r#"let x:[$"if"(False,1,False)]; x: [0]"#).unwrap_err();
   tlc.check(Some(si), r#"let x:[$"if"(False,1,True)]; x: [True]"#).unwrap();
   tlc.check(Some(si), r#"let x:[$"if"(False,1,True)]; x: [-2]"#).unwrap_err();

   tlc.check(Some(si), "let x:[True && True && True]; x: [True]").unwrap();
   tlc.check(Some(si), "let x:[True && True && False]; x: [True]").unwrap_err();
   tlc.check(Some(si), "let x:[True && False && True]; x: [True]").unwrap_err();
   tlc.check(Some(si), "let x:[True && False && False]; x: [True]").unwrap_err();
   tlc.check(Some(si), "let x:[False && True && True]; x: [True]").unwrap_err();
   tlc.check(Some(si), "let x:[False && True && False]; x: [True]").unwrap_err();
   tlc.check(Some(si), "let x:[False && False && True]; x: [True]").unwrap_err();
   tlc.check(Some(si), "let x:[False && False && False]; x: [True]").unwrap_err();

   tlc.check(Some(si), "let x:[True && True && True]; x: [False]").unwrap_err();
   tlc.check(Some(si), "let x:[True && True && False]; x: [False]").unwrap();
   tlc.check(Some(si), "let x:[True && False && True]; x: [False]").unwrap();
   tlc.check(Some(si), "let x:[True && False && False]; x: [False]").unwrap();
   tlc.check(Some(si), "let x:[False && True && True]; x: [False]").unwrap();
   tlc.check(Some(si), "let x:[False && True && False]; x: [False]").unwrap();
   tlc.check(Some(si), "let x:[False && False && True]; x: [False]").unwrap();
   tlc.check(Some(si), "let x:[False && False && False]; x: [False]").unwrap();

   tlc.check(Some(si), "let x:[True || True || True]; x: [True]").unwrap();
   tlc.check(Some(si), "let x:[True || True || False]; x: [True]").unwrap();
   tlc.check(Some(si), "let x:[True || False || True]; x: [True]").unwrap();
   tlc.check(Some(si), "let x:[True || False || False]; x: [True]").unwrap();
   tlc.check(Some(si), "let x:[False || True || True]; x: [True]").unwrap();
   tlc.check(Some(si), "let x:[False || True || False]; x: [True]").unwrap();
   tlc.check(Some(si), "let x:[False || False || True]; x: [True]").unwrap();
   tlc.check(Some(si), "let x:[False || False || False]; x: [True]").unwrap_err();

   tlc.check(Some(si), "let x:[True || True || True]; x: [False]").unwrap_err();
   tlc.check(Some(si), "let x:[True || True || False]; x: [False]").unwrap_err();
   tlc.check(Some(si), "let x:[True || False || True]; x: [False]").unwrap_err();
   tlc.check(Some(si), "let x:[True || False || False]; x: [False]").unwrap_err();
   tlc.check(Some(si), "let x:[False || True || True]; x: [False]").unwrap_err();
   tlc.check(Some(si), "let x:[False || True || False]; x: [False]").unwrap_err();
   tlc.check(Some(si), "let x:[False || False || True]; x: [False]").unwrap_err();
   tlc.check(Some(si), "let x:[False || False || False]; x: [False]").unwrap();

   tlc.check(Some(si), "let x:[0 + 0]; x: [0]").unwrap();
   tlc.check(Some(si), "let x:[0 + 1]; x: [1]").unwrap();
   tlc.check(Some(si), "let x:[1 + 0]; x: [1]").unwrap();
   tlc.check(Some(si), "let x:[1 + 1]; x: [2]").unwrap();
   tlc.check(Some(si), "let x:[2 + 1]; x: [3]").unwrap();
   tlc.check(Some(si), "let x:[1 + 2]; x: [3]").unwrap();
   tlc.check(Some(si), "let x:[2 + 2]; x: [4]").unwrap();
   tlc.check(Some(si), "let x:[1 + 1]; x: [3]").unwrap_err();
   tlc.check(Some(si), "let x:[2 + 2]; x: [5]").unwrap_err();

   tlc.check(Some(si), "let x:[0 - 0]; x: [0]").unwrap();
   tlc.check(Some(si), "let x:[0 - 1]; x: [-1]").unwrap();
   tlc.check(Some(si), "let x:[1 - 0]; x: [1]").unwrap();
   tlc.check(Some(si), "let x:[1 - 1]; x: [0]").unwrap();
   tlc.check(Some(si), "let x:[2 - 1]; x: [1]").unwrap();
   tlc.check(Some(si), "let x:[1 - 2]; x: [-1]").unwrap();
   tlc.check(Some(si), "let x:[2 - 2]; x: [0]").unwrap();
   tlc.check(Some(si), "let x:[5 - 2]; x: [0]").unwrap_err();
   tlc.check(Some(si), "let x:[2 - 5]; x: [1]").unwrap_err();

   tlc.check(Some(si), "let x:[0*0]; x: [0]").unwrap();
   tlc.check(Some(si), "let x:[0*1]; x: [0]").unwrap();
   tlc.check(Some(si), "let x:[1*0]; x: [0]").unwrap();
   tlc.check(Some(si), "let x:[1*1]; x: [1]").unwrap();
   tlc.check(Some(si), "let x:[2*1]; x: [2]").unwrap();
   tlc.check(Some(si), "let x:[1*2]; x: [2]").unwrap();
   tlc.check(Some(si), "let x:[2*2]; x: [4]").unwrap();
   tlc.check(Some(si), "let x:[2*2]; x: [5]").unwrap_err();
   tlc.check(Some(si), "let x:[1*3]; x: [4]").unwrap_err();

   tlc.check(Some(si), "let x:[0/0]; x: [NaN]").unwrap();
   tlc.check(Some(si), "let x:[0/1]; x: [0]").unwrap();
   tlc.check(Some(si), "let x:[1/0]; x: [NaN]").unwrap();
   tlc.check(Some(si), "let x:[1/1]; x: [1]").unwrap();
   tlc.check(Some(si), "let x:[2/1]; x: [2]").unwrap();
   tlc.check(Some(si), "let x:[1/2]; x: [0]").unwrap();
   tlc.check(Some(si), "let x:[2/2]; x: [1]").unwrap();
   tlc.check(Some(si), "let x:[3/2]; x: [1]").unwrap();
   tlc.check(Some(si), "let x:[4/2]; x: [2]").unwrap();
   tlc.check(Some(si), "let x:[5/2]; x: [2]").unwrap();
   tlc.check(Some(si), "let x:[5/0]; x: [2]").unwrap_err();
   tlc.check(Some(si), "let x:[5/2]; x: [3]").unwrap_err();

   tlc.check(Some(si), "let x:[0%0]; x: [NaN]").unwrap();
   tlc.check(Some(si), "let x:[0%1]; x: [0]").unwrap();
   tlc.check(Some(si), "let x:[1%0]; x: [NaN]").unwrap();
   tlc.check(Some(si), "let x:[1%1]; x: [0]").unwrap();
   tlc.check(Some(si), "let x:[2%1]; x: [0]").unwrap();
   tlc.check(Some(si), "let x:[1%2]; x: [1]").unwrap();
   tlc.check(Some(si), "let x:[2%2]; x: [0]").unwrap();
   tlc.check(Some(si), "let x:[3%2]; x: [1]").unwrap();
   tlc.check(Some(si), "let x:[4%2]; x: [0]").unwrap();
   tlc.check(Some(si), "let x:[5%2]; x: [1]").unwrap();
   tlc.check(Some(si), "let x:[5%2]; x: [7]").unwrap_err();
   tlc.check(Some(si), "let x:[0%1]; x: [0]").unwrap();
   tlc.check(Some(si), "let x:[0%0]; x: [0]").unwrap_err();

   tlc.check(Some(si), "let x:[1^0]; x: [1]").unwrap();
   tlc.check(Some(si), "let x:[1^0]; x: [2]").unwrap_err();
   tlc.check(Some(si), "let x:[1^1]; x: [1]").unwrap();
   tlc.check(Some(si), "let x:[1^1]; x: [2]").unwrap_err();
   tlc.check(Some(si), "let x:[1^2]; x: [1]").unwrap();
   tlc.check(Some(si), "let x:[1^2]; x: [2]").unwrap_err();
   tlc.check(Some(si), "let x:[2^0]; x: [1]").unwrap();
   tlc.check(Some(si), "let x:[2^0]; x: [2]").unwrap_err();
   tlc.check(Some(si), "let x:[2^1]; x: [1]").unwrap_err();
   tlc.check(Some(si), "let x:[2^1]; x: [2]").unwrap();
   tlc.check(Some(si), "let x:[2^2]; x: [1]").unwrap_err();
   tlc.check(Some(si), "let x:[2^2]; x: [2]").unwrap_err();
   tlc.check(Some(si), "let x:[2^2]; x: [4]").unwrap();

   tlc.check(Some(si), "let x:[if True then 0 else 2]; x: [0]").unwrap();
   tlc.check(Some(si), "let x:[if True then 0 else 2]; x: [1]").unwrap_err();
   tlc.check(Some(si), "let x:[if False then 0 else 2]; x: [2]").unwrap();
   tlc.check(Some(si), "let x:[if False then 0 else 2]; x: [1]").unwrap_err();
   tlc.check(Some(si), "let x:[if 1 then 0 else 2]; x: [NaN]").unwrap();
   tlc.check(Some(si), "let x:[if True then 0]; x: [0]").unwrap();
   tlc.check(Some(si), "let x:[if True then 0]; x: [NaN]").unwrap_err();
   tlc.check(Some(si), "let x:[if False then 0]; x: [0]").unwrap_err();
   tlc.check(Some(si), "let x:[if False then 0]; x: [()]").unwrap();
}

#[test]
fn check_variable_substitution() {
   let mut tlc = TLC::new();
   let si = tlc.import_file(None, "preludes/si.tlc").unwrap();

   tlc.check(Some(si), "-0: [0]").unwrap();
   tlc.check(Some(si), "-0: [-0]").unwrap();
   tlc.check(Some(si), "-1: [-1]").unwrap();
   tlc.check(Some(si), "-2: [-2]").unwrap();
   tlc.check(Some(si), "-2: [2]").unwrap_err();
   //tlc.check(Some(si), "-(-0): [0]").unwrap();
   //tlc.check(Some(si), "-(-(1)): [1]").unwrap();
   //tlc.check(Some(si), "-(-1): [1]").unwrap();
   //tlc.check(Some(si), "-(-2): [2]").unwrap();
   //tlc.check(Some(si), "-(-1): [-1]").unwrap_err();

   tlc.check(Some(si), "0 + 0: [0]").unwrap();
   tlc.check(Some(si), "0 + 1: [1]").unwrap();
   tlc.check(Some(si), "1 + 0: [1]").unwrap();
   tlc.check(Some(si), "1 + 1: [2]").unwrap();
   tlc.check(Some(si), "2 + 1: [3]").unwrap();
   tlc.check(Some(si), "1 + 2: [3]").unwrap();
   tlc.check(Some(si), "2 + 2: [4]").unwrap();
   tlc.check(Some(si), "1 + 1: [3]").unwrap_err();
   tlc.check(Some(si), "2 + 2: [5]").unwrap_err();

   tlc.check(Some(si), "0 - 0: [0]").unwrap();
   tlc.check(Some(si), "0 - 1: [-1]").unwrap();
   tlc.check(Some(si), "1 - 0: [1]").unwrap();
   tlc.check(Some(si), "1 - 1: [0]").unwrap();
   tlc.check(Some(si), "2 - 1: [1]").unwrap();
   tlc.check(Some(si), "1 - 2: [-1]").unwrap();
   tlc.check(Some(si), "2 - 2: [0]").unwrap();
   tlc.check(Some(si), "5 - 2: [0]").unwrap_err();
   tlc.check(Some(si), "2 - 5: [1]").unwrap_err();

   tlc.check(Some(si), "0*0: [0]").unwrap();
   tlc.check(Some(si), "0*1: [0]").unwrap();
   tlc.check(Some(si), "1*0: [0]").unwrap();
   tlc.check(Some(si), "1*1: [1]").unwrap();
   tlc.check(Some(si), "2*1: [2]").unwrap();
   tlc.check(Some(si), "1*2: [2]").unwrap();
   tlc.check(Some(si), "2*2: [4]").unwrap();
   tlc.check(Some(si), "2*2: [5]").unwrap_err();
   tlc.check(Some(si), "1*3: [4]").unwrap_err();

   tlc.check(Some(si), "0/0: [NaN]").unwrap();
   tlc.check(Some(si), "0/1: [0]").unwrap();
   tlc.check(Some(si), "1/0: [NaN]").unwrap();
   tlc.check(Some(si), "1/1: [1]").unwrap();
   tlc.check(Some(si), "2/1: [2]").unwrap();
   tlc.check(Some(si), "1/2: [0]").unwrap();
   tlc.check(Some(si), "2/2: [1]").unwrap();
   tlc.check(Some(si), "3/2: [1]").unwrap();
   tlc.check(Some(si), "4/2: [2]").unwrap();
   tlc.check(Some(si), "5/2: [2]").unwrap();
   tlc.check(Some(si), "5/0: [2]").unwrap_err();
   tlc.check(Some(si), "5/2: [3]").unwrap_err();

   tlc.check(Some(si), "0%0: [NaN]").unwrap();
   tlc.check(Some(si), "0%1: [0]").unwrap();
   tlc.check(Some(si), "1%0: [NaN]").unwrap();
   tlc.check(Some(si), "1%1: [0]").unwrap();
   tlc.check(Some(si), "2%1: [0]").unwrap();
   tlc.check(Some(si), "1%2: [1]").unwrap();
   tlc.check(Some(si), "2%2: [0]").unwrap();
   tlc.check(Some(si), "3%2: [1]").unwrap();
   tlc.check(Some(si), "4%2: [0]").unwrap();
   tlc.check(Some(si), "5%2: [1]").unwrap();
   tlc.check(Some(si), "5%2: [7]").unwrap_err();
   tlc.check(Some(si), "0%1: [NaN]").unwrap_err();
   tlc.check(Some(si), "0%0: [0]").unwrap_err();
}
