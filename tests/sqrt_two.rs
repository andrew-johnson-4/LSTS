use lsts::tlc::TLC;

#[test]
fn check_constant_literals() {
   let mut tlc = TLC::new();
   let sa = tlc.import_str(None, r#"
let $"/"(x:X,y:Y):X/Y;
let $"*"(x:X,y:Y):X*Y;
let square(x:X):X*X;              "#).unwrap();

   //prove that sqrt(2) is irrational
   tlc.check(Some(sa), r#"
type P;
type Q; let q:Q;
let sqrt_of_two: P/Q;
square(sqrt_of_two) * square(q): P*P "#).unwrap_err();
}
