//TODO FIXME: use strict mode
use lsts::tlc::TLC;

#[test]
fn check_sqrt_irrationality() {
   let mut tlc = TLC::new();
   let sa = tlc.import_str(None, r#"
let $"/"(x:X,y:Y):X/Y;
let $"*"(x:X,y:Y):X*Y;
let square(x:X):X*X;              "#).unwrap();

   //prove that sqrt(2) is irrational
   tlc.check(Some(sa), r#"
type Pt; let p:Pt;
type Qt; let q:Qt;
let sqrt_of_two: Pt/Qt;
square(sqrt_of_two) * square(q): Pt*Pt; //2 * q*q = p*p
square(p) / square(sqrt_of_two): Qt*Qt; //p*p / 2 = q*q
p / square(sqrt_of_two) : ?/();         //2 is a factor of p
"#).unwrap_err();

   //assert that proof is OK until absurd final statement
   tlc.check(Some(sa), r#"
type Pt; let p:Pt;
type Qt; let q:Qt;
let sqrt_of_two: Pt/Qt;
square(sqrt_of_two) * square(q): Pt*Pt; //2 * q*q = p*p
square(p) / square(sqrt_of_two): Qt*Qt; //p*p / 2 = q*q
"#).unwrap();
}
