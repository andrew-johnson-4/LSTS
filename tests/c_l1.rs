use lsts::tlc::TLC;

#[test]
fn l1_literals() {
   let mut tlc = TLC::new();
   let l1 = tlc.import_file(None, "preludes/l1.tlc").unwrap();

   tlc.check(Some(l1), "0: I64;").unwrap();
   tlc.check(Some(l1), "0: I64;").unwrap();
   tlc.check(Some(l1), "1: I64;").unwrap();
   tlc.check(Some(l1), "1: I64;").unwrap();
   tlc.check(Some(l1), "-1: I64;").unwrap();
   tlc.check(Some(l1), "-1: U64;").unwrap_err();
}

#[test]
fn l1_functions() {
   let mut tlc = TLC::new();
   let l1 = tlc.import_file(None, "preludes/l1.tlc").unwrap();

   tlc.check(Some(l1), "let f(x:I64): I64 = x; f(1:I64);").unwrap();
   tlc.check(Some(l1), "let f(x:I64): I64 = x; f(1:I64);").unwrap();
   tlc.check(Some(l1), "let f(x:I64): I64 = x; f(-1);").unwrap();
   tlc.check(Some(l1), "let f(x:I64): I64 = x; f(-1);").unwrap();
   tlc.check(Some(l1), "let f(x:I64): I64 = x; f(-1);").unwrap();
}

#[test]
fn l1_dot_functions() {
   let mut tlc = TLC::new();
   let l1 = tlc.import_file(None, "preludes/l1.tlc").unwrap();

   tlc.check(Some(l1), "let .f(x:I64): I64 = x; (1:I64).f: I64;").unwrap();
   tlc.check(Some(l1), "let .f(x:I64, y:I64): I64 = x; (1:I64).f(2:I64): I64;").unwrap();
   tlc.check(Some(l1), "let .f(x:I64, y:I64, z:I64): I64 = x; (1:I64).f(2:I64,3:I64): I64;").unwrap();
}

#[test]
fn l1_reduce() {
   let mut tlc = TLC::new();
   let l1 = tlc.import_file(None, "preludes/l1.tlc").unwrap();

   tlc.check(Some(l1), "- 0 @reduce;").unwrap();
   tlc.check(Some(l1), "- 1 @reduce;").unwrap();
   tlc.check(Some(l1), "-1 @reduce;").unwrap();
   tlc.check(Some(l1), "- -1 @reduce;").unwrap();
}

#[test]
fn l1_homogenous_tuples() {
   let mut tlc = TLC::new();
   let l1 = tlc.import_file(None, "preludes/l1.tlc").unwrap();

   tlc.check(Some(l1), "(): I64[0];").unwrap();
   tlc.check(Some(l1), "(1): I64[0];").unwrap_err();
   tlc.check(Some(l1), "(1): I64;").unwrap();
   tlc.check(Some(l1), "(1,): I64[0];").unwrap_err();
   tlc.check(Some(l1), "(1,): I64[1];").unwrap();
   tlc.check(Some(l1), "(1,2): I64[1];").unwrap_err();
   tlc.check(Some(l1), "(1,2): I64[];").unwrap();
   tlc.check(Some(l1), "(1,2): I64[2];").unwrap();
   tlc.check(Some(l1), "(1,2,3): I64[2];").unwrap_err();
   tlc.check(Some(l1), "(1,2,3): I64[3];").unwrap();
   tlc.check(Some(l1), "((1,2,3): I64[3]).0 @reduce :[1];").unwrap();
   tlc.check(Some(l1), "((1,2,3): I64[3]).1 @reduce :[2];").unwrap();
   tlc.check(Some(l1), "((1,2,3): I64[3]).2 @reduce :[3];").unwrap();
   tlc.check(Some(l1), "((1,2,3): I64[3])[0] @reduce :[1];").unwrap();
   tlc.check(Some(l1), "((1,2,3): I64[3])[1] @reduce :[2];").unwrap();
   tlc.check(Some(l1), "((1,2,3): I64[3])[2] @reduce :[3];").unwrap();
   tlc.check(Some(l1), "((1,2,3): I64[])[2] @reduce :[3];").unwrap();
   tlc.check(Some(l1), "().length @reduce :[0];").unwrap();
   tlc.check(Some(l1), "(3,).length @reduce :[1];").unwrap();
   tlc.check(Some(l1), "(3,4).length @reduce :[2];").unwrap();
   tlc.check(Some(l1), "(3,4,7).length @reduce :[3];").unwrap();
   tlc.check(Some(l1), "((3,4,7): I64[]).length @reduce :[3];").unwrap();
   //tlc.check(Some(l1), "((1,),(),(2,3,)).flatten() @reduce :[(1,2,3)];").unwrap();
   //tlc.check(Some(l1), "((1,),(),(2,3,)).flatten().length @reduce :[3];").unwrap();
}

/* TODO FIXME destructure tuples
#[test]
fn l1_destructure_tuples() {
   let mut tlc = TLC::new();
   let l1 = tlc.import_file(None, "preludes/l1.tlc").unwrap();

   tlc.check(Some(l1), "match () { () => 1 }  @reduce :[1];").unwrap();
   tlc.check(Some(l1), "match () { (x,) => 1 }  @reduce :[1];").unwrap_err();
   tlc.check(Some(l1), "match () { (x,y) => 1 }  @reduce :[1];").unwrap_err();
   tlc.check(Some(l1), "match (3,) { () => 1 }  @reduce :[1];").unwrap_err();
   tlc.check(Some(l1), "match (3,) { (x) => 1 }  @reduce :[1];").unwrap();
   tlc.check(Some(l1), "match (3,) { (x,y) => 1 }  @reduce :[1];").unwrap_err();
   tlc.check(Some(l1), "match (3,4) { () => 1 }  @reduce :[1];").unwrap_err();
   tlc.check(Some(l1), "match (3,4) { (x,) => 1 }  @reduce :[1];").unwrap_err();
   tlc.check(Some(l1), "match (3,4) { (x,y) => 1 }  @reduce :[1];").unwrap();
   tlc.check(Some(l1), "match (3,4) { (x,y) => x }  @reduce :[3];").unwrap();
}
*/

/* TODO FIXME update tuple LHS syntax
#[test]
fn l1_partial_tuples() {
   let mut tlc = TLC::new();
   let l1 = tlc.import_file(None, "preludes/l1.tlc").unwrap();

   tlc.check(Some(l1), "match (3,4) { +((x,),y) => x }  @reduce :[3];").unwrap();
   tlc.check(Some(l1), "match (3,4) { +(x,(y,)) => y }  @reduce :[4];").unwrap();
   tlc.check(Some(l1), "match (3,4) { +((3,),(y,)) => 5 }  @reduce :[5];").unwrap();
   tlc.check(Some(l1), "match (3,4) { +((3,),x,(y,)) => 5 }  @reduce :[5];").unwrap();
   tlc.check(Some(l1), "match (3,4) { +((3,),(y,)) => 5 }  @reduce :[5];").unwrap();
}
*/
