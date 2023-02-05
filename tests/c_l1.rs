/* TODO FIXME update this file to work with current L1 prelude
use lsts::tlc::TLC;

#[test]
fn l1_literals() {
   let mut tlc = TLC::new();
   let l1 = tlc.import_file(None, "preludes/l1.tlc").unwrap();

   tlc.check(Some(l1), "0: Integer;").unwrap();
   tlc.check(Some(l1), "0: Whole;").unwrap();
   tlc.check(Some(l1), "1: Integer;").unwrap();
   tlc.check(Some(l1), "1: Whole;").unwrap();
   tlc.check(Some(l1), "-1: Integer;").unwrap();
   tlc.check(Some(l1), "-1: Whole;").unwrap_err();
}

#[test]
fn l1_functions() {
   let mut tlc = TLC::new();
   let l1 = tlc.import_file(None, "preludes/l1.tlc").unwrap();

   tlc.check(Some(l1), "let f(x:Integer): Integer = x; f(1);").unwrap();
   tlc.check(Some(l1), "let f(x:Whole): Whole = x; f(1:Whole);").unwrap();
   tlc.check(Some(l1), "let f(x:Integer): Integer = x; f(-1);").unwrap();
   tlc.check(Some(l1), "let f(x:Whole): Whole = x; f(-1);").unwrap_err();
   tlc.check(Some(l1), "let f(x:Whole): Integer = x; f(-1);").unwrap_err();
}

#[test]
fn l1_dot_functions() {
   let mut tlc = TLC::new();
   let l1 = tlc.import_file(None, "preludes/l1.tlc").unwrap();

   tlc.check(Some(l1), "let .f(x:Integer): Integer = x; (1).f: Integer;").unwrap();
   tlc.check(Some(l1), "let .f(x:Integer, y:Integer): Integer = x; (1).f(2): Integer;").unwrap();
   tlc.check(Some(l1), "let .f(x:Integer, y:Integer, z:Integer): Integer = x; (1).f(2,3): Integer;").unwrap();
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

   tlc.check(Some(l1), "(): Integer[0];").unwrap();
   tlc.check(Some(l1), "(1): Integer[0];").unwrap_err();
   tlc.check(Some(l1), "(1): Integer;").unwrap();
   tlc.check(Some(l1), "(1,): Integer[0];").unwrap_err();
   tlc.check(Some(l1), "(1,): Integer[1];").unwrap();
   tlc.check(Some(l1), "(1,2): Integer[1];").unwrap_err();
   tlc.check(Some(l1), "(1,2): Integer[];").unwrap();
   tlc.check(Some(l1), "(1,2): Integer[2];").unwrap();
   tlc.check(Some(l1), "(1,2,3): Integer[2];").unwrap_err();
   tlc.check(Some(l1), "(1,2,3): Integer[3];").unwrap();
   tlc.check(Some(l1), "((1,2,3): Integer[3]).0 @reduce :[1];").unwrap();
   tlc.check(Some(l1), "((1,2,3): Integer[3]).1 @reduce :[2];").unwrap();
   tlc.check(Some(l1), "((1,2,3): Integer[3]).2 @reduce :[3];").unwrap();
   tlc.check(Some(l1), "((1,2,3): Integer[3])[-1] @reduce :[1];").unwrap_err();
   tlc.check(Some(l1), "((1,2,3): Integer[3])[0] @reduce :[1];").unwrap();
   tlc.check(Some(l1), "((1,2,3): Integer[3])[1] @reduce :[2];").unwrap();
   tlc.check(Some(l1), "((1,2,3): Integer[3])[2] @reduce :[3];").unwrap();
   tlc.check(Some(l1), "((1,2,3): Integer[3])[3] @reduce :[3];").unwrap_err();
   tlc.check(Some(l1), "((1,2,3): Integer[])[2] @reduce :[3];").unwrap();
   tlc.check(Some(l1), "().length @reduce :[0];").unwrap();
   tlc.check(Some(l1), "(3,).length @reduce :[1];").unwrap();
   tlc.check(Some(l1), "(3,4).length @reduce :[2];").unwrap();
   tlc.check(Some(l1), "(3,4,7).length @reduce :[3];").unwrap();
   tlc.check(Some(l1), "((3,4,7): Integer[]).length @reduce :[3];").unwrap();
   tlc.check(Some(l1), "(+((1,),(),(2,3,)): Integer[]).length @reduce :[3];").unwrap();
}

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
