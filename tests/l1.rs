use lsts::tlc::TLC;

#[test]
fn l1_homogenous_tuples() {
   let mut tlc = TLC::new();
   let alg = tlc.import_file(None, "preludes/l1.tlc").unwrap();

   tlc.check(Some(alg), "(): Integer[0];").unwrap();
   tlc.check(Some(alg), "(1): Integer[0];").unwrap_err();
   tlc.check(Some(alg), "(1): Integer;").unwrap();
   tlc.check(Some(alg), "(1,): Integer[0];").unwrap_err();
   tlc.check(Some(alg), "(1,): Integer[1];").unwrap();
   tlc.check(Some(alg), "(1,2): Integer[1];").unwrap_err();
   tlc.check(Some(alg), "(1,2): Integer[];").unwrap();
   tlc.check(Some(alg), "(1,2): Integer[2];").unwrap();
   tlc.check(Some(alg), "(1,2,3): Integer[2];").unwrap_err();
   tlc.check(Some(alg), "(1,2,3): Integer[3];").unwrap();
   tlc.check(Some(alg), "((1,2,3): Integer[3]).0 @reduce :[1];").unwrap();
   tlc.check(Some(alg), "((1,2,3): Integer[3]).1 @reduce :[2];").unwrap();
   tlc.check(Some(alg), "((1,2,3): Integer[3]).2 @reduce :[3];").unwrap();
   tlc.check(Some(alg), "((1,2,3): Integer[3])[-1] @reduce :[1];").unwrap_err();
   tlc.check(Some(alg), "((1,2,3): Integer[3])[0] @reduce :[1];").unwrap();
   tlc.check(Some(alg), "((1,2,3): Integer[3])[1] @reduce :[2];").unwrap();
   tlc.check(Some(alg), "((1,2,3): Integer[3])[2] @reduce :[3];").unwrap();
   tlc.check(Some(alg), "((1,2,3): Integer[3])[3] @reduce :[3];").unwrap_err();
   tlc.check(Some(alg), "((1,2,3): Integer[])[2] @reduce :[3];").unwrap();
   tlc.check(Some(alg), "().length @reduce :[0];").unwrap();
   tlc.check(Some(alg), "(3,).length @reduce :[1];").unwrap();
   tlc.check(Some(alg), "(3,4).length @reduce :[2];").unwrap();
   tlc.check(Some(alg), "(3,4,7).length @reduce :[3];").unwrap();
   tlc.check(Some(alg), "((3,4,7): Integer[]).length @reduce :[3];").unwrap();
   tlc.check(Some(alg), "(+((1,),(),(2,3,)): Integer[]).length @reduce :[3];").unwrap();
   tlc.check(Some(alg), "match () { () => 1 }  @reduce :[1];").unwrap();
   tlc.check(Some(alg), "match () { (x,) => 1 }  @reduce :[1];").unwrap_err();
   tlc.check(Some(alg), "match () { (x,y) => 1 }  @reduce :[1];").unwrap_err();
   tlc.check(Some(alg), "match (3,) { () => 1 }  @reduce :[1];").unwrap_err();
   tlc.check(Some(alg), "match (3,) { (x) => 1 }  @reduce :[1];").unwrap();
   tlc.check(Some(alg), "match (3,) { (x,y) => 1 }  @reduce :[1];").unwrap_err();
   tlc.check(Some(alg), "match (3,4) { () => 1 }  @reduce :[1];").unwrap_err();
   tlc.check(Some(alg), "match (3,4) { (x,) => 1 }  @reduce :[1];").unwrap_err();
   tlc.check(Some(alg), "match (3,4) { (x,y) => 1 }  @reduce :[1];").unwrap();
   tlc.check(Some(alg), "match (3,4) { (x,y) => x }  @reduce :[3];").unwrap();
   tlc.check(Some(alg), "match (3,4) { +((x,),y) => x }  @reduce :[3];").unwrap();
   tlc.check(Some(alg), "match (3,4) { +(x,(y,)) => x }  @reduce :[4];").unwrap();
   tlc.check(Some(alg), "match (3,4) { +((3,),(y,)) => 5 }  @reduce :[5];").unwrap();
   tlc.check(Some(alg), "match (3,4) { +((3,),x,(y,)) => 5 }  @reduce :[5];").unwrap();
   tlc.check(Some(alg), "match (3,4) { +((3,),(),(y,)) => 5 }  @reduce :[5];").unwrap();
}
