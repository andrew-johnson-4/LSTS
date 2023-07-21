use lsts::tlc::TLC;
use lsts::constant::Constant;

#[test]
fn one_plus_one() {
   let mut tlc = TLC::new();
   let val = tlc.reduce_file(None, "examples/one_plus_one.tlc").unwrap();
   assert_eq!( val, Constant::parse(&tlc, "2").unwrap() );
}

#[test]
fn adder() {
   let mut tlc = TLC::new();
   let val = tlc.reduce_file(None, "examples/adder.tlc").unwrap();
   assert_eq!( val, Constant::parse(&tlc, "126").unwrap() );
}

#[test]
fn fibonacci() {
   let mut tlc = TLC::new();
   let val = tlc.reduce_file(None, "examples/fibonacci.tlc").unwrap();
   assert_eq!( val, Constant::parse(&tlc, "75025").unwrap() );
}

#[test]
fn tuples() {
   let mut tlc = TLC::new();
   let val = tlc.reduce_file(None, "examples/tuples.tlc").unwrap();
   assert_eq!( format!("{:?}",val), "(4,48,92,136,180,224,268,312,356)" );
}

#[test]
fn right_triangles() {
   let mut tlc = TLC::new();
   let val = tlc.reduce_file(None, "examples/right_triangles.tlc").unwrap();
   assert_eq!( format!("{:?}",val), "((6,8,10))" );
}

#[test]
fn string_formatting() {
   let mut tlc = TLC::new();
   let val = tlc.reduce_file(None, "examples/string_formatting.tlc").unwrap();
   assert_eq!( format!("{:?}",val), r#""[0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24]""# );
}

/* TODO FIXME reintroduce SI prelude as a mixin
#[test]
fn si() {
   let mut tlc = TLC::new();
   let val = tlc.reduce_file(None, "examples/si.tlc").unwrap();
   assert_eq!( val, Constant::parse(&tlc, "1.23").unwrap() );
}
*/
