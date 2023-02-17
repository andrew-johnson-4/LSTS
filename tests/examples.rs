use lsts::tlc::TLC;
use lsts::constant::Constant;

#[test]
fn one_plus_one() {
   println!("current directory: {:?}", std::env::current_dir().unwrap());

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
   println!("current directory: {:?}", std::env::current_dir().unwrap());

   let mut tlc = TLC::new();
   let val = tlc.reduce_file(None, "examples/fibonacci.tlc").unwrap();
   assert_eq!( val, Constant::parse(&tlc, "75025").unwrap() );
}

#[test]
fn tuples() {
   println!("current directory: {:?}", std::env::current_dir().unwrap());

   let mut tlc = TLC::new();
   let val = tlc.reduce_file(None, "examples/tuples.tlc").unwrap();
   assert_eq!( val, Constant::parse(&tlc, "(4,48,92,136,180,224,268,312,356)").unwrap() );
}

#[test]
fn right_triangles() {
   println!("current directory: {:?}", std::env::current_dir().unwrap());

   let mut tlc = TLC::new();
   let val = tlc.reduce_file(None, "examples/right_triangles.tlc").unwrap();
   assert_eq!( val, Constant::parse(&tlc, "((6,8,10))").unwrap() );
}

#[test]
fn string_formatting() {
   println!("current directory: {:?}", std::env::current_dir().unwrap());

   let mut tlc = TLC::new();
   let val = tlc.reduce_file(None, "examples/string_formatting.tlc").unwrap();
   assert_eq!( val, Constant::parse(&tlc, r#""[0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24]""#).unwrap() );
}
