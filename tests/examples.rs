use lsts::tlc::TLC;
use lsts::constant::Constant;

/*
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
*/

#[test]
fn tuples() {
   println!("current directory: {:?}", std::env::current_dir().unwrap());

   let mut tlc = TLC::new();
   let val = tlc.reduce_file(None, "examples/tuples.tlc").unwrap();
   assert_eq!( val, Constant::parse(&tlc, "((6,8,10))").unwrap() );
}

/*
#[test]
fn right_triangles() {
   println!("current directory: {:?}", std::env::current_dir().unwrap());

   let mut tlc = TLC::new();
   let val = tlc.reduce_file(None, "examples/right_triangles.tlc").unwrap();
   assert_eq!( val, Constant::parse(&tlc, "((6,8,10))").unwrap() );
}
*/
