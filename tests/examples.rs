use lsts::tlc::TLC;
use lsts::constant::Constant;

#[test]
fn right_triangles() {
   println!("current directory: {:?}", std::env::current_dir().unwrap());

   let mut tlc = TLC::new();
   let val = tlc.reduce_file(None, "examples/right_triangles.tlc").unwrap();
   assert!( val == Constant::parse(&tlc, "((6,8,10))").unwrap() );
}
