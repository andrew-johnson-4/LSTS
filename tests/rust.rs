
#[test]
fn rustly_typed() {
   assert!(lsts::rust::typecheck_file("tests/rust/ast1.rs").is_ok());
   assert!(lsts::rust::typecheck_file("tests/rust/ast2.rs").is_ok());
}

#[test]
fn rustly_blamed() {
   assert!(lsts::rust::typecheck_file("tests/rust/blame/move1.rs").is_err());
   //fn main() {
   //   let x = 5;
   //   let y = x;
   //   let z = x;
   //}
   //
   //YIELDS something like
   //
   //main: () -> ()
   //main:x: integer
   //main:y: x
   //close main:x
   //main:z: z
   //close main:x
}

