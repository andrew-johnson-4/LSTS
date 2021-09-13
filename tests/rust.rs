
#[test]
fn rustly_typed() {
   lsts::rust::typecheck_file("tests/rust/ast1.rs");
   lsts::rust::typecheck_file("tests/rust/ast2.rs");
}
