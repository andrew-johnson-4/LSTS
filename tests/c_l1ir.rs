use lsts::tlc::TLC;

#[test]
fn l1ir_u64() {
   let mut tlc = TLC::new();
   let l1 = tlc.import_file(None, "preludes/l1.tlc").unwrap();

   tlc.check(Some(l1), "(0:U64) + (0:U64) @reduce :[0];").unwrap();
   tlc.check(Some(l1), "(0:U64) + (1:U64) @reduce :[1];").unwrap();
   tlc.check(Some(l1), "(1:U64) + (0:U64) @reduce :[1];").unwrap();
   tlc.check(Some(l1), "(1:U64) + (2:U64) @reduce :[3];").unwrap();
}
