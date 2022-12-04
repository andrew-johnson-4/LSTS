use lsts::tlc::TLC;

#[test]
fn check_match_literal() {
   let mut tlc = TLC::new();
   let l1 = tlc.import_file(None, "preludes/l1.tlc").unwrap();

   tlc.check(Some(l1), "match 1 { 1 => 1, 2 => 2, _ => 3 } @reduce : [1];").unwrap();
   tlc.check(Some(l1), "match 2 { 1 => 1, 2 => 2, _ => 3 } @reduce : [2];").unwrap();
   tlc.check(Some(l1), "match 3 { 1 => 1, 2 => 2, _ => 3 } @reduce : [3];").unwrap();
   tlc.check(Some(l1), "match 3 { 1 => 1, 2 => 2, _ => 3 } @reduce : [4];").unwrap_err();
}

#[test]
fn check_iflet_literal() {
   let mut tlc = TLC::new();
   let l1 = tlc.import_file(None, "preludes/l1.tlc").unwrap();

   tlc.check(Some(l1), "(if let 1 = 1 then 1 else 2) @reduce : [1];").unwrap();
   tlc.check(Some(l1), "(if let 1 = 2 then 1 else 2) @reduce : [2];").unwrap();
   tlc.check(Some(l1), "(if let 1 = 1 then 1 else 2) @reduce : [3];").unwrap_err();
}
