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

#[test]
fn destructure_literal() {
   let mut tlc = TLC::new();
   let l1 = tlc.import_file(None, "preludes/l1.tlc").unwrap();

   tlc.check(Some(l1), r#"match 0 { literal '0' => literal '0' } : Integer @reduce : [0];"#).unwrap();
   tlc.check(Some(l1), r#"match 0 { literal '0' => literal '0' } : Integer @reduce : [00];"#).unwrap_err();
   tlc.check(Some(l1), r#"match 0 { literal '0'a => literal a } : Integer @reduce : [0];"#).unwrap();
   tlc.check(Some(l1), r#"match 00 { literal "00" => literal "00" } : Integer @reduce : [0];"#).unwrap_err();
   tlc.check(Some(l1), r#"match 00 { literal "00" => literal "00" } : Integer @reduce : [00];"#).unwrap();
   tlc.check(Some(l1), r#"match 00 { literal "00"a => literal a } : Integer @reduce : [00];"#).unwrap();
   tlc.check(Some(l1), r#"match 00 { literal '0' a => literal '0' a } : Integer @reduce : [00];"#).unwrap();
   tlc.check(Some(l1), r#"match 00 { literal '0' a '0' => literal '0' a '0' } : Integer @reduce : [00];"#).unwrap();
   tlc.check(Some(l1), r#"match 000 { literal '0' a '0' => literal '0' a '0' } : Integer @reduce : [000];"#).unwrap();
   tlc.check(Some(l1), r#"match 0 { literal [0-9]a => literal a } : Integer @reduce : [0];"#).unwrap();
   tlc.check(Some(l1), r#"match 0 { literal [0-9]a b => literal a b } : Integer @reduce : [0];"#).unwrap();
   tlc.check(Some(l1), r#"match 00 { literal [0-9]a b [0-9]c => literal a b c } : Integer @reduce : [00];"#).unwrap();
   tlc.check(Some(l1), r#"match 000 { literal [0-9]a b [0-9]c => literal a b c } : Integer @reduce : [000];"#).unwrap();
}
