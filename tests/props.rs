use lsts::tlc::TLC;

#[test]
fn check_boolean() {
   let mut tlc = TLC::new();
   let si = tlc.import_file(None, "preludes/si.tlc").unwrap();

   //Boolean Proofs
   tlc.check(Some(si), "True:True").unwrap();
   tlc.check(Some(si), "True:False").unwrap_err();
   tlc.check(Some(si), "False:False").unwrap();
   tlc.check(Some(si), "False:True").unwrap_err();

   tlc.check(Some(si), "not(True) as False").unwrap();
   tlc.check(Some(si), "not(True) as True").unwrap_err();
   tlc.check(Some(si), "not(False) as False").unwrap_err();
   tlc.check(Some(si), "not(False) as True").unwrap();

   /*
   tlc.check(Some(si), "True && True as True").unwrap();
   tlc.check(Some(si), "True && False as True").unwrap_err();
   tlc.check(Some(si), "False && True as True").unwrap_err();
   tlc.check(Some(si), "False && False as True").unwrap_err();
   tlc.check(Some(si), "True && True as False").unwrap_err();
   tlc.check(Some(si), "True && False as False").unwrap();
   tlc.check(Some(si), "False && True as False").unwrap();
   tlc.check(Some(si), "False && False as False").unwrap();

   tlc.check(Some(si), "True || True as True").unwrap();
   tlc.check(Some(si), "True || False as True").unwrap();
   tlc.check(Some(si), "False || True as True").unwrap();
   tlc.check(Some(si), "False || False as True").unwrap_err();
   tlc.check(Some(si), "True || True as False").unwrap_err();
   tlc.check(Some(si), "True || False as False").unwrap_err();
   tlc.check(Some(si), "False || True as False").unwrap_err();
   tlc.check(Some(si), "False || False as False").unwrap();
   */
}

#[test]
fn check_contradictions() {
   let mut tlc = TLC::new();
   let si = tlc.import_file(None, "preludes/si.tlc").unwrap();

   //Boolean Soundness
   //True and False are constructors of the Boolean type
   //it is therefore unsound to have a term that is both :True and :False
   tlc.check(Some(si), "let a:True").unwrap();
   tlc.check(Some(si), "let a:False").unwrap();
   tlc.check(Some(si), "let a:True+False").unwrap_err();
}
