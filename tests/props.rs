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

   tlc.check(Some(si), "not(True):False").unwrap();
   tlc.check(Some(si), "not(True):True").unwrap_err();
   tlc.check(Some(si), "not(False):False").unwrap_err();
   tlc.check(Some(si), "not(False):True").unwrap();

   tlc.check(Some(si), "True && True:True").unwrap();
   tlc.check(Some(si), "True && False:True").unwrap_err();
   tlc.check(Some(si), "False && True:True").unwrap_err();
   tlc.check(Some(si), "False && False:True").unwrap_err();
   tlc.check(Some(si), "True && True:False").unwrap_err();
   tlc.check(Some(si), "True && False:False").unwrap();
   tlc.check(Some(si), "False && True:False").unwrap();
   tlc.check(Some(si), "False && False:False").unwrap();

   tlc.check(Some(si), "True || True:True").unwrap();
   tlc.check(Some(si), "True || False:True").unwrap();
   tlc.check(Some(si), "False || True:True").unwrap();
   tlc.check(Some(si), "False || False:True").unwrap_err();
   tlc.check(Some(si), "True || True:False").unwrap_err();
   tlc.check(Some(si), "True || False:False").unwrap_err();
   tlc.check(Some(si), "False || True:False").unwrap_err();
   tlc.check(Some(si), "False || False:False").unwrap();
}

#[test]
fn check_contradictions() {
   let mut tlc = TLC::new();
   let si = tlc.import_file(None, "preludes/si.tlc").unwrap();

   //Boolean Soundness
   tlc.check(Some(si), "let a:True").unwrap();
   tlc.check(Some(si), "let a:False").unwrap();
   tlc.check(Some(si), "let a:True+False").unwrap_err();
}
