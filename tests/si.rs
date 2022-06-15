use lsts::tlc::TLC;

#[test]
fn check_constant_literals() {
   let mut tlc = TLC::new();
   let si = tlc.compile_file(None, "preludes/si.tlc").unwrap();

   //soft cast literals into typed values
   tlc.check(Some(si), "True:Boolean").unwrap();
   tlc.check(Some(si), "False:Boolean").unwrap();
   tlc.check(Some(si), "1:Boolean").unwrap_err();

   tlc.check(Some(si), "0:Integer").unwrap();
   tlc.check(Some(si), "321:Integer").unwrap();
   tlc.check(Some(si), "3e2:Integer").unwrap();
   tlc.check(Some(si), "3.2:Integer").unwrap_err();
   tlc.check(Some(si), "3+1i:Integer").unwrap_err();
   tlc.check(Some(si), "True:Integer").unwrap_err();

   tlc.check(Some(si), "0:Real").unwrap();
   tlc.check(Some(si), "321:Real").unwrap();
   tlc.check(Some(si), "3e2:Real").unwrap();
   tlc.check(Some(si), "3.2:Real").unwrap();
   tlc.check(Some(si), "3+1i:Real").unwrap_err();
   tlc.check(Some(si), "3.2+1i:Real").unwrap_err();
   tlc.check(Some(si), "True:Real").unwrap_err();

   tlc.check(Some(si), "0:Complex").unwrap();
   tlc.check(Some(si), "321:Complex").unwrap();
   tlc.check(Some(si), "3e2:Complex").unwrap();
   tlc.check(Some(si), "3.2:Complex").unwrap();
   tlc.check(Some(si), "3+1i:Complex").unwrap();
   tlc.check(Some(si), "3.2+1i:Complex").unwrap();
   tlc.check(Some(si), "True:Complex").unwrap_err();
}

#[test]
fn check_type_equality() {
   let mut tlc = TLC::new();
   let si = tlc.compile_file(None, "preludes/si.tlc").unwrap();

   //Ground Types unify with themselves
   tlc.check(Some(si), "let x:Integer=1:Integer;").unwrap();
   tlc.check(Some(si), "let x:Real=1:Real;").unwrap();
   tlc.check(Some(si), "let x:Complex=1:Complex;").unwrap();

   /*
   //Ground Types unify with other Types when a viable cast rule is available
   tlc.check(Some(si), "let x:Real=1:Integer;").unwrap();
   tlc.check(Some(si), "let x:Complex=1:Integer;").unwrap();
   tlc.check(Some(si), "let x:Complex=1:Real;").unwrap();
   */

   //Ground Types do not unify if no cast rule is available
   tlc.check(Some(si), "let x:Integer=1:Real;").unwrap_err();
   tlc.check(Some(si), "let x:Integer=1:Complex;").unwrap_err();
   tlc.check(Some(si), "let x:Real=1:Complex;").unwrap_err();
}

#[test]
fn check_kinded_type_equality() {
   //TODO check Units unify and persist
}
