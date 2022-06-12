use lsts::tlc::TLC;

#[test]
fn check_constant_literals() {
   //soft cast literals into typed values
   let mut tlc = TLC::new();
   let si = tlc.compile_file(None, "preludes/si.tlc").unwrap();
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
   //Ground Units unify with themselves
   let mut tlc = TLC::new();
   let si = tlc.compile_file(None, "preludes/si.tlc").unwrap();
   tlc.check(Some(si), "let x:Second=1:Real; let y:Second=x;").unwrap();

   let mut tlc = TLC::new();
   let si = tlc.compile_file(None, "preludes/si.tlc").unwrap();
   tlc.check(Some(si), "let x:Metre=2:Real; let y:Metre=x;").unwrap();

   let mut tlc = TLC::new();
   let si = tlc.compile_file(None, "preludes/si.tlc").unwrap();
   tlc.check(Some(si), "let x:Gram=3:Real; let y:Gram=x;").unwrap();

   let mut tlc = TLC::new();
   let si = tlc.compile_file(None, "preludes/si.tlc").unwrap();
   tlc.check(Some(si), "let x:Ampere=1.2:Real; let y:Ampere=x;").unwrap();

   let mut tlc = TLC::new();
   let si = tlc.compile_file(None, "preludes/si.tlc").unwrap();
   tlc.check(Some(si), "let x:Kelvin=2.3:Real; let y:Kelvin=x;").unwrap();

   let mut tlc = TLC::new();
   let si = tlc.compile_file(None, "preludes/si.tlc").unwrap();
   tlc.check(Some(si), "let x:Mole=3.1:Real; let y:Mole=x;").unwrap();

   let mut tlc = TLC::new();
   let si = tlc.compile_file(None, "preludes/si.tlc").unwrap();
   tlc.check(Some(si), "let x:Candela=3.2:Real; let y:Candela=x;").unwrap();

   //Ground Units unify with compatible Numbers
   let mut tlc = TLC::new();
   let si = tlc.compile_file(None, "preludes/si.tlc").unwrap();
   tlc.check(Some(si), "let x:Metre=1:Integer; let y:Integer=x;").unwrap();
   let mut tlc = TLC::new();
   let si = tlc.compile_file(None, "preludes/si.tlc").unwrap();
   tlc.check(Some(si), "let x:Metre=1:Integer; let y:Real=x;").unwrap();
   let mut tlc = TLC::new();
   let si = tlc.compile_file(None, "preludes/si.tlc").unwrap();
   tlc.check(Some(si), "let x:Metre=1:Integer; let y:Complex=x;").unwrap();

   let mut tlc = TLC::new();
   let si = tlc.compile_file(None, "preludes/si.tlc").unwrap();
   tlc.check(Some(si), "let x:Metre=1.2:Real; let y:Integer=x;").unwrap_err();
   let mut tlc = TLC::new();
   let si = tlc.compile_file(None, "preludes/si.tlc").unwrap();
   tlc.check(Some(si), "let x:Metre=1.2:Real; let y:Real=x;").unwrap();
   let mut tlc = TLC::new();
   let si = tlc.compile_file(None, "preludes/si.tlc").unwrap();
   tlc.check(Some(si), "let x:Metre=1.2:Real; let y:Complex=x;").unwrap();

   let mut tlc = TLC::new();
   let si = tlc.compile_file(None, "preludes/si.tlc").unwrap();
   tlc.check(Some(si), "let x:Metre=1.2+3i:Complex; let y:Integer=x;").unwrap_err();
   let mut tlc = TLC::new();
   let si = tlc.compile_file(None, "preludes/si.tlc").unwrap();
   tlc.check(Some(si), "let x:Metre=1.2+3i:Complex; let y:Real=x;").unwrap_err();
   let mut tlc = TLC::new();
   let si = tlc.compile_file(None, "preludes/si.tlc").unwrap();
   tlc.check(Some(si), "let x:Metre=1.2+3i:Complex; let y:Complex=x;").unwrap();

   //Ground Units do not unify with different Ground Units
   let mut tlc = TLC::new();
   let si = tlc.compile_file(None, "preludes/si.tlc").unwrap();
   tlc.check(Some(si), "let x:Metre=2:Integer; let y:Second=x;").unwrap_err();
}
