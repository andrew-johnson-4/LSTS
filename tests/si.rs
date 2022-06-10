use lsts::tlc::TLC;

#[test]
fn check_type_equality() {
   //Ground Units unify with themselves
   let mut tlc = TLC::new();
   let si = tlc.compile_file(None, "preludes/si.tlc").unwrap();
   tlc.check(Some(si), "let x:Second=1; let y:Second=x;").unwrap();

   let mut tlc = TLC::new();
   let si = tlc.compile_file(None, "preludes/si.tlc").unwrap();
   tlc.check(Some(si), "let x:Metre=2; let y:Metre=x;").unwrap();

   let mut tlc = TLC::new();
   let si = tlc.compile_file(None, "preludes/si.tlc").unwrap();
   tlc.check(Some(si), "let x:Gram=3; let y:Gram=x;").unwrap();

   let mut tlc = TLC::new();
   let si = tlc.compile_file(None, "preludes/si.tlc").unwrap();
   tlc.check(Some(si), "let x:Ampere=1.2; let y:Ampere=x;").unwrap();

   let mut tlc = TLC::new();
   let si = tlc.compile_file(None, "preludes/si.tlc").unwrap();
   tlc.check(Some(si), "let x:Kelvin=2.3; let y:Kelvin=x;").unwrap();

   let mut tlc = TLC::new();
   let si = tlc.compile_file(None, "preludes/si.tlc").unwrap();
   tlc.check(Some(si), "let x:Mole=3.1; let y:Mole=x;").unwrap();

   let mut tlc = TLC::new();
   let si = tlc.compile_file(None, "preludes/si.tlc").unwrap();
   tlc.check(Some(si), "let x:Candela=3.2; let y:Candela=x;").unwrap();

   //Ground Units unify with compatible Numbers
   let mut tlc = TLC::new();
   let si = tlc.compile_file(None, "preludes/si.tlc").unwrap();
   tlc.check(Some(si), "let x:Metre=1; let y:Integer=x;").unwrap();
   let mut tlc = TLC::new();
   let si = tlc.compile_file(None, "preludes/si.tlc").unwrap();
   tlc.check(Some(si), "let x:Metre=1; let y:Real=x;").unwrap();
   let mut tlc = TLC::new();
   let si = tlc.compile_file(None, "preludes/si.tlc").unwrap();
   tlc.check(Some(si), "let x:Metre=1; let y:Complex=x;").unwrap();

   let mut tlc = TLC::new();
   let si = tlc.compile_file(None, "preludes/si.tlc").unwrap();
   tlc.check(Some(si), "let x:Metre=1.2; let y:Integer=x;").unwrap_err();
   let mut tlc = TLC::new();
   let si = tlc.compile_file(None, "preludes/si.tlc").unwrap();
   tlc.check(Some(si), "let x:Metre=1.2; let y:Real=x;").unwrap();
   let mut tlc = TLC::new();
   let si = tlc.compile_file(None, "preludes/si.tlc").unwrap();
   tlc.check(Some(si), "let x:Metre=1.2; let y:Complex=x;").unwrap();

   let mut tlc = TLC::new();
   let si = tlc.compile_file(None, "preludes/si.tlc").unwrap();
   tlc.check(Some(si), "let x:Metre=1.2+3i; let y:Integer=x;").unwrap_err();
   let mut tlc = TLC::new();
   let si = tlc.compile_file(None, "preludes/si.tlc").unwrap();
   tlc.check(Some(si), "let x:Metre=1.2+3i; let y:Real=x;").unwrap_err();
   let mut tlc = TLC::new();
   let si = tlc.compile_file(None, "preludes/si.tlc").unwrap();
   tlc.check(Some(si), "let x:Metre=1.2+3i; let y:Complex=x;").unwrap();

   //Ground Units do not unify with different Ground Units
   let mut tlc = TLC::new();
   let si = tlc.compile_file(None, "preludes/si.tlc").unwrap();
   tlc.check(Some(si), "let x:Metre=2; let y:Second=x;").unwrap_err();
}
