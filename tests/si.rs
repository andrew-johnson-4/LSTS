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
fn check_compound_types() {
   let mut tlc = TLC::new();
   let si = tlc.compile_file(None, "preludes/si.tlc").unwrap();

   //use constructors
   tlc.check(Some(si), "Point2D { x=1:Integer, y=2:Integer }").unwrap();
   tlc.check(Some(si), "Point2D { x=True:Boolean, y=1:Integer").unwrap_err();
   tlc.check(Some(si), "Point2D { x=True:Boolean, y=False:Boolean").unwrap_err();

   tlc.check(Some(si), "Point3D { x=1:Integer, y=2:Integer, z=1:Integer }").unwrap();
   tlc.check(Some(si), "Point3D { x=1:Integer, y=2:Integer, z=False:Boolean").unwrap_err();
   tlc.check(Some(si), "Point3D { x=True:Boolean, y=False:Boolean, z=False:Boolean").unwrap_err();

   //use fields
   tlc.check(Some(si), "let xy: Point2D<Integer>; xy.x:Integer").unwrap();
   tlc.check(Some(si), "let xy: Point2D<Integer>; xy.y:Integer").unwrap();
   tlc.check(Some(si), "let xy: Point2D<Integer>; xy.z:Integer").unwrap_err();
   tlc.check(Some(si), "let xy: Point2D<Real>; xy.x:Real").unwrap();
   tlc.check(Some(si), "let xy: Point2D<Real>; xy.y:Real").unwrap();
   tlc.check(Some(si), "let xy: Point2D<Real>; xy.z:Real").unwrap_err();
   tlc.check(Some(si), "let xy: Point2D<Real>; xy.x:Integer").unwrap_err();
   tlc.check(Some(si), "let xy: Point2D<Real>; xy.y:Integer").unwrap_err();
   tlc.check(Some(si), "let xy: Point2D<Real>; xy.z:Integer").unwrap_err();

   tlc.check(Some(si), "let xyz: Point3D<Integer>; xyz.x:Integer").unwrap();
   tlc.check(Some(si), "let xyz: Point3D<Integer>; xyz.y:Integer").unwrap();
   tlc.check(Some(si), "let xyz: Point3D<Integer>; xyz.z:Integer").unwrap();
   tlc.check(Some(si), "let xyz: Point3D<Real>; xyz.x:Real").unwrap();
   tlc.check(Some(si), "let xyz: Point3D<Real>; xyz.y:Real").unwrap();
   tlc.check(Some(si), "let xyz: Point3D<Real>; xyz.z:Real").unwrap();
   tlc.check(Some(si), "let xyz: Point3D<Real>; xyz.x:Integer").unwrap_err();
   tlc.check(Some(si), "let xyz: Point3D<Real>; xyz.y:Integer").unwrap_err();
   tlc.check(Some(si), "let xyz: Point3D<Real>; xyz.z:Integer").unwrap_err();
}

#[test]
fn check_kinded_type_equality() {
   //TODO check Units unify and persist
}
