#[test]
fn test_uvar1(){
   let ts = lsts::declare([
      lsts::uvar(),
      lsts::uvar(),
   ]).normalize();

   assert_eq!(
      ts.lines[0].to_string()[..6],
      "'uvar_".to_string()
   );

   assert_eq!(
      ts.lines[1].to_string()[..6],
      "'uvar_".to_string()
   );
}

#[test]
fn test_ground1(){
   let ts = lsts::declare([
      lsts::ground("int")
   ]).normalize();

   assert_eq!(
      ts.lines[0].to_string(),
      "int".to_string()
   );
}

#[test]
fn test_arrow1(){
   let ts = lsts::declare([
      lsts::arrow(lsts::ground("int"), lsts::ground("bool"))
   ]).normalize();

   assert_eq!(
      ts.lines[0].to_string(),
      "int -> bool".to_string()
   );
}

#[test]
fn test_forall1(){
   let mut f1token = 0;
   let ts = lsts::declare([
      lsts::forall(&mut f1token, ["a"], lsts::ttrue()),
      lsts::arrow(lsts::var("a"), lsts::ground("bool")),
      lsts::end(f1token),
   ]).normalize();

   assert_eq!(
      ts.lines[0].to_string(),
      format!("forall 'a. T")
   );
   assert_eq!(
      ts.lines[1].to_string(),
      format!("'a -> bool")
   );
   assert_eq!(
      ts.lines[2].to_string(),
      format!("end {}", f1token)
   );
}

#[test]
fn test_exists1(){
   let mut e1token = 0;
   let ts = lsts::declare([
      lsts::exists(&mut e1token, ["a"], lsts::ttrue()),
      lsts::arrow(lsts::var("a"), lsts::ground("bool")),
      lsts::end(e1token),
   ]).normalize();

   assert_eq!(
      ts.lines[0].to_string(),
      format!("exists 'a. T")
   );
   assert_eq!(
      ts.lines[1].to_string(),
      format!("'a -> bool")
   );
   assert_eq!(
      ts.lines[2].to_string(),
      format!("end {}", e1token)
   );
}
