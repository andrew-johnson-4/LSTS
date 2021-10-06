#[test]
fn test_ground1(){
   let ts = lsts::declare([
      lsts::var("a"),
      lsts::ascript("a",lsts::ground("int"))
   ]).normalize().unwrap();

   assert_eq!(
      ts.lines[0].to_string(),
      "int".to_string()
   );
}

#[test]
fn test_arrow1(){
   let ts = lsts::declare([
      lsts::var("a"),
      lsts::ascript("a", lsts::arrow(lsts::ground("int"), lsts::ground("bool")))
   ]).normalize().unwrap();

   assert_eq!(
      ts.lines[0].to_string(),
      "int -> bool".to_string()
   );
}
