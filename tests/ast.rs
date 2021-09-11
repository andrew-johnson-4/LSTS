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
