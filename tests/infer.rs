#[test]
fn test_ground1(){
   let ts = lsts::declare([
      lsts::var("a"),
      lsts::ascript("a",lsts::ground("int"))
   ]).normalize();

   assert_eq!(
      ts.lines[0].to_string(),
      "int".to_string()
   );
}
