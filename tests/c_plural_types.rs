use lsts::typ::Type;

#[test]
fn check_arrow() {
   let tany = Type::Any;
   let tc1  = Type::Named("T1".to_string(),vec![]);
   let tc2  = Type::Named("T2".to_string(),vec![]);
   let ts1  = Type::And(vec![ tc1.clone(), tc2.clone() ]);
   let ta1  = Type::Arrow( Box::new(tc1.clone()), Box::new(tc2.clone()) );
   let ta2  = Type::Arrow( Box::new(ts1.clone()), Box::new(tany.clone()) );
   let ta3  = Type::Arrow( Box::new(tc1.clone()), Box::new(tc2.clone()) );

   assert_eq!( ta3, ta2.implication_unifier(&ta1) );
}
