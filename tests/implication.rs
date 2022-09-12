use lsts::typ::*;
use lsts::term::TermId;

#[test]
fn check_structural_equality() {
   let tany = Type::Any;
   let tn1  = Type::Named("Aa".to_string(),vec![]);
   let tn2  = Type::Named("Bb".to_string(),vec![]);
   let tn3  = Type::Named("Cc".to_string(),vec![tn1.clone(),tn2.clone()]);
   let td1  = Type::And(vec![]);
   let td2  = Type::And(vec![tn1.clone()]);
   let td3  = Type::And(vec![tn1.clone(),tn2.clone(),tn3.clone()]);
   let ta1  = Type::Arrow(Box::new(tn1.clone()), Box::new(tn2.clone()));
   let tt1  = Type::Tuple(vec![tn1.clone(),ta1.clone()]);
   let tp1  = Type::Product(vec![tn1.clone(),ta1.clone()]);
   let tr1  = Type::Ratio(Box::new(tt1.clone()),Box::new(tp1.clone()));
   let tc1  = Type::Constant(TermId{id:1});
   let tc2  = Type::Constant(TermId{id:2});
   assert_eq!(tany, tany);
   assert_eq!(tn1, tn1);
   assert_eq!(tn2, tn2);
   assert_eq!(tn3, tn3);
   assert_eq!(td1, td1);
   assert_eq!(td2, td2);
   assert_eq!(td3, td3);
   assert_eq!(ta1, ta1);
   assert_eq!(tt1, tt1);
   assert_eq!(tp1, tp1);
   assert_eq!(tr1, tr1);
   assert_eq!(tc1, tc1);
   assert_eq!(tc2, tc2);

   assert_ne!(tany, tn1);
   assert_ne!(tn1, tn2);
   assert_ne!(tn2, tn3);
   assert_ne!(tn3, td1);
   assert_ne!(td1, td2);
   assert_ne!(td2, td3);
   assert_ne!(td3, ta1);
   assert_ne!(ta1, tt1);
   assert_ne!(tt1, tp1);
   assert_ne!(tp1, tr1);
   assert_ne!(tr1, tc1);
   assert_ne!(tc1, tc2);
   assert_ne!(tc2, tany);
}

#[test]
fn check_self_unifies() {
   let tany = Type::Any;
   let tn1  = Type::Named("Aa".to_string(),vec![]);
   let tn2  = Type::Named("Bb".to_string(),vec![]);
   let tn3  = Type::Named("Cc".to_string(),vec![tn1.clone(),tn2.clone()]);
   let td1  = Type::And(vec![]);
   let td2  = Type::And(vec![tn1.clone()]);
   let td3  = Type::And(vec![tn1.clone(),tn2.clone(),tn3.clone()]);
   let ta1  = Type::Arrow(Box::new(tn1.clone()), Box::new(tn2.clone()));
   let tt1  = Type::Tuple(vec![tn1.clone(),ta1.clone()]);
   let tp1  = Type::Product(vec![tn1.clone(),ta1.clone()]);
   let tr1  = Type::Ratio(Box::new(tt1.clone()),Box::new(tp1.clone()));
   let tc1  = Type::Constant(TermId{id:1});
   let tc2  = Type::Constant(TermId{id:2});
   assert_eq!(tany, tany.implication_unifier(&tany));
   assert_eq!(tn1, tn1.implication_unifier(&tn1));
   assert_eq!(tn2, tn2.implication_unifier(&tn2));
   assert_eq!(tn3, tn3.implication_unifier(&tn3));
   assert_eq!(td1, td1.implication_unifier(&td1));
   assert_eq!(tn1, td2.implication_unifier(&td2));
   assert_eq!(td3, td3.implication_unifier(&td3));
   assert_eq!(ta1, ta1.implication_unifier(&ta1));
   assert_eq!(tt1, tt1.implication_unifier(&tt1));
   assert_eq!(tp1, tp1.implication_unifier(&tp1));
   assert_eq!(tr1, tr1.implication_unifier(&tr1));
   assert_eq!(tc1, tc1.implication_unifier(&tc1));
   assert_eq!(tc2, tc2.implication_unifier(&tc2));
}

#[test]
fn check_compound_failures() {
   let tb   = Type::And(vec![]);
   let tn1  = Type::Named("Aa".to_string(),vec![]);
   let tn2  = Type::Named("Bb".to_string(),vec![]);
   let ta1  = Type::Arrow(Box::new(tn1.clone()), Box::new(tn1.clone()));
   let ta2  = Type::Arrow(Box::new(tn1.clone()), Box::new(tn2.clone()));
   let ta3  = Type::Arrow(Box::new(tn2.clone()), Box::new(tn1.clone()));
   let tt1  = Type::Tuple(vec![]);
   let tt2  = Type::Tuple(vec![tn1.clone()]);
   let tt3  = Type::Tuple(vec![tn2.clone()]);
   let tt4  = Type::Tuple(vec![tn1.clone(),tn2.clone()]);
   let tp1  = Type::Product(vec![]);
   let tp2  = Type::Product(vec![tn1.clone()]);
   let tp3  = Type::Product(vec![tn2.clone()]);
   let tp4  = Type::Product(vec![tn1.clone(),tn2.clone()]);
   let tr1  = Type::Ratio(Box::new(tn1.clone()), Box::new(tn1.clone()));
   let tr2  = Type::Ratio(Box::new(tn1.clone()), Box::new(tn2.clone()));
   let tr3  = Type::Ratio(Box::new(tn2.clone()), Box::new(tn1.clone()));
   assert_eq!(ta1.implication_unifier(&ta2), tb.clone());
   assert_eq!(ta1.implication_unifier(&ta3), tb.clone());

   assert_eq!(tt1.implication_unifier(&tt2), tb.clone());
   assert_eq!(tt2.implication_unifier(&tt3), tb.clone());
   assert_eq!(tt2.implication_unifier(&tt4), tb.clone());

   assert_eq!(tp1.implication_unifier(&tp2), tb.clone());
   assert_eq!(tp2.implication_unifier(&tp3), tb.clone());
   assert_eq!(tp2.implication_unifier(&tp4), tb.clone());

   assert_eq!(tr1.implication_unifier(&tr2), tb.clone());
   assert_eq!(tr1.implication_unifier(&tr3), tb.clone());
}

#[test]
fn check_simplytyped() {
   let tb   = Type::And(vec![]);
   let tany = Type::Any;
   let tn1  = Type::Named("Aa".to_string(),vec![]);
   let tn2  = Type::Named("Bb".to_string(),vec![]);
   let ta1  = Type::Arrow(Box::new(tn1.clone()), Box::new(tn2.clone()));
   let ta2  = Type::Arrow(Box::new(tn1.clone()), Box::new(tany.clone()));
   let ta3  = Type::Arrow(Box::new(tn2.clone()), Box::new(tany.clone()));
   assert_eq!(ta1, ta1.implication_unifier(&ta2));
   assert_eq!(tb, ta1.implication_unifier(&ta3));
}

#[test]
fn check_normalization() {
   let tn1  = Type::Named("Aa".to_string(),vec![]);
   let tn2  = Type::Named("Bb".to_string(),vec![]);
   let tn3  = Type::Named("Cc".to_string(),vec![]);
   let tp1  = Type::Product(vec![tn1.clone(),tn2.clone()]);
   let tp2  = Type::Product(vec![tn2.clone(),tn1.clone()]);
   let ts1  = Type::And(vec![tn3.clone(),tp1.clone()]);
   let ts2  = Type::And(vec![tp2.clone(),tn3.clone()]);
   assert_eq!(ts1.normalize(), ts1.normalize().implication_unifier(&ts2.normalize()));

   //{(Cd*Bc)+Ab} = {Ab+(Bc*Cd)}
   let tp3  = Type::Product(vec![tn3.clone(),tn2.clone()]);
   let tp4  = Type::Product(vec![tn2.clone(),tn3.clone()]);
   let ts3  = Type::And(vec![tp3.clone(),tn1.clone()]);
   let ts4  = Type::And(vec![tn1.clone(),tp4.clone()]);
   assert_eq!(ts3.normalize(), ts4.normalize());
}

#[test]
fn check_subtyping() {
   let td   = Type::And(vec![]);
   let tn1  = Type::Named("Aa".to_string(),vec![]);
   let tn2  = Type::Named("Bb".to_string(),vec![]);
   let tn3  = Type::Named("Cc".to_string(),vec![]);
   let ts1  = Type::And(vec![tn1.clone(),tn2.clone()]);
   let ts2  = Type::And(vec![tn1.clone(),tn2.clone(),tn3.clone()]);
   assert_eq!(tn1, ts1.implication_unifier(&tn1));
   assert_eq!(tn2, ts1.implication_unifier(&tn2));
   assert_eq!(td, ts1.implication_unifier(&tn3));
   assert_eq!(td, tn1.implication_unifier(&ts1));
   assert_eq!(td, tn2.implication_unifier(&ts1));
   assert_eq!(td, tn3.implication_unifier(&ts1));
   assert_eq!(ts1, ts2.implication_unifier(&ts1));
   assert_eq!(td, ts1.implication_unifier(&ts2));
}

#[test]
fn check_parameters_subtyping() {
   let td   = Type::And(vec![]);
   let tn1  = Type::Named("Aa".to_string(),vec![]);
   let tn2  = Type::Named("Bb".to_string(),vec![]);
   let tn3  = Type::Named("Cc".to_string(),vec![ tn1.clone() ]);
   let tn4  = Type::Named("Cc".to_string(),vec![ Type::And(vec![tn1.clone(),tn2.clone()]) ]);
   assert_eq!(tn3, tn3.implication_unifier(&tn3));
   assert_eq!(tn3, tn4.implication_unifier(&tn3));
   assert_eq!(td, tn3.implication_unifier(&tn4));
}

#[test]
fn check_compound_subtyping() {
   let td   = Type::And(vec![]);
   let tnil = Type::Tuple(vec![]);
   let tn1  = Type::Named("Aa".to_string(),vec![]);
   let tn2  = Type::Named("Bb".to_string(),vec![]);
   let ts1  = Type::And(vec![tn1.clone(),tn2.clone()]);

   let ta1  = Type::Arrow(Box::new(tn1.clone()), Box::new(tn1.clone()));
   let ta2  = Type::Arrow(Box::new(ts1.clone()), Box::new(tn1.clone()));
   let ta3  = Type::Arrow(Box::new(tn1.clone()), Box::new(ts1.clone()));
   assert_eq!(ta1, ta1.implication_unifier(&ta1));
   assert_eq!(td, ta2.implication_unifier(&ta1));  //contravariance
   assert_eq!(ta1, ta3.implication_unifier(&ta1));
   assert_eq!(ta1, ta1.implication_unifier(&ta2)); //contravariance
   assert_eq!(td, ta1.implication_unifier(&ta3));

   let tt1  = Type::Tuple(vec![tn1.clone(), tn1.clone()]);
   let tt2  = Type::Tuple(vec![ts1.clone(), tn1.clone()]);
   let tt3  = Type::Tuple(vec![tn1.clone(), ts1.clone()]);
   assert_eq!(tt1, tt1.implication_unifier(&tt1));
   assert_eq!(tt1, tt2.implication_unifier(&tt1));
   assert_eq!(tt1, tt3.implication_unifier(&tt1));
   assert_eq!(td, tt1.implication_unifier(&tt2));
   assert_eq!(td, tt1.implication_unifier(&tt3));

   let tp1  = Type::Product(vec![tn1.clone(), tn1.clone()]);
   let tp2  = Type::Product(vec![ts1.clone(), tn1.clone()]);
   let tp3  = Type::Product(vec![tn1.clone(), ts1.clone()]);
   assert_eq!(tp1, tp1.implication_unifier(&tp1));
   assert_eq!(tp1, tp2.implication_unifier(&tp1));
   assert_eq!(tp1, tp3.implication_unifier(&tp1));
   assert_eq!(td, tp1.implication_unifier(&tp2));
   assert_eq!(td, tp1.implication_unifier(&tp3));

   let tr1  = Type::Ratio(Box::new(tn1.clone()), Box::new(tn1.clone()));
   let tr2  = Type::Ratio(Box::new(ts1.clone()), Box::new(tn1.clone()));
   let tr3  = Type::Ratio(Box::new(tn1.clone()), Box::new(ts1.clone()));
   assert_eq!(tnil, tr1.implication_unifier(&tr1));
   assert_eq!(tnil, tr2.implication_unifier(&tr1));
   assert_eq!(tnil, tr3.implication_unifier(&tr1));
   assert_eq!(td, tr1.implication_unifier(&tr2));
   assert_eq!(td, tr1.implication_unifier(&tr3));
}

#[test]
fn check_products_and_ratios() {
   let td   = Type::And(vec![]);
   let tany = Type::Any;
   let tn1  = Type::Named("Aa".to_string(),vec![]);
   let tn2  = Type::Named("Bb".to_string(),vec![]);
   let tp1  = Type::Product(vec![ tn1.clone(), tn1.clone() ]);
   let tp2  = Type::Product(vec![ tn1.clone(), tn2.clone() ]);
   let tt1  = Type::Tuple(vec![]);
   let tr1  = Type::Ratio( Box::new(tp1.clone()), Box::new(tt1.clone()) );
   let tr2  = Type::Ratio( Box::new(tp1.clone()), Box::new(tn1.clone()) );
   let tr3  = Type::Ratio( Box::new(tn1.clone()), Box::new(tn1.clone()) );
   let tr4  = Type::Ratio( Box::new(tt1.clone()), Box::new(tn1.clone()) );
   let tr5  = Type::Ratio( Box::new(tany.clone()), Box::new(tn1.clone()) );

   assert_eq!(tp1.normalize(), tp1.normalize().implication_unifier(&tp1));
   assert_eq!(tp1.normalize(), tp1.normalize().implication_unifier(&tany));
   assert_eq!(tp2.normalize(), tp2.normalize().implication_unifier(&tp2));
   assert_eq!(tp2.normalize(), tp2.normalize().implication_unifier(&tany));

   assert_eq!(tp1.normalize(), tp1.normalize().implication_unifier(&tr1));

   assert_eq!(td, tp1.normalize().implication_unifier(&tn1));
   assert_eq!(td, tp2.normalize().implication_unifier(&tn1));

   assert_eq!(tn1, tr2.normalize());
   assert_eq!(tn1, tr2.normalize().implication_unifier(&tn1));

   assert_eq!(tt1, tr3.normalize());
   assert_eq!(tt1, tr3.normalize().implication_unifier(&tt1));

   assert_eq!(tr4, tr4.normalize());
   assert_eq!(tr4, tr4.normalize().implication_unifier(&tr4));

   assert_eq!(td, tr3.normalize().implication_unifier(&tr5));
}

#[test]
fn check_parameter_unification() {
   let td   = Type::And(vec![]);
   let tn1  = Type::Named("Aa".to_string(),vec![]);
   let tn2  = Type::Named("Bb".to_string(),vec![]);
   let tn3  = Type::Named("Cc".to_string(),vec![]);
   let tn4  = Type::Named("X".to_string(),vec![]);

   let tt1  = Type::Tuple(vec![tn1.clone(), tn1.clone()]);
   let tt2  = Type::Tuple(vec![tn1.clone(), tn2.clone()]);
   let tt3  = Type::Tuple(vec![tn4.clone(), tn4.clone()]);

   let ts1  = Type::And(vec![tn1.clone(), tn2.clone()]);
   let ts2  = Type::And(vec![tn1.clone(), tn2.clone(), tn3.clone()]);
   let ts3  = Type::And(vec![tn1.clone(), tn3.clone()]);
   let ts4  = Type::And(vec![tn2.clone(), tn3.clone()]);

   let tt4  = Type::Tuple(vec![ts1.clone(), ts1.clone()]);
   let tt5  = Type::Tuple(vec![ts1.clone(), ts3.clone()]);

   let ta1  = Type::Arrow( Box::new(tt1.clone()), Box::new(tn1.clone()) );
   let ta2  = Type::Arrow( Box::new(tt1.clone()), Box::new(tn3.clone()) );
   let ta3  = Type::Arrow( Box::new(tt2.clone()), Box::new(tn3.clone()) );
   let ta4  = Type::Arrow( Box::new(tt3.clone()), Box::new(tn4.clone()) );
   assert_eq!(ta1, ta1.implication_unifier(&ta4));
   assert_eq!(td,  ta2.implication_unifier(&ta4));
   assert_eq!(td,  ta3.implication_unifier(&ta4));

   let ta5  = Type::Arrow( Box::new(tt4.clone()), Box::new(ts2.clone()) );
   let ta6  = Type::Arrow( Box::new(tt5.clone()), Box::new(ts2.clone()) );
   let ta7  = Type::Arrow( Box::new(tt5.clone()), Box::new(ts4.clone()) );
   let ta8  = Type::Arrow( Box::new(tt4.clone()), Box::new(ts1.clone()) );
   assert_eq!(ta8, ta5.implication_unifier(&ta4));
   assert_eq!(ta1, ta6.implication_unifier(&ta4));
   assert_eq!(td,  ta7.implication_unifier(&ta4));
}

#[test]
fn check_function_unification() {
   let td   = Type::And(vec![]);
   let tany = Type::Any;
   let tn1  = Type::Named("Integer".to_string(),vec![]);
   let tn2  = Type::Named("Number".to_string(),vec![]);
   let tn3  = Type::Named("Point2D".to_string(),vec![]);
   let tn4  = Type::Named("N".to_string(),vec![]);
   let tn5  = Type::Named("Point2D".to_string(),vec![ Type::And(vec![tn1.clone(), tn2.clone()]) ]);
   let tn6  = Type::Named("Point2D".to_string(),vec![ tn4.clone() ]);
   let ts1  = Type::And(vec![tn1.clone(), tn2.clone()]);
   let ts2  = Type::And(vec![tn3.clone(), tn5.clone()]);

   //Integer -> ? => N -> N = Integer -> Integer
   let ta1  = Type::Arrow( Box::new(tn1.clone()), Box::new(tany.clone()) );
   let ta2  = Type::Arrow( Box::new(tn4.clone()), Box::new(tn4.clone()) );
   let ta3  = Type::Arrow( Box::new(tn1.clone()), Box::new(tn1.clone()) );
   assert_eq!(ta3, ta1.implication_unifier(&ta2));

   //Point2D<{Integer+Number}> -> ? => Point2D<N> -> N = Point2D<{Integer+Number}> -> {Integer+Number}
   let ta4  = Type::Arrow( Box::new(tn5.clone()), Box::new(tany.clone()) );
   let ta5  = Type::Arrow( Box::new(tn6.clone()), Box::new(tn4.clone()) );
   let ta6  = Type::Arrow( Box::new(tn5.clone()), Box::new(ts1.clone()) );
   assert_eq!(ta6, ta4.implication_unifier(&ta5));

   //{Point2D+Point2D<{Integer+Number}>} -> ? => Point2D<N> -> N = {}
   let ta7  = Type::Arrow( Box::new(ts2.clone()), Box::new(tany.clone()) );
   let ta8  = Type::Arrow( Box::new(tn6.clone()), Box::new(tn4.clone()) );
   assert_eq!(td, ta7.implication_unifier(&ta8));

}

#[test]
fn check_arrow_ratio() {
   let tany = Type::Any;
   let tn1  = Type::Named("Pt".to_string(),vec![]);
   let tn2  = Type::Named("Qt".to_string(),vec![]);
   let tn3  = Type::Named("X".to_string(),vec![]);
   let tr1  = Type::Ratio( Box::new(tn1.clone()), Box::new(tn2.clone()) );
   let tp1  = Type::Product(vec![ tn1.clone(), tn1.clone() ]); 
   let tp2  = Type::Product(vec![ tn2.clone(), tn2.clone() ]); 
   let tp3  = Type::Product(vec![ tn3.clone(), tn3.clone() ]); 
   let tr2  = Type::Ratio( Box::new(tp1.clone()), Box::new(tp2.clone()) );
   let ta1  = Type::Arrow( Box::new(tr1.clone()), Box::new(tany.clone()) );
   let ta2  = Type::Arrow( Box::new(tn3.clone()), Box::new(tp3.clone()) );
   let ta3  = Type::Arrow( Box::new(tr1.clone()), Box::new(tr2.clone()) );
   //Pt/Qt -> ? => X -> X*X = Pt/Qt -> Pt*Pt/Qt*Qt
   assert_eq!( ta3, ta1.implication_unifier(&ta2) );
}

#[test]
fn check_constant_arrows() {
   let tany = Type::Any;
   let tc1  = Type::Constant(TermId{id:1});
   let tc2  = Type::Constant(TermId{id:2});
   let tc3  = Type::Constant(TermId{id:3});
   let tc4  = Type::Constant(TermId{id:4});
   let ta1  = Type::Arrow( Box::new(tc1.clone()), Box::new(tany.clone()) );
   let ta2  = Type::Arrow( Box::new(tc2.clone()), Box::new(tc3.clone()) );
   assert_eq!( ta2, ta1.implication_unifier(&ta2) );

   let ts1  = Type::And(vec![ tc1.clone(), tc4.clone() ]);
   let ta3  = Type::Arrow( Box::new(ts1.clone()), Box::new(tany.clone()) );
   assert_eq!( ta2, ta3.implication_unifier(&ta2) );

   let tt1  = Type::Tuple(vec![ tc1.clone(), tc2.clone() ]);
   let tt2  = Type::Tuple(vec![ tc3.clone(), tc4.clone() ]);
   let ta4  = Type::Arrow( Box::new(tt1.clone()), Box::new(tt1.clone()) );
   let ta5  = Type::Arrow( Box::new(tt2.clone()), Box::new(tt2.clone()) );
   assert_eq!( ta5, ta4.implication_unifier(&ta5) );
}
