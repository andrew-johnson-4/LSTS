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
   let tc1  = Type::Constant(false,TermId{id:1});
   let tc2  = Type::Constant(false,TermId{id:2});
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
   let tc1  = Type::Constant(false,TermId{id:1});
   let tc2  = Type::Constant(false,TermId{id:2});
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
}

/*
#[test]
fn check_subtyping() {
   let mut tlc = TLC::new();

   //the type system should accept subtyping relationships
   tlc.check(None, "type Ab; type Bc; type Cd; let a: Ab+Bc; a:Ab").unwrap(); 
   tlc.check(None, "type Ab; type Bc; type Cd; let a: Ab+Bc; a:Bc").unwrap(); 
   tlc.check(None, "type Ab; type Bc; type Cd; let a: Ab+Bc; a:Cd").unwrap_err(); 
}

#[test]
fn check_narrow_implication() {
   let mut tlc = TLC::new();
   
   //narrow subtyping truth table, Bc => Ab
   tlc.check(None, "type Ab; type Bc: Ab; let a: Ab; a:Ab").unwrap();     //Ab implies        Ab
   tlc.check(None, "type Ab; type Bc: Ab; let a: Ab; a:Bc").unwrap_err(); //Ab does not imply Bc
   tlc.check(None, "type Ab; type Bc: Ab; let a: Bc; a:Ab").unwrap();     //Bc implies        Ab
   tlc.check(None, "type Ab; type Bc: Ab; let a: Bc; a:Bc").unwrap();     //Bc implies        Bc
}

#[test]
fn check_narrow_implication_with_parameters() {
   let mut tlc = TLC::new();
   
   //narrow subtyping truth table, Bc<Pt> => Ab<Pt>
   tlc.check(None, "type Pt; type Ab<A>; type Bc<B>: Ab<B>; let a: Ab<Pt>; a:Ab<Pt>").unwrap();     //Ab<Pt> implies        Ab<Pt>
   tlc.check(None, "type Pt; type Ab<A>; type Bc<B>: Ab<B>; let a: Ab<Pt>; a:Bc<Pt>").unwrap_err(); //Ab<Pt> does not imply Bc<Pt>
   tlc.check(None, "type Pt; type Ab<A>; type Bc<B>: Ab<B>; let a: Bc<Pt>; a:Ab<Pt>").unwrap();     //Bc<Pt> implies        Ab<Pt>
   tlc.check(None, "type Pt; type Ab<A>; type Bc<B>: Ab<B>; let a: Bc<Pt>; a:Bc<Pt>").unwrap();     //Bc<Pt> implies        Bc<Pt>
}

#[test]
fn check_kinded_polymorphism() {
   let mut tlc = TLC::new();

   //unification is kind sensitive
   tlc.check(None, "type Ab::Term; type Bc::BKind; let a:Ab; let b:Bc; let c:Ab = a;").unwrap();
   tlc.check(None, "type Ab::Term; type Bc::BKind; let a:Ab; let b:Bc; let c:Bc = b;").unwrap();
   tlc.check(None, "type Ab::Term; type Bc::BKind; let a:Ab; let b:Bc; let c:Ab+Bc = a;").unwrap_err();
   tlc.check(None, "type Ab::Term; type Bc::BKind; let a:Ab; let b:Bc; let c:Ab+Bc = b;").unwrap_err();
   tlc.check(None, "type Ab::Term; type Bc::BKind; let a:Ab; let b:Bc; let c:Ab = b;").unwrap_err();
   tlc.check(None, "type Ab::Term; type Bc::BKind; let a:Ab; let b:Bc; let c:Bc = a;").unwrap_err();
   tlc.check(None, "type Ab::Term; type Bc::BKind; let a:Ab; let b:Ab+Bc; let c:Ab = a;").unwrap();
   tlc.check(None, "type Ab::Term; type Bc::BKind; let a:Ab; let b:Ab+Bc; let c:Bc = a;").unwrap_err();
   tlc.check(None, "type Ab::Term; type Bc::BKind; let a:Ab; let b:Ab+Bc; let c:Ab = b;").unwrap();
   tlc.check(None, "type Ab::Term; type Bc::BKind; let a:Ab; let b:Ab+Bc; let c:Bc = b;").unwrap();
}

#[test]
fn check_kinded_parametric_polymorphism() {
   let mut tlc = TLC::new();

   //unification is kind sensitive
   tlc.check(None, "type Ab::Term; type Bc::BKind; let f(x:Ab::Term); let f(x:Bc::BKind); let x:Ab; f(x)").unwrap();
   tlc.check(None, "type Ab::Term; type Bc::BKind; let f(x:Ab::Term); let f(x:Bc::BKind); let x:Bc; f(x)").unwrap();
   tlc.check(None, "type Ab::Term; type Bc::BKind; let f(x:Ab::Term); let f(x:Bc::BKind); let x:Ab+Bc; f(x)").unwrap(); //Permitted to match multiple

   //parameters can be inferred by kind
   tlc.check(None, "type Ab::Term; type Bc::BKind; let f(x:X::Term); let f(x:X::BKind); let x:Ab; f(x)").unwrap();
   tlc.check(None, "type Ab::Term; type Bc::BKind; let f(x:X::Term); let f(x:X::BKind); let x:Bc; f(x)").unwrap();
   tlc.check(None, "type Ab::Term; type Bc::BKind; let f(x:X::Term); let f(x:X::BKind); let x:Ab+Bc; f(x)").unwrap(); //Permitted to match multiple
}

#[test]
fn check_products_and_ratios() {
   let mut tlc = TLC::new();

   //ratio types and product types are rational
   tlc.check(None, "type At; let a: At*At; a:At*At").unwrap();
   tlc.check(None, "type At; let a: At*At; a:?").unwrap();
   tlc.check(None, "type At; let a: At*At; a:?/()").unwrap();
   tlc.check(None, "type At; let a: At*At; a:At").unwrap_err();
   tlc.check(None, "type At; let a: At*At/At; a:At").unwrap();
   tlc.check(None, "type At; let a: At*At/At; a:?").unwrap();
   tlc.check(None, "type At; let a: At*At/At; a:?/()").unwrap();
   tlc.check(None, "type At; let a: At/At; a:()").unwrap();
   tlc.check(None, "type At; let a: At/At; a:?").unwrap();
   tlc.check(None, "type At; let a: At/At; a:?/()").unwrap();
   tlc.check(None, "type At; let a: At/At; a:At").unwrap_err();
   tlc.check(None, "type At; let a: At/At*At; a:At").unwrap_err();
   tlc.check(None, "type At; let a: At/At*At; a:()/At").unwrap();
   tlc.check(None, "type At; let a: At/At*At; a:()/At*At").unwrap_err();
   tlc.check(None, "type At; let a: At/At*At; a:?/()").unwrap_err();
}

#[test]
fn check_functions() {
   let mut tlc = TLC::new();

   //function application is narrowly typed
   tlc.check(None, "type Aa::Ka; type Bb; let f(X::Ka):X; let x: Aa+Bb; f(x): Aa").unwrap();
   tlc.check(None, "type Aa::Ka; type Bb; let f(X::Ka):X; let x: Aa+Bb; f(x): Bb").unwrap_err();

   //default kind is Term
   //there is no Any kind
   tlc.check(None, "type Aa::Ka; type Bb; let f(X):X; let x: Aa+Bb; f(x): Aa").unwrap_err();
   tlc.check(None, "type Aa::Ka; type Bb; let f(X):X; let x: Aa+Bb; f(x): Bb").unwrap();
}

#[test]
fn check_dependent_variable() {
   let mut tlc = TLC::new();

   tlc.check(None, "let x; let y; x: typeof(x)").unwrap();
   tlc.check(None, "let x; let y; x: typeof(y)").unwrap_err();
   tlc.check(None, "let x; let y; y: typeof(x)").unwrap_err();
   tlc.check(None, "let x; let y; y: typeof(y)").unwrap();
}
*/
