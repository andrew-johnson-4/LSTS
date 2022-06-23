use lsts::tlc::TLC;

#[test]
fn parse_simplytyped() {
   let mut tlc = TLC::new();
   tlc.parse("a").unwrap();
   tlc.parse("a()").unwrap();
   tlc.parse("a(b)").unwrap();
   tlc.parse("a(b,c)").unwrap();
   tlc.parse("let t: ?").unwrap();
   tlc.parse("let t: T").unwrap();
   tlc.parse("let t: ()").unwrap();
   tlc.parse("let t: (A)").unwrap();
   tlc.parse("let t: (A,B)").unwrap();
   tlc.parse("let t: T<A,B>").unwrap();
   tlc.parse("let t: ?[A]").unwrap();
   tlc.parse("let t: ()->A").unwrap();
   tlc.parse("let t: A->B").unwrap();
   tlc.parse("let t: (A)->B").unwrap();
   tlc.parse("let t: (A,B)->C").unwrap();
   tlc.parse("let a: A; let a: B").unwrap();
   tlc.parse("let a: A; let b: A").unwrap();
   tlc.parse("let f()").unwrap();
   tlc.parse("let f(a:A)").unwrap();
   tlc.parse("let f(a:A,b:B)").unwrap();
   tlc.parse("let f(a:A)").unwrap();
   tlc.parse("let f(a:A::Term)").unwrap();
   tlc.parse("let f():A").unwrap();
   tlc.parse("let f()::Term").unwrap();
   tlc.parse("type A").unwrap();
   tlc.parse("forall :A,:B::C. (A,B)").unwrap();
   tlc.parse("forall :A,:B::C. (A,B) :: R").unwrap();
   tlc.parse("forall :A,:B::C. (A,B) => C :: R").unwrap();
   tlc.parse("{a; b}").unwrap();
}

#[test]
fn check_simplytyped() {
   let mut tlc = TLC::new();

   //inhabited types must be defined
   tlc.check(None, "type Ab; let a: Ab").unwrap();
   tlc.check(None, "let a: Ab").unwrap_err();

   //unexpected argument B to function A -> B
   tlc.check(None, "type Ab; type Bc; let a: Ab->Bc; let b: Ab; a(b)").unwrap();
   tlc.check(None, "type Ab; type Bc; let a: Ab->Bc; let b: Bc; a(b)").unwrap_err();
}

#[test]
fn check_normalization() {
   let mut tlc = TLC::new();

   //types should be normalized during unification
   tlc.check(None, "type Ab; type Bc; type Cd; let a: Ab+Bc*Cd; a:Cd*Bc+Ab").unwrap(); 
}

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
}
