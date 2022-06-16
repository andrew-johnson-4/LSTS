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
   tlc.parse("let t: A|B").unwrap();
   tlc.parse("let a: A; let a: B").unwrap();
   tlc.parse("let a: A; let b: A").unwrap();
   tlc.parse("let f()").unwrap();
   tlc.parse("let f(a)").unwrap();
   tlc.parse("let f(a,b)").unwrap();
   tlc.parse("let f(a: A)").unwrap();
   tlc.parse("let f(a: A::Term)").unwrap();
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
fn check_narrow_implication() {
   let mut tlc = TLC::new();
   
   //narrow subtyping truth table, Bc => Ab
   tlc.check(None, "type Ab; type Bc: Ab; let a: Ab; a:Ab").unwrap();     //Ab implies        Ab
   tlc.check(None, "type Ab; type Bc: Ab; let a: Ab; a:Bc").unwrap_err(); //Ab does not imply Bc
   tlc.check(None, "type Ab; type Bc: Ab; let a: Bc; a:Ab").unwrap();     //Bc implies        Ab
   tlc.check(None, "type Ab; type Bc: Ab; let a: Bc; a:Bc").unwrap();     //Bc implies        Bc
}

