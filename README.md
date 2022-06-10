# LSTS
Large Scale Type Systems

Pronounced "Loss-Toss"

Large Scale Type Systems is a library for performance-sensitive type system operations, specifically designed for
cases where type logic may greatly outscale AST logic. The LSTS code works only with backreferences to AST code
thereby permitting lazy generation of AST nodes.

LSTS implements [a categorical view of typed lambda calculus with flexible soundness guarantees](https://github.com/andrew-johnson-4/perplexity/blob/main/categorical_prelude.md).

# Examples

    let v: Kilo<Meter>/Second = 123.456;
    let s: Minute = 78.9;
    let d: Mile = v * s;

Adding Units to existing Numbers is always permitted. Changing Units requires an additional verification step for dimension analysis. Unit is a Kind separate from Term. Inference rules are programmable on a Kind by Kind basis.

# Capabilities

LSTS does not ensure against all forms of logical errors, however it does complain about some famous ones. 

    /* Curry's Paradox */
    
    type A; forall :B. A => B
    //reject: (A,B) do not share a domain (Term,Nil)

    type A; forall :B::Term. A => B
    //accept: (A,B) share a domain (Term,Term)
    
    type A; forall :B. A => B :: Term
    //accept: (A,B) share a domain (Term,Term)
