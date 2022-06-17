# LSTS: Large Scale Type Systems

[![Crates.IO](https://img.shields.io/crates/v/LSTS.svg)](https://crates.rs/crates/LSTS)
[![Build](https://github.com/andrew-johnson-4/LSTS/workflows/Build/badge.svg)](https://github.com/andrew-johnson-4/LSTS)

Pronounced "Loss-Toss"

Large Scale Type Systems is a library for performance-sensitive type system operations, specifically designed for
cases where type logic may greatly outscale AST logic. The LSTS code works only with backreferences to AST code
thereby permitting lazy generation of AST nodes.

LSTS implements [a categorical view of typed lambda calculus with flexible soundness guarantees](https://github.com/andrew-johnson-4/perplexity/blob/main/categorical_prelude.md).

# Examples

    let v: Kilo<Meter>/Second = 123.456;
    let s: Minute = 78.9;
    let d: Mile = (v as Mile/Minute) * s;

Unit is a Kind separate from Term. Unit typing adds a check for dimension analysis. Inference rules are programmable on a Kind by Kind basis.

# Capabilities

LSTS does not ensure against all forms of logical errors, however it does complain about some famous ones. 

    /* Curry's Paradox */
    
    type A; forall :B. A => B
    //reject: (A,B) do not share a domain (Term,Nil)

    type A; forall :B::Term. A => B
    //accept: (A,B) share a domain (Term,Term)
    
    type A; forall :B. A => B :: Term
    //accept: (A,B) must share a domain (Term,Term)
    
    /* Types are polymorphic by default */
    
    let f(x: X);
    //X is a Term but also might have a Unit or other association
    
    let f(x: X::Term);
    //to disable polymorphic inference a bounding domain must be specified
