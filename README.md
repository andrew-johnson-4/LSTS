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

    /* The square root of 2 is irrational */

    let $"/"(x:X, y:Y): X/Y;
    let $"*"(x:X, y:Y): X*Y;
    let square(x:X): X*X;

    type Pt; let p:Pt;
    type Qt; let q:Qt;
    let sqrt_of_two: Pt/Qt;
    square(sqrt_of_two) * square(q): Pt*Pt; //2 * q*q = p*p
    square(p) / square(sqrt_of_two): Qt*Qt; //p*p / 2 = q*q
    p / square(sqrt_of_two) : ?/();         //2 is a factor of p

    /* Infinitude of primes */

    import "number_theory.tlc";

    let primes:Prime[];           //assume there are a finite number of primes
    let p = primes.product() + 1; //let p be the product of all primes + 1
    forall d:primes. p%d == 1;    //p mod d, forall d in primes list, is 1
