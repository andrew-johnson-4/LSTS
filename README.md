<a href="https://andrew-johnson-4.github.io/lsts-tutorial/"><img src="https://repository-images.githubusercontent.com/404928261/4b75e965-a631-4489-a00a-d84b19a09eb9" alt="logo image" width=40%></a>

[![Crates.IO](https://img.shields.io/crates/v/LSTS.svg)](https://crates.rs/crates/LSTS)
[![Documentation](https://img.shields.io/badge/api-rustdoc-blue.svg)](https://docs.rs/lsts/latest/lsts/)
[![Read the Docs](https://img.shields.io/badge/book-reference-blue)](https://andrew-johnson-4.github.io/lsts-tutorial/)

LSTS is a proof assistant and maybe [a programming language](https://github.com/andrew-johnson-4/L1IR).

Proofs in LSTS are built by connecting terms, type definitions, and quantified statements. Terms can be evaluated to obtain Values. Types describe properties of Terms. Statements describe relations between Terms and Types.

### Terms

Terms are Lambda Calculus expressions with some extensions.

```lsts
1;
"abc";
2 + 3;
"[" + (for x in range(1,25) yield x^3).join(",") + "]";
```

### Types

Type definitions define logical statements that are then attached to Terms. All valid Terms have at least one Type. Some Terms may have more than one Type. Types may define invariant properties. These invariant properties impose preconditions and postconditions on what values may occupy that Type. Values going into a Type must satisfy that Type's preconditions. Values coming out of a Term are then known to have satisfied each Type's invariants.

Plural types are implemented through the use of a Logical Conjunction Type, similar to a Sum or Product. Types can be put into a Conjunction or projected out. Subtyping relations are then used to determine whether one conjunction implies another. There is no logical OR, only AND (unless you count arrows), and types are expected to be normalized into conjunctive normal form.

```lsts
type Even: Integer
     where self % 2 | 0;
type Odd: Integer
     where self % 2 | 1;
```

### Statements
Statements connect logic to form conclusions. Each Statement has a Term part and a Type part. A Statement may optionally have a label so it can be referenced directly later. Statements, when applied, provide new information to the Type of a Term. When a Statement is applied, it must match the pattern of its application context. An application context consists of a Term and a Type, which is then compared to the Term and Type of the Statement. These Term x Type relations form the basis of strict reasoning for LSTS.

```lsts
forall @inc_odd x: Odd. Even = x + 1;
forall @dec_odd x: Odd. Even = x - 1;
forall @inc_even x: Even. Odd = x + 1;
forall @dec_even x: Even. Odd = x - 1;

((8: Even) + 1) @inc_even : Odd
```
    
## Tutorial

For further information there is a [tutorial and reference documentation](https://andrew-johnson-4.github.io/lsts-tutorial/).
- [Español](https://andrew-johnson-4.github.io/lsts-tutorial-es/)
