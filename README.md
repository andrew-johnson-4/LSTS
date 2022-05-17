# LSTS
Large Scale Type Systems

Pronounced "Loss-Toss"

Large Scale Type Systems is a library for performance-sensitive type system operations, specifically designed for
cases where type logic may greatly outscale AST logic. The LSTS code works only with backreferences to AST code
thereby permitting lazy generation of AST nodes.

LSTS implements [a categorical view of typed lambda calculus with flexible soundness guarantees](https://github.com/andrew-johnson-4/perplexity/blob/main/categorical_prelude.md).
