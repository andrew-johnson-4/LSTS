<a href="https://andrew-johnson-4.github.io/lsts-tutorial/"><img src="https://repository-images.githubusercontent.com/404928261/4b75e965-a631-4489-a00a-d84b19a09eb9" alt="logo image" width=40%></a>

LSTS is a programming language and proof assistant.
Proofs in LSTS are built by connecting terms, type definitions, and quantified statements.
Terms can be evaluated to obtain Values.
Types describe properties of Terms.
Statements describe relations between Terms and Types.

### Much Like C
C is a portable assembler, and does a pretty good job at that.
LM/LSTS is a portable assembler, and tries to do a better job at that.

Things removed from C
* weird syntax and AST quirks
* text-based preprocessor

Things added to C
* specialization
* hygienic macros
* functionally sound type system
* generics / templates
* truly platform agnostic output (MP3 / HTML / etc.)

### [Tutorial](https://github.com/andrew-johnson-4/LSTS/wiki)
### [Documentation](https://andrew-johnson-4.github.io/lsts-language-reference/)
### [Discord](https://discord.gg/sW2ksPY9jj)

### Performance

The default LSTS backend compiles to C with little or no overhead or runtime dependencies.
Previously, the compiler generated x86-Linux objects directly, however this was approximately 3x slower than the C backend.
We will revisit the direct targets to generate fully certified builds.
Until then, C is the default backend.
