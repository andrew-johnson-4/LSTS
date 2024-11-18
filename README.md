<a href="https://andrew-johnson-4.github.io/lsts-tutorial/"><img src="https://repository-images.githubusercontent.com/404928261/4b75e965-a631-4489-a00a-d84b19a09eb9" alt="logo image" width=40%></a>

LSTS is a programming language and proof assistant.
Proofs in LSTS are built by connecting terms, type definitions, and quantified statements.
Terms can be evaluated to obtain Values.
Types describe properties of Terms.
Statements describe relations between Terms and Types.

### Installation

LSTS is now bundled by default with the [Lambda Mountain](https://github.com/andrew-johnson-4/lambda-mountain) compiler.

```
git clone https://github.com/andrew-johnson-4/lambda-mountain.git
cd lambda-mountain
make install
```

### Performance

The default LSTS backend compiles to C with little or no overhead or runtime dependencies.
Previously, the compiler generated x86-Linux objects directly, however this was approximately 3x slower than the C backend.
We will revisit the direct targets to generate fully certified builds.
Until then, C is the default backend.

### Terms

Terms are Lambda Calculus expressions with some extensions.

```lsts
1;
"abc";
2 + 3;
"[" + (for x in range(1,25) yield x^3).join(",") + "]";
```

### Types

Type definitions define logical statements that are then attached to Terms.
All valid Terms have at least one Type. Some Terms may have more than one Type.
Types may define invariant properties.
These invariant properties impose preconditions and postconditions on what values may occupy that Type.
Values going into a Type must satisfy that Type's preconditions. Values coming out of a Term are then known to have satisfied each Type's invariants.

```lsts
type Even: Integer
     where self % 2 | 0;
type Odd: Integer
     where self % 2 | 1;
```

### Statements
Statements connect logic to form conclusions. Each Statement has a Term part and a Type part.
Statements, when applied, provide new information to the Type of a Term. When a Statement is applied, it must match the pattern of its application context.
An application context consists of a Term and a Type, which is then compared to the Term and Type of the Statement.
These Term x Type relations form the basis of strict reasoning for LSTS.

```lsts
forall @inc_odd x: Odd. Even = x + 1;
forall @dec_odd x: Odd. Even = x - 1;
forall @inc_even x: Even. Odd = x + 1;
forall @dec_even x: Even. Odd = x - 1;

((8: Even) + 1) @inc_even : Odd
```

### Theorem Proving

Runtime and performance are the primary constraint on theorem proving.
To address these concerns we employ several strategies somewhat unique to LSTS:
* aggressive search-space [pruning](https://github.com/andrew-johnson-4/lambda-mountain/wiki/Type-System)
   * Punning is key here: ["well designed puns can lead to asymptotically different inference performance"](https://github.com/andrew-johnson-4/lambda-mountain/wiki#%CE%BB-name-origin) 
* [full control over every instruction](https://github.com/andrew-johnson-4/lambda-mountain) for control-freak style performance tuning
* (not implemented yet) parallel inference: the specialization rule is highly amenable to parallel execution

### Logic Backend

The language here is based on [System F-sub](https://en.wikipedia.org/wiki/System_F) with the following inference rules added.

$$abstraction \quad \frac{\Gamma \vdash a:A \quad \Gamma \vdash b:B \quad \Gamma \vdash x:X \quad \Gamma \vdash y:Y \quad λ⟨a.b⟩⟨x.y⟩}{\Gamma \vdash λ⟨a.b⟩⟨x.y⟩:(A \to B) + (X \to Y)}$$

$$application \quad \frac{\Gamma \vdash f:(A \to B) + (C \to D) + (X \to Y) \quad \Gamma \vdash x:A + X \quad f(x)}{\Gamma \vdash f(x):B + Y}$$

### Further Reading

* [Wiki](https://github.com/andrew-johnson-4/LSTS/wiki)
