<a href="https://andrew-johnson-4.github.io/lsts-tutorial/"><img src="https://repository-images.githubusercontent.com/404928261/4b75e965-a631-4489-a00a-d84b19a09eb9" alt="logo image" width=40%></a>

LSTS is a programming language and proof assistant.
Proofs in LSTS are built by connecting terms, type definitions, and quantified statements.
Terms can be evaluated to obtain Values.
Types describe properties of Terms.
Statements describe relations between Terms and Types.

### Tutorial

A tutorial is available on [the wiki](https://github.com/andrew-johnson-4/LSTS/wiki).

### Performance

The default LSTS backend compiles to C with little or no overhead or runtime dependencies.
Previously, the compiler generated x86-Linux objects directly, however this was approximately 3x slower than the C backend.
We will revisit the direct targets to generate fully certified builds.
Until then, C is the default backend.

### Example

The lsts tokenizer is an example of an efficient tokenization algorithm written in LSTS.

```
let lsts-tokenize-string(file-path: String, text: String): List<String> = (

   let tokens = [] :: List<String>;
   while non-zero(text) {match text {
      # ignore whitespace
      "\s".. rest => text = rest;
      "\t".. rest => text = rest;
      "\n".. rest => text = rest;

      # consume tokens that start with these strings
      "~=".. rest => (tokens = cons(text[:"~=".length], tokens); text = rest;);
      "+=".. rest => (tokens = cons(text[:"+=".length], tokens); text = rest;);
      ...

      # consume tokens that start with these regular expressions
      (lit=r/^["]([^"\\]|([\\].))*["]/).. rest => (
         tokens = cons(text[:lit.length], tokens); text = rest;
      );
      (rgx=r/^r[\/]([^\/]|([\\].))*[\/]/).. rest => (
         tokens = cons(text[:rgx.length], tokens); text = rest;
      );
      ...

      # otherwise complain about unexpected token
      rest => ( fail("Unrecognized Token in File \{file-path}: \{clone-rope(rest[0])}"); );
   }};

   tokens;
);

# token source location and snippets can be derived from the original source substrings
# this information is only calculated when there is a demand for it
let .token-location(t: String): SourceLocation = (
   let file-path = token-file-paths.lookup( t.data as U64, "[Unknown File]" );
   let line = 1;
   let column = 1;
   let data = t.data;
   while data < t.start {
      if data[0] == $"10_u8" then {
         line = line + 1;
         column = 1;
      } else {
         column = column + 1;
      };
      data = data + 1;
   };
   SourceLocation { file-path, line, column }
);
```
