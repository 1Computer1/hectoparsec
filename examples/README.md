# Examples

These are some examples of using Hectoparsec. Note that they mostly focus on lexing and parsing, since material for parsing directly from text is already covered for existing libraries like `parsec` which is not too different with Hectoparsec.  

- `while` - Example parser for an imperative language with C-like syntax (some backtracking and non-significant whitespace). It lexes the source text into tokens then parses using a list of tokens. Rather than have the lexer error, we instead make it so that there can be unknown tokens, so it also shows how you can report lexical errors in the parser. We also combine our `ParserT` with `Writer` so that we can report warnings during parsing.

- `csg` - Example parser for a context-sensitive grammar via interpolating an expression into a string e.g. `"hello $(1 + 2)"`, requiring interleaving the lexer and parser together. This example shows a complicated `TokenStream` that uses the lexer to get the next token, keeping track of the current lexer mode.

In each example, we parse a bunch of files and pretty print either the resulting abstract syntax tree, or a pretty error using [errata](https://hackage.haskell.org/package/errata).  

You can run the examples with `cabal run hectoparsec-example-<name>`.  
