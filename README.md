# Hectoparsec

[![License MIT](https://img.shields.io/badge/license-MIT-blue.svg)](./LICENSE)
[![Hackage](https://img.shields.io/hackage/v/hectoparsec.svg)](https://hackage.haskell.org/package/hectoparsec)

Flexible and powerful parser combinators for Haskell.  

## Features

Hectoparsec's main feature is assuming very little about the input and output types of your parsing workflow. This means that it is very flexible, while still providing everything you need to create a powerful parser. In particular, Hectoparsec was originally created in order to support the lexing and parsing workflow more easily than existing parser combinator libraries in Haskell.  

- Assumes very little about the type of input stream.
- Allows for custom parser error types and custom parser label types.
- Supports taking multiple tokens from the stream for higher performance.
- Keeps track of parser labels to generate informational error messages.

In addition, Hectoparsec is fairly lightweight. It does not define any general parser combinators, so it is recommended you use the [parser-combinators](https://hackage.haskell.org/package/parser-combinators) library for that.  

There is also no built-in error pretty printing. However, the information to generate pretty errors is generated, and it is recommended you convert it to a pretty error message with the [errata](https://hackage.haskell.org/package/errata) library.  

## Comparison

Hectoparsec is based on the [parsec](https://hackage.haskell.org/package/parsec) and [megaparsec](https://hackage.haskell.org/package/megaparsec) libraries, but it solves a different problem from them. In particular, they are much more focused on parsing directly from text, and have built-in methods for generating languages and pretty print error messages. If you are looking for a batteries-included parser combinator library, you should prefer one of those two.  

If you want to parse directly from text and are looking for performance, incremental parsing, and not pretty error messages, you should take a look [attoparsec](https://hackage.haskell.org/package/attoparsec) instead.  

## Examples

There are examples in the [examples directory](./examples), which includes using [parser-combinators](https://hackage.haskell.org/package/parser-combinators) for combinators and integration with [errata](https://hackage.haskell.org/package/errata) for error pretty-printing.  

You can run the examples with `cabal run hectoparsec-example-<name>`.  

## Contributing

Check out the [contributing](./CONTRIBUTING.md) documentation.  

Found a bug? Open an issue or a pull request if you can fix it!  
