# Contributing

You can contribute to Hectoparsec!  

Hectoparsec uses [Cabal](https://cabal.readthedocs.io/en/3.4/) for building and [Haskell Language Server](https://github.com/haskell/haskell-language-server) for IDE integration.  

## Tests

Hectoparsec currently uses [hspec](http://hspec.github.io/) as the test framework. You can run tests with `cabal test`.  

If you would like to rerun failed tests, follow the instructions [here](http://hspec.github.io/rerun.html).  
If you would like to run a specific test, use `cabal run hectoparsec-test -- -m PATTERN`, where the pattern syntax is as specified in [here](http://hspec.github.io/match.html).  
