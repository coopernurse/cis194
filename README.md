# cis194

Homework solutions for UPenn's CIS 194:

http://www.seas.upenn.edu/~cis194/lectures.html

## Installation

On a mac:

    brew install haskell-platform
    cd cis194
    cabal install hspec
    cabal configure --enable-test
    cabal build
    cabal test

## How to run tests

```
cabal configure --enable-tests && cabal build && cabal test
```
