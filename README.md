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

OR

```
runhaskell -isrc -itest test/Spec.hs
```

## Using QuickCheck in ghci

    :m +Test.QuickCheck
    :l src/Cis194/Hw/Week1.hs 

    -- define a fx that generates data
    let negInts = choose(-10000000, -1)

    -- define a Property that runs function and asserts something about result
    let toDigitsEmpty x = (toDigits x) == []

    -- run QuickCheck to validate property
    quickCheck (forAll negInts toDigitsEmpty)
