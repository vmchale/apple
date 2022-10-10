# Apple Array System

## Compiler-As-a-Library

Rather than an environment-based interpreter or a compiler invoked on the
command line and generating object files, Apple generates machine code which can
be used by a JIT compiler or in general.

Thus the same implementation can be used interpreted, compiled, or called from
another language.

## Documentation from Types

Like Haskell's Haddock or Doxygen, one can generate hyperlinked type signatures,
e.g.

```
hypergeometric : Arr (i `Cons` Nil) float -> Arr (j `Cons` Nil) float -> float -> float
```

This saves the author from writing redundant documentation.

Programmatic type inference can be of aid in debugging.

## Dimension As a Functor

This is based on J (and APL?). Looping is replaced by functoriality (map); we
have a family of functors `('n)`

<!-- rank is "bottom-up" -->

## Static Memory Allocation

Because there are no references and conditionals are rare, memory
allocations/frees are inserted up-front.

<!-- reuse analysis -->
<!-- no gc etc., comes from array &c. -->

## Special Combinations

Apple takes inspiration from [J's special combinations](https://code.jsoftware.com/wiki/Vocabulary/SpecialCombinations).

## Moving Code vs. Moving Data

> For a computation to take place the data and the program have to be at the
> same point in space-time - this is just physics. You can move the data to the
> program or the program to the data, or both somewhere else. ...
> data movement predominates.

- [Joe Armstrong](https://twitter.com/joeerl/status/1087678726911987712)

## Property Testing

Types act as witnesses, as in [QuickCheck](https://hackage.haskell.org/package/QuickCheck).
