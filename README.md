# Apple Array System

## Compiler-As-a-Library

Rather than an environment-based interpreter or a compiler invoked on the
command line and generating object files, Apple generates machine code which can
be used by a JIT compiler or in general.

Thus the same implementation can be used interpreted, compiled, or called from
another language.

## Dimension As a Functor

This is based on J (and APL?). Looping is replaced by functoriality (map); we
have a family of functors `('n)`

<!-- rank is "bottom-up" -->

## Static Memory Allocation

Because there are no references and conditionals are rare, memory
allocations/frees are inserted up-front.

## Moving Code vs. Moving Data

> For a computation to take place the data and the program have to be at the
> same point in space-time - this is just physics. You can move the data to the
> program or the program to the data, or both somewhere else. ...
> data movement predominates.

- [Joe Armstrong](https://twitter.com/joeerl/status/1087678726911987712)
