% Apple User Guide
% Vanessa McHale

# Introduction

Apple is an experimental compiler structured as a library.

## Installation

The REPL and typechecker are available on the [release page](https://github.com/vmchale/apple/releases).

### Libraries

To install the libraries, you can use [ghcup](https://www.haskell.org/ghcup/) to install cabal and GHC.

Then:

```
make
sudo make install-lib
```

To install the Python extension module:

```
make install-py
```

To use within R, install the `libappler` shared library with:

```
make -C Rc
sudo make install-r
```

Then:

```
source("R/apple.R")
```

## Type System

Apple has shape types, like
[Accelerate](https://hackage.haskell.org/package/accelerate) or
[Repa](https://hackage.haskell.org/package/repa).

### Existential Types

Apple has existential types, consider `§` (filter):

```
 > :ty (even.§)
Vec i int → Vec #n int
```

Here `Vec #n int` denotes `∃m. Vec m int`.

## Compilation

One can compile an Apple function to an object file with `writeo`, viz.

```
writeo test/examples/kl.🍎 -f kullback_leibler
```

This generates `kullback_leibler.o` and `kullback_leibler.h`, which has wrappers
to
