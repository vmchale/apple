# Apple Array System

## Compiler-As-a-Library

Rather than an environment-based interpreter or a compiler invoked on the
command line and generating object files, Apple generates machine code which can
be used by a JIT compiler or in general.

Thus the same implementation can be used interpreted, compiled, or called from
another language.

```
 > [((+)/x)%ℝ(:x)]\`7 (frange 1 10 10)
Arr (4) [4.0, 5.0, 6.0, 7.0]
```

```python
>>> import apple
>>> import numpy as np
>>> apple.apple('([((+)/x)%(ℝ(:x))]\`7)',np.arange(0,10,dtype=np.float64))
array([3., 4., 5., 6.])
>>>
```

```R
> source("R/apple.R")
> apple("([((+)/x)%ℝ(:x)]\\`7)",seq(0,10,1.0))
[1] 3 4 5 6 7
```

## Dimension As a Functor

This is based on J (and APL?). Looping is replaced by functoriality (rerank).

## Moving Code vs. Moving Data

> For a computation to take place the data and the program have to be at the
> same point in space-time - this is just physics. You can move the data to the
> program or the program to the data, or both somewhere else. ...
> data movement predominates.

- [Joe Armstrong](https://twitter.com/joeerl/status/1087678726911987712)

## Installation

Use [ghcup](https://www.haskell.org/ghcup/) to install [cabal](https://www.haskell.org/cabal/) and GHC. Then:

```
make install
```

to install `arepl` (the REPL).

Run

```
make
sudo make install-lib
```

To install the shared library.

### Python

To install the Python module:

```
make install-py
```

### R

Install `libappler.so` on your system like so:

```
make -C Rc
sudo make install-r
```

Then:

```
source("R/apple.R")
```

to access the functions.

## Documentation

Type `\l` in the REPL to show the reference card:

```
 > \l
Λ             scan                     √             sqrt
⋉             max                      ⋊             min
⍳             integer range            ⌊             floor
ℯ             exp                      ⨳ {m,n}       convolve
\~            successive application   \`n           dyadic infix
_.            log                      'n            map
`             zip                      `{i,j∘[k,l]}  rank
𝒻             range (real)             𝜋             pi
_             negate                   :             size
𝓉             dimension                }.?           last
->n           select                   **            power
gen.          generate                 𝓕             fibonacci
re:           repeat                   }.            typesafe last
⊲             cons                     ⊳             snoc
^:            iterate                  %.            matmul
⊗             outer product            |:            transpose
{.?           head                     {.            typesafe head
}.?           last                     }:            typesafe init
⟨z,w⟩         array literal            ?p,.e1,.e2    conditional
...
```

Enter `:help` in REPL:

```
 > :help
:help, :h                    Show this help
:ty            <expression>  Display the type of an expression
...
```
