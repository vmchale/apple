# Apple Array System

Many cases (folds of tuples, folds of arrays) are not implemented. Boolean
arrays are completely missing. This is
provided as an artefact.

In general, the compiler will bail out with arcane error messages rather than
produce an incorrect result, except that the Python/R extension modules do not
enforce type safety and thus may mysteriously segfault or produce unpredictable corrupt results!

Spilling (during register allocation) is not implemented for Arm. Also
floating-point registers aren't spilled on x86.

## Compiler-As-a-Library

Rather than an environment-based interpreter or a compiler invoked on the
command line and generating object files, one calls a library function which
returns assembly or machine code from a source string.

Thus the same implementation can be used interpreted, compiled, or called from
another language.

```
 > [((+)/x)%ℝ(:x)]\`7 (frange 1 10 10)
Arr (4) [4.0, 5.0, 6.0, 7.0]
```

```python
>>> import apple
>>> import numpy as np
>>> sliding_mean=apple.jit('([((+)/x)%(ℝ(:x))]\`7)')
>>> apple.f(sliding_mean,np.arange(0,10,dtype=np.float64))
array([3., 4., 5., 6.])
>>>
```

```R
> source("R/apple.R")
> sliding_mean<-jit("([((+)/x)%ℝ(:x)]\\`7)")
> run(sliding_mean,seq(0,10,1.0))
[1] 3 4 5 6 7
```

## Dimension As a Functor

This is based on J (and APL?). Looping is replaced by functoriality (rerank).

To supply a zero-cells (scalars) as the first argument to `⊲` (cons) and 1-cells as the second:

```
(⊲)`{0,1}
```

We can further specify that the cells should be selected along some axis, e.g.
to get vector-matrix multiplication:

```
λA.λx.
{
  dot ⇐ [(+)/((*)`x y)];
  (dot x)`{1∘[2]} (A::Arr (i`Cons`j`Cons`Nil) float)
}
```

The `2` means "iterate over the second axis" i.e. columns.

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
