# Apple Array System

This is an experimental compiler for an array domain-specific language (DSL) targeting Aarch64.

See [Apple by Example](https://vmchale.github.io/apple/) for a tour of the
language.

The compiler will bail out with arcane error messages rather than
produce an incorrect result (some cases are not implemented), except that the Python/R extension modules do not
enforce dimension inferred from types and may mysteriously produce unpredictable corrupt results.

## Compiler-As-a-Library

Rather than an environment-based interpreter or a compiler invoked on the
command line and generating object files, one calls a library function which
returns assembly or machine code from a source string.

Thus the same implementation can be used interpreted, compiled, or called from
another language.

```
 > [(+)/x%ℝ(:x)]\`7 (frange 1 10 10)
Arr (4) [4.0, 5.0, 6.0, 7.0]
```

```python
>>> import apple
>>> import numpy as np
>>> sliding_mean=apple.jit('([(+)/x%ℝ(:x)]\\`7)')
>>> sliding_mean(np.arange(0,10,dtype=np.float64))
array([3., 4., 5., 6.])
```

```janet
repl:1:> (import apple)
@{_ @{:value <cycle 0>} apple/jit @{:private true} apple/tyof @{:private true}}
repl:2:> (def sliding-mean (apple/jit ``([(+)/x%ℝ(:x)]\`7)``))
<jit Vec (i + 7) float → Vec i float>
repl:3:> (sliding-mean @[0.0 1.0 2.0 3.0 4.0 5.0 6.0 7.0 8.0 9.0 10.0])
@[3 4 5 6 7]
```

```R
> source("R/apple.R")
> sliding_mean<-jit("([(+)/x%ℝ(:x)]\\`7)")
> run(sliding_mean,seq(0,10,1.0))
[1] 3 4 5 6 7
```

Apple [tends to be faster than R](http://blog.vmchale.com/article/r-perf) but
lags NumPy.

### JIT'ed Calculator

There are no imports.

Recursive functions are not allowed.

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
  λA.λx. (x⋅)`{1∘[2]} (A::Arr (i × j) float)
}
```

The `2` means "iterate over the second axis" i.e. columns.

## Array QuickCheck

```
 > :qc \x. [(+)/(*)`x y] x x >= 0.0
Passed, 100.
 > :qc \x. [(+)/(*)`x y] x x > 2.0
Proposition failed!
[ Arr (5) [ 0.6213045301664751
          , 0.6599381241699802
          , 0.762478867048601
          , 6.026206825450409e-3
          , 0.5633419282435523 ] ]
```

Test cases are generated based on inferred type, nonempty vectors in this case.

```
 > :ty \x. [(+)/(*)`x y] x x > 2.0
Vec (i + 1) float → bool
```

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

To install the shared library (requires [jq](https://jqlang.github.io/jq/)).

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

### Janet

Uses [jpm](https://janet-lang.org/docs/jpm.html).

```
make -C janet install
```

## Documentation

Type `\l` in the REPL to show the reference card:

```
 > \l
Λ             scan                     √             sqrt
⋉             max                      ⋊             min
⍳             integer range            ⌊, ⌈          floor, ceiling
e:            exp                      ⨳ {m,n}       convolve
\~            successive application   \`n           infix
_.            log                      '             map
`             zip                      `{i,j∘[k,l]}  rank
𝒻             range (real)             𝜋             pi
_             negate                   :             size
𝓉             dimension                {x⟜y;z}       no inline
->n           select                   **            power
⊂             scatter                  }.            last
⊲             cons                     ⊳             snoc
^:            iterate                  %.            matmul
⊗             outer product            ⍉, |:         transpose
{.            head                     }:            typesafe init
⟨z,w⟩         array literal            ?p,.e1,.e2    conditional
...
```

Enter `:help` in REPL:

```
 > :help
:help, :h                    Show this help
:yank, :y      <fn> <file>   Read file
:store, :st    <name> <expresAdd to environment
:ty            <expression>  Display the type of an expression
:ann           <expression>  Annotate with types
...
```

### Vim Plugin

`:h apple` lists potentially useful digraphs, viz.

```
←   <-
⟜   o-
𝒻   ff
⊲   <\
⊳   \>
⋮
```

### Python Module

To display module documentation:

```python
>>> import apple
>>> help(apple)
```

```
CLASSES
    builtins.object
        AppleJIT

    class AppleJIT(builtins.object)
     |  JIT-compiled function in-memory
     |
     |  Methods defined here:
     |
     |  __call__(self, /, *args, **kwargs)
     |      Call self as a function.
     |
     |  ----------------------------------------------------------------------

FUNCTIONS
    asm(...)
        Dump assembly

    ir(...)
        Dump IR (debug)

    jit(...)
        Compile an expressoin into a callable object

    typeof(...)
        Display type of expression
```

### Janet

```janet
repl:2:> (import apple)
@{_ @{:value <cycle 0>} apple/jit @{:private true} apple/tyof @{:private true}}
repl:4:> (doc apple/jit)


    cfunction

    Compile source string into Janet callable


nil
repl:5:> (doc apple/tyof)


    cfunction

    type of expression


nil
```
