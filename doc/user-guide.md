% Apple User Guide
% Vanessa McHale

# Introduction

Apple is an experimental compiler structured as a library.

Everything is an expression; there are no imports.

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

### Editor Integration

There is a [vim plugin](https://github.com/vmchale/apple/tree/canon/vim) and a
[VSCode extension](https://marketplace.visualstudio.com/items?itemName=vmchale.apple). The Vim plugin has digraphs which may be helpful, `:h apple`.

The file extension is `.🍎` or `.🍏`.

## Python Extension Module

To JIT compile a function:

```python
>>> import apple
>>> moving_average=apple.jit('([((+)/x)%(ℝ(:x))]\\`7)')
```

Apple accepts and returns NumPy arrays:

```python
>>> import numpy as np
>>> moving_average(np.arange(0,10,dtype=np.float64))
array([3., 4., 5., 6.])
```

### Debug Facilities

```
>>> apple.typeof('([((+)/x)%(ℝ(:x))]\\`7)')
'Vec (i + 7) float → Vec i float'
```

We can inspect generated assembly with:

```
>>> print(apple.asm('[(+)/((*)`((x::Vec n float)) y)]'))

    mov rdx, [rdi+8]
    movq xmm3, [rdi+16]
    movq xmm2, [rsi+16]
    vmulsd xmm0, xmm3, xmm2
    mov rcx, 1
    cmp rcx, rdx
    jge apple_1
apple_0:
    movq xmm3, [rdi+8*rcx+16]
    vfmadd231sd xmm0, xmm3, [rsi+8*rcx+16]
    add rcx, 1
    cmp rcx, rdx
    jl apple_0
apple_1:
    ret
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

### Row Types

Apple uses [row types](http://blog.vmchale.com/article/row-types) for accessing
tuples. This allows us to write:

```
 > (1.0,2::int)->2
2
 > (2.0,1::int,#f)->2
1
```

in a way that is still type-safe. Consider:

```
 > [(x,y)->3]
1:2: could not unify '{ρ | 3: b}' with '(a * a)' in expression '(x, y)'
```

```
 > :ty (->2)
{ρ | 2: a} → a
```

## Compilation

One can compile an Apple function to an object file with `writeo`, viz.

```
writeo test/examples/kl.🍎 -f kullback_leibler
```

This generates `kullback_leibler.o` and `kullback_leibler.h`, which has wrappers
to
