% Apple by Example
% Vanessa McHale

# Introduction

Apple is an experimental compiler with a working though not-perfectly-designed
typed frontend.

## Installation

The REPL and typechecker are available on the [release page](https://github.com/vmchale/apple/releases).

### Libraries

To install the libraries, you will can use [ghcup](https://www.haskell.org/ghcup/) to install cabal and GHC.

Then:

```
make
sudo make install-lib
```

To install the Python extension module:

```
make install-py
```

## Reference Card

In the REPL, type `\l`.

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

⋮
```

Use `:ty` for more:

```
 > :ty (⊲)
a → Vec i a → Vec (i + 1) a
```

# Capabilities

## Integer range

To generate an integer range use `irange` or `⍳` (APL iota).

```
 > ⍳
⍳ : int → int → int → Vec #n int
```

`⍳` takes a start value, end value, and step size as arguments, viz.

```
 > ⍳ 0 9 1
[0, 1, 2, 3, 4, 5, 6, 7, 8, 9]
 > irange 30 0 _3
[30, 27, 24, 21, 18, 15, 12, 9, 6, 3, 0]
```

## Map

`'` maps over an array.

```
 > (*2)'⍳ 0 9 1
[0, 2, 4, 6, 8, 10, 12, 14, 16, 18]
```

Functions can be [curried](https://wiki.haskell.org/Currying).

## Fold

`/` folds over an array.

```
 > (+)/⍳ 1 100 1
5050
```

## Array Literals

Array literals are delineated by `⟨`...`⟩`.

```
 > ⟨1,0::int⟩
[1, 0]
```

For a higher-dimensional array:

```
 > ⟨⟨0,1⟩,⟨1,0::int⟩⟩
[ [0, 1]
, [1, 0] ]
```

## Reverse

`~` reverses an array.

```
 > ~(irange 0 9 1)
[9, 8, 7, 6, 5, 4, 3, 2, 1, 0]
 > irange 9 0 _1
[9, 8, 7, 6, 5, 4, 3, 2, 1, 0]
```

Note that we can map over an array.

```
 > ~'⟨⟨0,1⟩,⟨1,0::int⟩⟩
[ [1, 0]
, [0, 1] ]
```

Reverse applied to a higher-dimensional array reverses elements (sub-arrays) along the first dimension.

```
 > ⟨⟨0,1⟩,⟨1,0::int⟩,⟨2,4⟩⟩
[ [0, 1]
, [1, 0]
, [2, 4] ]
 > ~⟨⟨0,1⟩,⟨1,0::int⟩,⟨2,4⟩⟩
[ [2, 4]
, [1, 0]
, [0, 1] ]
```

# Examples

## Dot Product

```
[(+)/ ((*)`((x::Vec n float)) y)]
```

`/` is fold and ` is zip. Note that we must provide a type annotation

We can inspect the assembly:

```
 > :asm [(+)/ ((*)`((x::Vec n float)) y)]

    ldr x4, [x0, #0x8]
    ldr d3, [x0, #0x10]
    ldr d2, [x1, #0x10]
    fmul d0, d3, d2
    mov x2, #0x1
    cmp x2, x4
    b.GE apple_1
apple_0:
    add x3, x2, #0x2
    ldr d3, [x0, x3, LSL #3]
    add x3, x2, #0x2
    ldr d2, [x1, x3, LSL #3]
    fmadd d0, d3, d2, d0
    add x2, x2, #0x1
    cmp x2, x4
    b.LT apple_0
apple_1:
    ret
```

## Rising Factorial

The [rising factorial](https://mathworld.wolfram.com/RisingFactorial.html) is defined as:

$$ x^{(n)} = x(x+1)\cdots (x+n-1)$$

In apple this is

```
[(*)/ₒ 1 (⍳ x (x+y-1) 1)]
```

`/ₒ` is a ternary operator, fold with seed.

## Kullback-Leibler Divergence

```
λp.λq. (+)/([x*_.(x%y)]`p q)
```

## Shoelace Theorem

```
λas.λbs.
    { sum ⇐ [(+)/x]
    ; 0.5*abs.(sum((*)`as (1⊖bs)) - sum((*)`(1⊖as) bs))
    }
```

## Polynomials

### Evaluation

```
λp.λx. (+)/ ((*)`(~p) (gen. 1 (*x) (𝓉p)))
```

## Array

To take all but the last 6 elements:

```
[{.\`7 x]
```

To drop the first 6 elements:

```
[}.\`7 x]
```

### Filter

```
\p.\xs. (xs˙)'p⩪xs
```

## Number Theory

### Primality Check

```
λn.¬((∨)/ₒ #f ([(n|x)=0]'(⍳ 2 (⌊(√(ℝn))) 1)))
```

### Radical

Compute the radical of an integer $n$, $\prod_{p|n} p$

```
λn.
  { ni ⟜ ⌊(√(ℝn))
  ; isPrime ← λn.¬((∨)/ₒ #f ([(n|x)=0]'(⍳ 2 (⌊(√(ℝn))) 1)))
  ; pf ⇐ (isPrime #.)
  ; pps ⟜  pf ((λk. ((n|k)=0)) #. (⍳ 2 ni 1))
  ; ?ni^2=n
    ,.((*)/ₒ 1 (pf ((n/.)'(}:? pps))⧺pps))
    ,.((*)/ₒ 1 (pf (n ⊲ ((n/.)'pps))⧺pps))
  }
```
