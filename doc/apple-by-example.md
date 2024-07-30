% Apple by Example
% Vanessa McHale

# Introduction

Apple is an experimental compiler with a working though not-perfectly-designed
typed frontend.

## Installation

The REPL and typechecker are available on the [release page](https://github.com/vmchale/apple/releases).

<!-- To install the libraries, you will need [ghcup](https://www.haskell.org/ghcup/) -->

## Reference Material

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

# Examples

## Rising Factorial

The [rising factorial](https://mathworld.wolfram.com/RisingFactorial.html) is defined as:

$$ x^{(n)} = x(x+1)\cdots (x+n-1)$$

In apple this is

```
[(*)/ₒ 1 (⍳ x (x+y-1) 1)]
```

`/ₒ` is a ternary operator, fold with seed.

## Shoelace Theorem

```
λas.λbs. 
    { sum ⇐ [(+)/x]
    ; 0.5*abs.(sum((*)`as (1⊖bs)) - sum((*)`(1⊖as) bs))
    }
```
