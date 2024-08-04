% Apple by Example
% Vanessa McHale

# Introduction

Apple is an experimental compiler with a working though not-perfectly-designed
typed frontend.

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

## Outer Product

The outer product `⊗` creates a table by applying some function.

It has type

```
 > :ty \f.\x.\y. x f⊗ y
(a → b → c) → Arr sh0 a → Arr sh1 b → Arr (sh0 ⧺ sh1) c
```

```
 > (frange 0 9 10) (*)⊗ (frange 0 9 10)
[ [0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0]
, [0.0, 1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0, 9.0]
, [0.0, 2.0, 4.0, 6.0, 8.0, 10.0, 12.0, 14.0, 16.0, 18.0]
, [0.0, 3.0, 6.0, 9.0, 12.0, 15.0, 18.0, 21.0, 24.0, 27.0]
, [0.0, 4.0, 8.0, 12.0, 16.0, 20.0, 24.0, 28.0, 32.0, 36.0]
, [0.0, 5.0, 10.0, 15.0, 20.0, 25.0, 30.0, 35.0, 40.0, 45.0]
, [0.0, 6.0, 12.0, 18.0, 24.0, 30.0, 36.0, 42.0, 48.0, 54.0]
, [0.0, 7.0, 14.0, 21.0, 28.0, 35.0, 42.0, 49.0, 56.0, 63.0]
, [0.0, 8.0, 16.0, 24.0, 32.0, 40.0, 48.0, 56.0, 64.0, 72.0]
, [0.0, 9.0, 18.0, 27.0, 36.0, 45.0, 54.0, 63.0, 72.0, 81.0] ]
```

```
 > (frange 0 4 5) [(x,y)]⊗ (frange 0 4 5)
[ [(0.0*0.0), (0.0*1.0), (0.0*2.0), (0.0*3.0), (0.0*4.0)]
, [(1.0*0.0), (1.0*1.0), (1.0*2.0), (1.0*3.0), (1.0*4.0)]
, [(2.0*0.0), (2.0*1.0), (2.0*2.0), (2.0*3.0), (2.0*4.0)]
, [(3.0*0.0), (3.0*1.0), (3.0*2.0), (3.0*3.0), (3.0*4.0)]
, [(4.0*0.0), (4.0*1.0), (4.0*2.0), (4.0*3.0), (4.0*4.0)] ]
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

## Numerics

### Arithmetic-Geometric Mean

```
λx.λy.([{a⟜x->1;g⟜x->2;((a+g)%2,√(a*g))}]^:6 (x,y))->1
```

Thence to compute the [complete elliptic integral of the first kind](https://en.wikipedia.org/wiki/Elliptic_integral#Complete_elliptic_integral_of_the_first_kind):

```
λk.
{
  agm ← λx.λy.([{a⟜x->1;g⟜x->2;((a+g)%2,√(a*g))}]^:6 (x,y))->1;
  𝜋%(2*agm 1 (√(1-k^2)))
}
```

To compute the logarithm, turn to Gauss:

```
λm.λx.
{
  amgm ← λx.λy.([{a⟜x->1;g⟜x->2;((a+g)%2,√(a*g))}]^:15 (x,y))->1;
  -- m>2
  𝜋%(2*amgm 1 (0.5^(m-2)%x))-ℝ m*0.6931471805599453
}
```

### Hypergeometric Function

$$ {}_pF_q(a_1,\ldots,a_p;b_1,\ldots,b_q;x) = \sum_{n=0}^\infty \frac{(a_1)_n\ldots (a_p)_n}{(b_1)_n\ldots (b_q)_n}\frac{x^n}{n!}$$

where $(x)_n$ is the [rising factorial](#rising-factorial) above.

```
λa.λb.λz.
{
  rf ← [(*)/ₒ 1 (𝒻 x (x+y-1) (⌊y))]; fact ← rf 1;
  Σ ← λN.λa. (+)/ₒ 0 (a'(⍳ 0 N 1)); Π ← [(*)/x];
  Σ 30 (λn. {nn⟜ℝ n; (Π ((λa.rf a nn)'a)%Π((λb. rf b nn)'b))*(z^n%fact nn)})
}
```

We can use the REPL to inspect the type:

```
 > :yank H math/hypergeometric.🍏
 > :ty H
Vec (i + 1) float → Vec (i + 1) float → float → float
```

## Geography

### Mercator

$$ x \mapsto \lambda - \lambda_0 $$
$$ y \mapsto \frac{1}{2} \log{\left(\frac{1+\sin \phi}{1-\sin \phi}\right)} $$

```
\𝜆₀.\𝜑.\𝜆.{a⟜sin.𝜑;(𝜆-𝜆₀,(_.((1+a)%(1-a)))%2)}
```

## Number Theory

### Primality Check

```
λn.¬((∨)/ₒ #f ([(n|x)=0]'(⍳ 2 (⌊(√(ℝn))) 1)))
```

### Radical

Compute the radical of an integer $n$, $\displaystyle \prod_{p|n} p$

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

## Python Extension Module

### Sample Normal Distribution

To generate normally distributed random values, we can use

$$ X = \sqrt{-2 \log B_1} \cos (2\pi B_2) $$

where $B_1$, $B_2$ are uniformly distributed $\in [0,1)$.

Debugging randomness is fraught, so we turn to Python's visualization
libraries.

```{.include}
nb/hist.html
```

### Image Processing

`([((+)/* 0 (x::Arr (7 × 7) float))%ℝ(:x)] ⨳ {7,7})` applies a 7x7 mean filter to a
2-dimensional array.

We can use [Pillow](https://python-pillow.org/) to apply it to an image:

```{.include}
nb/convolve.html
```
