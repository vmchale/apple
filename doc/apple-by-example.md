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

## Editor Integration

There is a [vim plugin](https://github.com/vmchale/apple/tree/canon/vim) and a
[VSCode extension](https://marketplace.visualstudio.com/items?itemName=vmchale.apple).

The file extension is `.🍎` or `.🍏`.

## Source

Source is archived on [Hackage](https://hackage.haskell.org/package/apple).

# Capabilities

## Integer Range

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

Note that `_` is used for negative literals.

## Real Range

For a range of real numbers, use `frange` or `𝒻`.

```
 > :ty frange
float → float → int → Vec #n float
```

`frange` takes a start value, an end value, and the number of steps.

```
 > frange 0 9 10
[0.0, 1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0, 9.0]
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
 > ⟨_1,0::int⟩
[-1, 0]
```

```
 > ⟨⟨0,1⟩,⟨_1,0::int⟩⟩
[ [0, 1]
, [-1, 0] ]
```

## Reverse

`~` reverses an array.

```
 > ~(irange 0 9 1)
[9, 8, 7, 6, 5, 4, 3, 2, 1, 0]
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

```
 > ~'⟨⟨0,1⟩,⟨1,0::int⟩,⟨2,4⟩⟩
[ [1, 0]
, [0, 1]
, [4, 2] ]
```

## Outer Product

The outer product `⊗` creates a table by applying some function.

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

## Successive Application

```
 > :ty (\~)
(a → a → b) → Arr (i + 1 `Cons` sh) a → Arr (i `Cons` sh) b
```

`[(-)\~ x]` gives successive differences.

```
 > (-)\~ ((^2)'(frange 0 9 10))
[1.0, 3.0, 5.0, 7.0, 9.0, 11.0, 13.0, 15.0, 17.0]
```

## Rotate

```
 > (⊖)
(⊖) : int → Vec i a → Vec i a
```

```
 > 2 ⊖ irange 0 9 1
[2, 3, 4, 5, 6, 7, 8, 9, 0, 1]
 > _2 ⊖ irange 0 9 1
[8, 9, 0, 1, 2, 3, 4, 5, 6, 7]
```

```
 > 2 ⊖ ⟨⟨1,2⟩,⟨3,4⟩,⟨5,6.0⟩⟩
Arr (3×2) [ [5.0, 6.0]
          , [1.0, 2.0]
          , [3.0, 4.0] ]
```

## Cycle

`cyc.` or `⊙` (infix) concatenates an array with itself a specified number of times.

```
 > cyc. ⟨0::int,1⟩ 4
Vec 8 [0, 1, 0, 1, 0, 1, 0, 1]
```

```
 > ⟨0::int,1⟩⊙4
Vec 8 [0, 1, 0, 1, 0, 1, 0, 1]
```

## Repeat

```
 > re: 3 ⟨1.0,0⟩
Arr (3×2) [ [1.0, 0.0]
          , [1.0, 0.0]
          , [1.0, 0.0] ]
```

## Transpose

⍉ or `|:`

```
 > ⍉ ⟨⟨1.0,3⟩,⟨4,4⟩,⟨2,_2⟩⟩
[ [1.0, 4.0, 2.0]
, [3.0, 4.0, -2.0] ]
 > ⟨⟨1.0,3⟩,⟨4,4⟩,⟨2,_2⟩⟩
[ [1.0, 3.0]
, [4.0, 4.0]
, [2.0, -2.0] ]
```

## Unfold

```
 > gen. (1::int) (*2) 10
Vec 10 [1, 2, 4, 8, 16, 32, 64, 128, 256, 512]
```

## Dyadic Infix

Moving average:

```
 > [((+)/x)%ℝ(:x)]\` 7 (frange 0 9 10)
[3.0, 4.0, 5.0, 6.0]
```

```
 > [x]\`4 (frange 0 5 6)
[ [0.0, 1.0, 2.0, 3.0]
, [1.0, 2.0, 3.0, 4.0]
, [2.0, 3.0, 4.0, 5.0] ]
```

## Bind

```
 > {i←2::int;i*i}
4
```

Bind, preventing inlining:

```
 > {i⟜2::int;i*i}
4
```

One can see that `2` is stored in a register by inspecting the generated
assembly:

```
 > :asm {i←2::int;i*i}

    mov x0, #0x4
    ret
 > :asm {i⟜2::int;i*i}

    mov x0, #0x2
    mul x0, x0, x0
    ret
```

### Polymorphic Bind

```
 > {sum ⇐ [(+)/x]; sum (irange 0 9 1)+⌊(sum(frange 0 9 10))}
90
```

```
 > {sum ← [(+)/x]; sum (irange 0 9 1)+⌊(sum(frange 0 9 10))}
1:42: could not unify 'float' with 'int' in expression '𝒻 0 9 10'
```

## Rank

Rank ```{i,j∘[k,l]}`` lifts a function to operate on i, j-cells, optionally
specifying axes k,l. Iteration is bottom-up; by contrast map `'` cuts across the leading
dimension.

To make a scalar function apply to arrays, re-rank

```
 > :ty ((*)`{0,0})
(IsNum c) :=> Arr sh c → Arr sh c → Arr sh c
```

Sigmoid on an arbitrary-dimension array:

```
([1%(1+ℯ(_x))]`{0})
```

```
 > ⟨⟨0,1,2⟩,⟨3,4,5::int⟩⟩
Arr (2×3) [ [0, 1, 2]
          , [3, 4, 5] ]
 > {sum←[(+)/x]; sum`{1} ⟨⟨0,1,2⟩,⟨3,4,5::int⟩⟩}
Vec 3 [3, 5, 7]
 > {sum←[(+)/x]; sum`{1∘[2]} ⟨⟨0,1,2⟩,⟨3,4,5::int⟩⟩}
Vec 2 [3, 12]
```

This may be confusing; Apple's rank feature was poorly designed.

Take 0-cells (scalars) from the first array and 1-cells from the second.

```
 > (⊲)`{0,1∘[2]} ⟨0::int,1⟩ ⟨⟨2,3⟩,⟨4,5⟩⟩
Arr (2×3) [ [0, 2, 3]
          , [1, 4, 5] ]
```

```
 > ⍉ ((2 ⊖)`{1} ⟨⟨1,2⟩,⟨3,4⟩,⟨5,6.0⟩⟩)
Arr (3×2) [ [5.0, 6.0]
          , [1.0, 2.0]
          , [3.0, 4.0] ]
```

```
 > :ty [♭`{3∘[2,3,4]} (x :: Arr (60000 × 28 × 28 × 1) float)]
Arr (60000 × 28 × 28 × 1) float → Arr (60000 × 784) float
```

## REPL Functionality

### Benchmark

```
 > :bench frange 0 999 1000
benchmarking...
time                 1.423 μs   (1.417 μs .. 1.427 μs)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 1.426 μs   (1.422 μs .. 1.429 μs)
std dev              11.94 ns   (9.819 ns .. 14.22 ns)
```

### QuickCheck

Apple can generate shape-correct test cases for property testing. For instance,

```
 > :qc \x. [(+)/(*)`x y] x x >= 0.0
Passed, 100.
```

tests that the dot product of a vector with itself is nonnegative.

## Exotic Syntax

### Coronis

Instead of

```
{x←y;z}
```

One can write

```
⸎x←y;z
```

Using the [typographical coronis](https://en.wikipedia.org/wiki/Coronis_(textual_symbol)).

### Matrix Dimensions

One can specify matrix dimensions in a type signature with unicode subscript
digits separated by a comma.

```
(𝔯 0 1) :: M ₁₂,₁₂ float
```

is equivalent to

```
(𝔯 0 1) :: Arr (12 × 12) float
```

### Identity Matrix

👁️ can be used in place of `eye.` for the identity matrix.

# Examples

## Kullback-Leibler Divergence

```
λp.λq. (+)/([x*_.(x%y)]`p q)
```

## Train Neural Network

```
λwh.λwo.λbh.λbo.
{ X ⟜ ⟨⟨0,0⟩,⟨0,1⟩,⟨1,0⟩,⟨1,1⟩⟩;
  Y ⟜ ⟨0,1,1,0⟩;
  sigmoid ← [1%(1+ℯ(_x))];
  sDdx ← [x*(1-x)];
  sum ⇐ [(+)/x];
  ho ⟜ sigmoid`{0} ([(+)`bh x]'(X%.wh));
  prediction ⟜ sigmoid'((+bo)'(ho%:wo));
  l1E ← (-)`Y prediction;
  l1Δ ⟜ (*)`(sDdx'prediction) l1E;
  he ← l1Δ (*)⊗ wo;
  hΔ ⟜ (*)`{0,0} (sDdx`{0} ho) he;
  wha ← (+)`{0,0} wh ((|:X)%.hΔ);
  woa ← (+)`wo ((|:ho)%:l1Δ);
  bha ← [(+)/ₒ x y]`{0,1} bh hΔ;
  boa ← bo + sum l1Δ;
  (wha,woa,bha,boa)
}
```

This is equivalent to the [Python](https://towardsdatascience.com/implementing-the-xor-gate-using-backpropagation-in-neural-networks-c1f255b4f20d):

```python
import numpy as np

def sigmoid (x):
    return 1/(1 + np.exp(-x))

def sigmoid_derivative(x):
    return x * (1 - x)

inputs = np.array([[0,0],[0,1],[1,0],[1,1]])
expected_output = np.array([[0],[1],[1],[0]])

hidden_layer_activation = np.dot(inputs,hidden_weights)
hidden_layer_activation += hidden_bias
hidden_layer_output = sigmoid(hidden_layer_activation)

output_layer_activation = np.dot(hidden_layer_output,output_weights)
output_layer_activation += output_bias
predicted_output = sigmoid(output_layer_activation)

#Backpropagation
error = expected_output - predicted_output
d_predicted_output = error * sigmoid_derivative(predicted_output)

error_hidden_layer = d_predicted_output.dot(output_weights.T)
d_hidden_layer = error_hidden_layer * sigmoid_derivative(hidden_layer_output)

#Updating Weights and Biases
output_weights += hidden_layer_output.T.dot(d_predicted_output)
output_bias += np.sum(d_predicted_output,axis=0,keepdims=True)
hidden_weights += inputs.T.dot(d_hidden_layer)
hidden_bias += np.sum(d_hidden_layer,axis=0,keepdims=True)
```

## [Shoelace Theorem](https://artofproblemsolving.com/wiki/index.php/Shoelace_Theorem)

If a polygon has vertices at $x_n$, $y_n$, then its area is given by

$$A=\frac{1}{2}\Biggl|(x_1y_2+x_2y_3+\cdots+x_ny_1)-(y_1x_2+y_2x_3+\cdots+y_nx_1)\Biggr|$$

```
λxs.λys.
    { sum ⇐ [(+)/x]
    ; 0.5*abs.(sum((*)`xs (1⊖ys)) - sum((*)`(1⊖xs) ys))
    }
```

Note the array style: `⊖`, ` (zip), and fold are enough to eschew pointful definitions.

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

Take the first 7 elements:

```
 > {. ([x] \`7 (irange 0 9 1))
Vec 7 [0, 1, 2, 3, 4, 5, 6]
```

Take the last 7 elements:

```
 > }. ([x] \`7 (irange 0 9 1))
Vec 7 [3, 4, 5, 6, 7, 8, 9]
```

### Matrix-Vector Multiplication

```
λA.λx.
{
  dot ⇐ [(+)/((*)`x y)];
  (dot x)`{1∘[2]} (A::Arr (i × j) float)
}
```

### Filter

```
\p.\xs. (xs˙)'p⩪xs
```

## [Argmax](https://numpy.org/doc/stable/reference/generated/numpy.argmax.html)

```
[{m⟜(⋉)/(x::Vec n float); (=m)@.x}]
```

## [Luhn Check](https://en.wikipedia.org/wiki/Luhn_algorithm)

```
λxs.
  { digitSum ← [?x>10,.x-9,.x]
  ; t ← (+)/ [digitSum (x*y)]`(~(}:xs)) (}: (cyc. ⟨2,1::int⟩ 8))
  ; 10-(t|10)=}.xs
  }
```

Note zipping with `cyc. ⟨2,1::int⟩ 8` to get alternating 2, 1, ... factors.

## Elliptic Fourier Series

From [Kuhl and Giardnia](http://www.sci.utah.edu/~gerig/CS7960-S2010/handouts/Kuhl-Giardina-CGIP1982.pdf), the coefficients are given by:

$$ a_n = \frac{T}{2n^2\pi^2}\sum_{p=1}^K \frac{\Delta x_p}{\Delta t_p}\left(\cos\frac{2n\pi t_p}{T} - \cos\frac{2n\pi t_{p-1}}{T}\right) $$

$$ b_n = \frac{T}{2n^2\pi^2}\sum_{p=1}^K \frac{\Delta x_p}{\Delta t_p}\left(\sin\frac{2n\pi t_p}{T} - \sin\frac{2n\pi t_{p-1}}{T}\right) $$

$$ c_n = \frac{T}{2n^2\pi^2}\sum_{p=1}^K \frac{\Delta y_p}{\Delta t_p}\left(\cos\frac{2n\pi t_p}{T} - \cos\frac{2n\pi t_{p-1}}{T}\right) $$

$$ d_n = \frac{T}{2n^2\pi^2}\sum_{p=1}^K \frac{\Delta y_p}{\Delta t_p}\left(\sin\frac{2n\pi t_p}{T} - \sin\frac{2n\pi t_{p-1}}{T}\right) $$

The offsets are given by:

$$ A_0 = \frac{1}{T} \sum_{p=1}^K \left[\frac{\Delta x_p}{2\Delta t_p} (t_p^2-t_{p-1}^2) + \xi_p (t_p-t_{p-1}^2)\right]$$

$$ C_0 = \frac{1}{T} \sum_{p=1}^K \left[\frac{\Delta y_p}{2\Delta t_p} (t_p^2-t_{p-1}^2) + \delta_p (t_p-t_{p-1}^2)\right]$$

where

$$ \xi_p = \sum_j^{p-1} \Delta x_j - \frac{\Delta x_p}{\Delta t_p} \sum_j^{p-1}\Delta t_j $$

$$ \delta_p = \sum_j^{p-1} \Delta y_j - \frac{\Delta y_p}{\Delta t_p} \sum_j^{p-1}\Delta t_j $$

$$ \xi_0,~\delta_0 = 0 $$

In Apple we can generate the first `N` coefficients alongside the offsets with:

```
λxs.λys.λN.
  { sum ← [(+)/x]
  ; tieSelf ← [({.x)⊳x]; Δ ← [(-)\~(tieSelf x)]
  ; dxs ⟜ Δ xs; dys ⟜ Δ ys
  ; dts ⟜ [√(x^2+y^2)]`dxs dys
  ; dxss ⟜ ((%)`dxs dts); dyss ⟜ ((%)`dys dts)
  ; pxs ← (+)Λ dxs; pys ← (+)Λ dys; pts ⟜ (+)Λₒ 0 dts; T ⟜}. pts
  ; coeffs ← λn.
    { n ⟜ ℝn; k ⟜ 2*n*𝜋%T
    ; cosDiffs ⟜ (-)\~([cos.(k*x)]'pts)
    ; sinDiffs ⟜ (-)\~([sin.(k*x)]'pts)
    ; c ⟜ T%(2*n^2*𝜋^2)
    ; aₙ ← c*sum ((*)`dxss cosDiffs)
    ; bₙ ← c*sum ((*)`dxss sinDiffs)
    ; cₙ ← c*sum ((*)`dyss cosDiffs)
    ; dₙ ← c*sum ((*)`dyss sinDiffs)
    ; (aₙ,bₙ,cₙ,dₙ)
    }
  ; dtss ⟜ (-)\~((^2)'pts)
  ; ppts ⟜ {: pts
  ; 𝜉 ← (-)`pxs ((*)`((%)`dxs dts) ppts)
  ; 𝛿 ← (-)`pys ((*)`((%)`dys dts) ppts)
  ; A ← (0.5*sum ((*)`((%)`dxs dts) dtss) + sum ((*)`𝜉 dts))%T
  ; C ← (0.5*sum ((*)`((%)`dys dts) dtss) + sum ((*)`𝛿 dts))%T
  ; (coeffs'(irange 1 N 1),A,C)
  }
```

Note the array style, e.g. `(-)\~` to define successive differences on an
array rather than definining pointfully.

## Geography

### [Albers](https://mathworld.wolfram.com/AlbersEqual-AreaConicProjection.html)

Let 𝜆₀, 𝜑₀ be the coördinates of the origin, 𝜑₁, 𝜑₂ standard parallels, `φs` and `lambdas` the longitudes and latitudes, respectively.

```
\𝜆₀.\𝜑₀.\𝜑₁.\𝜑₂.\φs.\lambdas.
{
  𝑛 ⟜ (sin. 𝜑₁+sin.𝜑₂)%2;
  𝐶 ⟜ (cos. 𝜑₁)^2+2*𝑛*sin. 𝜑₁;
  𝜌₀ ⟜ √(𝐶-2*𝑛*sin. 𝜑₀)%𝑛;
  albers ← \𝜑.\𝜆.
    {
      𝜃 ⟜ 𝑛*(𝜆-𝜆₀);
      𝜌 ⟜ √(𝐶-2*𝑛*sin. 𝜑)%𝑛;
      (𝜌*sin. 𝜃, 𝜌₀-𝜌*cos. 𝜃)
    };
  albers`φs lambdas
}
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

This shows the awkwardness of an array style.

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

### Random Walks

Cliff Reiter points out that we can simulate a random walk by simply scanning an array of random values, viz.

```
(+)Λ ((𝔯 _1 1) :: Vec 200 int)
```

```{.include}
nb/randomWalk.html
```
