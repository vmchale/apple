% Apple by Example
% Vanessa McHale

# Introduction

Apple is an experimental compiler with a working though not-perfectly-designed
typed frontend. The language is missing the ability to sort an array but otherwise capable.

## Installation

The REPL and typechecker are available on the [release page](https://github.com/vmchale/apple/releases).

## Reference Card

In the REPL, type `\l`.

```
 > \l
Λ             scan                     √             sqrt
⋉             max                      ⋊             min
⍳             integer range            ⌊             floor
ℯ             exp                      ⨳ {m,n}       convolve
\~            successive application   \`n           infix
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

`:help` inside the REPL:

```
 > :help
:help, :h                    Show this help
:ty            <expression>  Display the type of an expression
:ann           <expression>  Annotate with types
:bench, :b     <expression>  Benchmark an expression
:list                        List all names that are in scope
⋮
```

## Editor Integration

There is a [vim plugin](https://github.com/vmchale/apple/tree/canon/vim) and a
[VSCode extension](https://marketplace.visualstudio.com/items?itemName=vmchale.apple).

The file extension is `.🍎` or `.🍏`.

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

## Zip

Pick the greater value among two vectors:

```
 > (⋉)`⟨0,_1,3.0⟩ ⟨_3,1,3⟩
Vec 3 [0.0, 1.0, 3.0]
```

## Fold

`/` folds over an array.

```
 > (+)/⍳ 1 100 1
5050
```

## Scan

```
 > (+)Λ (irange 1 3 1)
Vec 3 [1, 3, 6]
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

## Reshape

`♭` flattens an array to a vector, `♯` adds a dimension.

```
 > :ty λA. ♭ (A::Arr (28×28×1) a)
Arr (28 × 28 × 1) a → Vec 784 a
 > ♯ (irange 0 9 1)
Arr (1×10) [ [0, 1, 2, 3, 4, 5, 6, 7, 8, 9] ]
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

## Infix

Moving average:

```
 > [(+)/x%ℝ(:x)]\` 7 (frange 0 9 10)
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

## Strong Induction

`𝓕` is like `gen.` but with access to all previously computed values.

```
 > :ty 𝓕
Vec m a → (Vec k a → a) → int(n) → Vec (m + n) a
```

Fibonacci sequence:

```
 > 𝓕 ⟨1::int,1⟩ [}.x+}.(}:x)] 10
Vec 11 [1, 1, 2, 3, 5, 8, 13, 21, 34, 55, 89]
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
 > ((+)/)`{1} ⟨⟨0,1,2⟩,⟨3,4,5::int⟩⟩
Vec 3 [3, 5, 7]
 > ((+)/)`{1∘[2]} ⟨⟨0,1,2⟩,⟨3,4,5::int⟩⟩
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

## Dot Product

`x⋅y` is shorthand for ``(+)/(*)`x y``.

## Random Numbers

`𝔯` or `rand.`

```
 > 𝔯 0 1 :: float
0.582699021207219
 > 𝔯 0 1 :: Arr (3 × 3) float
Arr (3×3) [ [0.9827522631010304, 0.30678513061418045, 0.6008551364891055]
          , [0.608715653358658, 0.2124387982011875, 0.8858951305876062]
          , [0.30465710174579286, 0.15185986406856955, 0.33766190287353126] ]
```

## Tuples

Use `->n` to access the `n`th element of a tuple, viz.

```
 > (1.0,#t,4::int)->1
1.0
 > (1.0,#t,4::int)->2
#t
 > (1.0,#t,4::int)->3
4
```

## Convolve

Convolve `(⨳ {m,n})` is like [infix](#infix) for higher-rank
windows.

```
 > [(⋉)/x] ⨳ {3} ⟨_1,0,4,_2,3,3,1,1,0,_5.0⟩
Vec 8 [4.0, 4.0, 4.0, 3.0, 3.0, 3.0, 1.0, 1.0]
```

```
 > [(⋉)/x] \`3 ⟨_1,0,4,_2,3,3,1,1,0,_5.0⟩
Vec 8 [4.0, 4.0, 4.0, 3.0, 3.0, 3.0, 1.0, 1.0]
```

```
 > ([(+)/* 0 (x::Arr (2×3) float)%ℝ(:x)] ⨳ {2,3}) ⟨⟨1.0,2,0,0⟩,⟨_1,_2,3,4⟩,⟨5,6,3,1⟩,⟨3,1,1,3⟩⟩
Arr (3×2) [ [0.5, 1.1666666666666665]
          , [2.333333333333333, 2.5]
          , [3.1666666666666665, 2.5] ]
```

## REPL Functionality

### Load

```
 > :yank chisqcdf math/chisqcdf.🍎
 > chisqcdf 10 28
0.9982004955179669
```

`math/chisqcdf.🍎` is a file that contains an expression of type `float → float→ float`.


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
𝔯 0 1 :: M ₁₂,₁₂ float
```

is equivalent to

```
𝔯 0 1 :: Arr (12 × 12) float
```

### Identity Matrix

👁️ can be used in place of `eye.` for the identity matrix.

### Identifiers

Identifiers may be latin or greek characters, or a single character from the
mathematical greek or mathematical latin unicode block, optionally followed by
some subscript alphanumeric characters. The single-character lambda `λ` is reserved. `∫`, `𝛻`,
and `∇` are also identifiers but may not be followed by a subscript.

Thus `pxs`, `aₙ`, `sn₁`, `𝐶`, `φs`, `𝜉`, and `𝜌₀` are valid identifiers but `𝜉s` is not.

### Vulgar Fractions

Unicode vulgar fractions are considered float literals:

```
 > ⅚
0.8333333333333334
```

One can use `⅟` for the reciprocal, viz.

```
 > [⅟(1+ℯ(_x))]
λx. 1.0 % (1 + 2.718281828459045 ** _ x) : float → float
```

### Time

In the REPL, one can use `⏱` in place of `:bench`, i.e.

```
⏱ [(+)/x%ℝ(:x)]\`7 (𝒻 0 999 1000)
```

# Examples

## Cross Product

The cross product of two vectors $(a_1,a_2,a_3)$, $(b_1,b_2,b_3) \in \mathbb{R}^3$ is the vector $\in\mathbb{R}^3$

$$(a_2b_3-a_3b2, a_3b1-a_1b3,a_1b_2-a_2b_1)$$

In Apple, we can write:

```
λa.λb. (-)`((*)`(1⊖a) (_1⊖b)) ((*)`(_1⊖a) (1⊖b))
```

This uses zips and rotations (no indices are mentioned); it is a new perspective on the problem.

## Linear Regression

```
λxs.λys.
{
  Σ ← [(+)/x];
  n ⟜ ℝ(:xs);
  xbar ⟜ (Σ xs) % n; ybar ⟜ (Σ ys) % n;
  xy ⟜ Σ ((*)`xs ys);
  x2 ⟜ Σ ((^2)'xs);
  denom ⟜ (x2-n*(xbar^2));
  a ← ((ybar*x2)-(xbar*xy))%denom;
  b ← (xy-(n*xbar*ybar))%denom;
  (a,b)
}
```

Note the `⟜` to prevent expressions from being inlined.

## Kullback-Leibler Divergence

```
λp.λq. (+)/([x*_.(x%y)]`p q)
```

## Numerically Stable Geometric Mean

```
λxs.
  ⸎ avg ← [{n ⟜ ℝ(:xs); ((+)/xs)%n}]
  ; e:(avg (_.'xs))
```

## Array Strides

```
λds. }:((*)Λₒ 1::int ds)
```

(Column-major order)

## Train Neural Network

```
λwh.λwo.λbh.λbo.
{ X ⟜ ⟨⟨0,0⟩,⟨0,1⟩,⟨1,0⟩,⟨1,1⟩⟩;
  Y ⟜ ⟨0,1,1,0⟩;
  sigmoid ← [1%(1+ℯ(_x))];
  sDdx ← [x*(1-x)];
  sum ⇐ [(+)/x];
  ho ⟜ sigmoid`{0} ([(+)`bh x]'(X%.wh));
  prediction ⟜ (sigmoid ∴ (+bo))'(ho%:wo);
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
λp.λx. ~p⋅gen. 1 (*x) (𝓉p)
```

## Covariance Matrix

Given a $K \times N$ matrix of $N$ obervations on $K$ variables, we can compute
the sample covariance matrix thusly:

```
λxs.
{
  𝜇 ← [⸎n⟜ ℝ(:x); (+)/x%n]; rs ← 𝜇'xs;
  nd ⟜ [(-x)'y]`{0,1∘[2]} rs xs;
  N ⟜ ℝ(:({.xs))-1;
  nd [x⋅y%N]⊗ nd
}
```

The array style gives a new take on the problem.

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
λA.λx. (x⋅)`{1∘[2]} (A::Arr (i × j) float)
```

### Filter

```
\p.\xs. (xs˙)'p⩪xs
```

## [Argmax](https://numpy.org/doc/stable/reference/generated/numpy.argmax.html)

```
[{m⟜(⋉)/x::Vec n float; (=m)@.x}]
```

## [Luhn Check](https://en.wikipedia.org/wiki/Luhn_algorithm)

```
λxs.
  { digitSum ← [?x>10,.x-9,.x]
  ; t ← (+)/ [digitSum (x*y)]`(~(}:xs)) (}: (cyc. ⟨2,1::int⟩ 8))
  ; 10-t|10=}.xs
  }
```

Note zipping with `cyc. ⟨2,1::int⟩ 8` to get alternating 2, 1, ... factors.

## Combinatorics

### A000081

The number of unlabeled rooted trees with at most $n$ nodes (this [appears in
chemistry (counting alkanes)](https://www.emis.de/journals/JIS/cayley.html)).

```
λN.
{ sum ⇐ [(+)/ₒ 0 x]
; divisors ← λn. (λk. (n|k=0))§⍳ 1 n 1
; 𝓕 ⟨0,1::int⟩ (λas. {n⟜ :as; sum ((λj.sum ((λd. d*as˙d)'(divisors j))*as˙(n-j))'⍳ 1 (n-1) 1)/.(n-1)}) N
}
```

The use of "strong induction" provides a new take on the problem [where Python uses memoization and Haskell exploits sharing/laziness](https://oeis.org/A000081).

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
    ; c ⟜ T%(2*n^2*𝜋^2)
    ; cosDiffs ⟜ (-)\~([cos.(k*x)]'pts)
    ; sinDiffs ⟜ (-)\~([sin.(k*x)]'pts)
    ; aₙ ← c*sum ((*)`dxss cosDiffs)
    ; cₙ ← c*sum ((*)`dyss cosDiffs)
    ; bₙ ← c*sum ((*)`dxss sinDiffs)
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
  ; pns ← ⍳ 2 ni 1
  ; isPrime ← λn.¬((∨)/ₒ #f ([n|x=0]'⍳ 2 (⌊(√(ℝn))) 1)); pf ⇐ (isPrime #.)
  ; pps ⟜  (λk. (n|k=0)) #. pns
  ; ?ni^2=n
    ,.((*)/ₒ 1 (pf (pps⧺(n/.)'}:?pps)))
    ,.((*)/ₒ 1 (pf (n ⊲ pps⧺((n/.)'pps))))
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
(+)Λ (𝔯 _1 1 :: Vec 200 int)
```

```{.include}
nb/randomWalk.html
```

## Statistics

Apple is capable of statistical computing, via the program suggested by [Ewart Shaw](https://www.jsoftware.com/papers/jhyper.pdf).

### CDF for Normal Distribution

```
λz.
{
  erf ← λz.
    {
      ffact ← [(*)/ₒ 1 (𝒻 1 x (⌊x))];
      Σ ← λN.λa. (+)/ₒ 0 (a'(⍳ 0 N 1));
      (2%√𝜋)*Σ 30 (λn. {nf⟜ℝn; ((_1^n)*z^(2*n+1))%((ffact nf)*(2*nf+1))})
    };
  zz ⟜ z%(√2);
  0.5*(1+erf(zz))
}
```

### CDF for Student's t-distribution

```
λx.λν.
{
  gammaln ← λz. {
    zz ⟜ z-1;
    c0 ← 0.999999999999997092;
    𝛾 ← 607%128;
    coeffs ← ⟨ 57.1562356658629235
             , _59.5979603554754912
             , 14.1360979747417471
             , _0.491913816097620199
             , 0.339946499848118887e-4
             , 0.465236289270485756e-4
             , _0.983744753048795646e-4
             , 0.158088703224912494e-3
             , _0.210264441724104883e-3
             , 0.217439618115212643e-3
             , _0.164318106536763890e-3
             , 0.844182239838527433e-4
             , _0.261908384015814087e-4
             , 0.368991826595316234e-5
             ⟩;
    ss ← (+)/ ([y%(zz+itof x)]`(⍳ 1 14 1) coeffs);
    (((zz+0.5)*_.(zz+𝛾+0.5))-(zz+𝛾+0.5))+_.((√(2*𝜋))*(c0+ss))
  };
  Γ ⟜ [ℯ(gammaln x)];
  f21 ← λa0.λa1.λb.λz. {
    rf ← [(*)/ₒ 1 (𝒻 x (x+y-1) (⌊y))]; fact ← rf 1;
    Σ ← λN.λa. (+)/ₒ 0 (a'(⍳ 0 N 1));
    term ← λn. {nn⟜ℝ n; rf a0 nn*(rf a1 nn%rf b nn)*(z^n%fact nn)};
    Σ 50 term
  };
  0.5+x*Γ(0.5*(ν+1))%((√(𝜋*ν))*Γ(ν*0.5))*f21 0.5 ((ν+1)%2) 1.5 (_(x^2%ν))
}
```

This uses the [Lanczos approximation](https://mathworld.wolfram.com/LanczosApproximation.html) to
compute the gamma function; it is not built-in to Apple.

Note the `Γ ⟜ [ℯ(gammaln x)]`; this prevents the function from being inlined and
thence speeds compilation.

### CDF for F-distribution

```
λn.λm.λx.
{
  incΒ ← λz.λa.λb.
    {
      f21 ← λa0.λa1.λb.λz.
        {
          rf ← [(*)/ₒ 1 (𝒻 x (x+y-1) (⌊y))]; fact ← rf 1;
          Σ ← λN.λa. (+)/ₒ 0 (a'(⍳ 0 N 1));
          term ← λn. {nn⟜ℝ n; ((rf a0 nn)*(rf a1 nn)%(rf b nn))*((z^n)%(fact nn))};
          Σ 30 term
        };
      ((z**a)%a)*f21 a (1-b) (a+1) z
    };
  Β ← λx.λy.
    {
      gammaln ⟜ λz.
        {
          zz ⟜ z-1;
          c0 ← 0.999999999999997092;
          𝛾 ← 607%128;
          coeffs ← ⟨ 57.1562356658629235
                   , _59.5979603554754912
                   , 14.1360979747417471
                   , _0.491913816097620199
                   , 0.339946499848118887e-4
                   , 0.465236289270485756e-4
                   , _0.983744753048795646e-4
                   , 0.158088703224912494e-3
                   , _0.210264441724104883e-3
                   , 0.217439618115212643e-3
                   , _0.164318106536763890e-3
                   , 0.844182239838527433e-4
                   , _0.261908384015814087e-4
                   , 0.368991826595316234e-5
                   ⟩;
          ss ← (+)/ ([y%(zz+ℝ x)]`(⍳ 1 14 1) coeffs);
          (((zz+0.5)*_.(zz+𝛾+0.5))-(zz+𝛾+0.5))+_.((√(2*𝜋))*(c0+ss))
        };
      e:(gammaln x+gammaln y-gammaln (x+y))
    };
  I ← λz.λa.λb. incΒ z a b%Β a b;
  I ((n*x)%(m+n*x)) (n%2) (m%2)
}
```

Apple does not have imports so we have to re-type the definition of
the gamma function each time.
