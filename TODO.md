- [ ] windows lol
- [ ] serialize (save on disk) REPL states
- [ ] documentation generated from types
- [ ] idioms... deforestation
- [x] `.🍎` file extension (`.🍏`)
  - [x] ⍳ (apl iota)
  - [x] ⩪ for filter
  - [x] § is fun I love typographers
  - [ ] ℘
    - [ ] span/break?
  - [½] ⊲ ⊳ ⪫ ⪪
  - [ ]〔〖【
  - [ ] ⫛ for J's oblique
  - [x] ⸎
  - [ ] ‰
  - [ ] https://en.wikipedia.org/wiki/Guillemet#Encoding
  - [ ] https://en.wikipedia.org/wiki/Prime_(symbol)#Computer_encodings
  - [ ] script f https://en.wikipedia.org/wiki/Mathematical_Alphanumeric_Symbols#Latin_letters
  - [ ] https://www.compart.com/en/unicode/block/U+2A00
  - [ ] https://www.compart.com/en/unicode/block/U+2440
  - [x] dfns like k, APL (J)
- [ ] mapMaybe hm
- [ ] numpy einstein summation
  - [ ] https://ajcr.net/Basic-guide-to-einsum/
- [ ] documentation from types
  - [ ] quickcheck!
  - [ ] automatic differentiation (pytorch eh)
# Features
- [ ] https://hackage.haskell.org/package/containers-0.7/docs/Data-Sequence.html#v:cycleTaking (cycle til')
- [ ] Use `arc4random_buf` and also `arc4random_uniform`, `random`, `drand48`
- [ ] special case for `𝔯 0 1 :: float` etc.
- [ ] `𝔯 0 1 :: Arr sh int` special case, apply bitmask over array?
- [x] folds/scans shouldn't take seeds
- [ ] allow type signatures in lambdas?
- [x] mko executable - compile expression into .o file, with some name
- [ ] random number generation
- [x] lift constants out of loops (precompute)
- [x] tuples idk.
  - [ ] float tuple return
- [ ] reshape arrays
- [ ] clz? (count leading zeroes = floor(log) -> digits)
- [ ] flatness check (arrays)
- [ ] generalize "diagonal"?
- [ ] fold-along-diagonal for poly mult. https://code.jsoftware.com/wiki/Vocabulary/slashdot
## Syntax
- [ ] · for dot product?
- [ ] https://en.wiktionary.org/wiki/Appendix:APL
- [x] `Mᵢⱼ` should parse as `Arr (i `Cons` j `Cons` Nil)` maybe? (subscript
  unicode block)
- [x] more laconic syntax for type signatures (no Arr... Cons) (`Vec`) (`M` for matrix)
- [x] `zipWith` builtin (2-ary)
  - [x] rename ` because it's only used as `2 1 anyway
- [ ] https://en.wikipedia.org/wiki/Mathematical_operators_and_symbols_in_Unicode
- [ ] https://en.wikipedia.org/wiki/Arabic_script_in_Unicode#Punctuation_and_ornaments
- [ ] https://commons.wikimedia.org/wiki/Unicode_circle_shaped_symbols
  - [ ] ☉
- [ ] https://www.compart.com/en/unicode/U+1D66
  - [ ] Ϟ (koppa), Ϡ (sampi)
## Optimization
- [ ] arrays in assembler: register indirection?
```
T13 = A_0
T16 = T13.dim[0]
```
- [ ] Break dependency chains: use e.g. four accumulators per loop cycle when
  summing float array (see agner fog)
# Performance
- [ ] consolidate move-zero for floats and ints
  ```
  eor x5, x5, x5                           a5 00 05 ca
  fmov d2, x5                              a2 00 67 9e
  eor x5, x5, x5                           a5 00 05 ca
  ```
- [ ] think of a better way to handle functions of tuples (internally)
  - [ ] `πe` which places in registers?
- [ ] map-of-gen. idiom
- [ ] bitmask immediates for `and` on aarch64
- [ ] Use `Word64` for sets of registers
- [x] Modify state (+1) instead of using lazy list to supply e.g. temps
# Bugs
- [ ]
```
 > irange 10 0 _1
Vec 9 [10, 9, 8, 7, 6, 5, 4, 3, 2]
```
```
 > ⟨(1.0,2.0,3.0),(3,4,5)⟩˙0
(1.0, 0.0, 3.0)
 > ⟨(1.0,2.0,3.0,3.0),(6.0,3,4,5)⟩˙0
(1.0, 2.0, 3.0, 3.0)
 > ⟨(1.0,2.0,3.0,3.0,3),(6.0,3,4,5,4.0)⟩˙0
zsh: segmentation fault  cabal run arepl
```
```
 > \xs.\rs. [(-y)'x]`{1∘[2],0} xs (rs::Vec 2 float)
λxs. (λrs. ((λx. (λy. (λx. x - y) ' x)) `{1∘[2],0} xs) rs)
    : Arr (2 × j) float → Vec 2 float → Arr (sh ⧺ j) float
 > \xs.\rs. [(-x)'y]`{0,1∘[2]} (rs::Vec 2 float) xs
λxs. (λrs. ((λx. (λy. (λx. x - x) ' y)) `{0,1∘[2]} rs) xs)
    : Arr (2 × j) float → Vec 2 float → Arr (2 × j) float
```
- [ ]
```
 > :yank ix test/data/ixGen.🍏
 > :ix ⟨1.0,3,4⟩
zsh: illegal hardware instruction  cabal run arepl
```
- [ ] (rand. 0 1::Arr (2×10) int)
- [ ] `irange 0 0 0` crashes
- [x]  `}:? ((<0)#.irange 0 4 1)` segfaults
- [ ] :cmm \xs. [⟨x->1,x->2⟩]'(xs::Vec n (float, float))
- [ ] {. ⟨⟨1,1.0⟩,⟨2,3⟩⟩ type
- [ ] segfault when `aso` is called pre-register allocation (arm)
- [ ] Should display constraints
```
 > :ty (+)
a → a → a
 > :ty (⋉)
o → o → o
```
- [ ]  `> (𝔯 _10 10) :: int 26`
- [ ] `:asm [x(%.)(y::Arr(i`Cons`j`Cons`Nil)float)]` type inference??
- [ ] `xmm0` and `xmm1` incorrectly marked as clobbered when return value is not
  actually in `xmm0`/`xmm1` or whatever
- [ ] `fsin` instruction requires reduction module 2pi or w/e
- [ ] beta-reduction with 'rand' or w/e (needs to be llet ... in)
- [ ] Pass over to ensure everything is monomorphized
- [ ] `itof (:xs)` - would prefer w/o parens?
- [x] it would be nice to write `_x%y` instead of `(_x)%y` (parse precedence)
- [ ] match doesn't check constraints on annotations
- [ ] check in assembler phase for labels not being duplicate
## Type system
- [ ] diagonal on higher-rank?
- [x] Check that bindings are not too polymorphic
  - [ ] after inlining
  - [x] add a pass to prevent arrays of tuples of arrays
- [ ] indexing with tuples (3-tuple for rank 3 array...)
# Checks/Passes
- [ ] Warn if irange or frange will exceed?
# Examples
- [ ] color!
  - [ ] https://en.wikipedia.org/wiki/YUV
- [ ] https://optimized-einsum.readthedocs.io/en/stable/
- [x] polynomial evaluation
- [ ] https://mathworld.wolfram.com/MotzkinNumber.html
- [ ] perceptual hash
  - [ ] median lol (indexing?)
- [ ] Pascal's triangle
- [ ] FFT
- [ ] generating functions
- [x] continued fractions
- [ ] `+//. y` in J... maybe `/.` takes `∀n. (Arr (n `Cons` Nil)) -> ...`
- [ ] https://www.labri.fr/perso/nrougier/from-python-to-numpy/
- [ ] neural net!
- [ ] think: inner/outer product, wedge products (?)
  - [ ] permutations/indices (determinant...)
  - [ ] discrete cosine transformation (gen2.)
- [ ] https://en.wikipedia.org/wiki/Arithmetic–geometric_mean#Complete_elliptic_integral_K(sinα)
- [ ] https://github.com/justin2004/image-processing#image-processing-with-apl
- [ ] http://shvbsle.in/computers-are-fast-but-you-dont-know-it-p1/
- [ ] SciPy t-test
- [ ] full hypergeometric (analytically extended?)
- [ ] https://www.shamusyoung.com/twentysidedtale/?p=11874
- [ ] ANOVA
  - [x] F-distribution CDF
- [ ] http://www.paulbourke.net/fractals/burnship/
- [ ] kaplan-meier, clopper-pearson?
- [ ] https://forem.julialang.org/inphyt/ann-juliaepi-collaborative-computational-epidemiology-in-julia-19ng
- [ ] https://michaelmoroz.github.io/TracingGeodesics/
- [ ] https://palaiologos.rocks/posts/linalg-apl/
- [ ] FFI https://code.jsoftware.com/wiki/Guides/DLLs/Calling_DLLs
- [ ] https://code.jsoftware.com/wiki/Essays
  - [ ] https://oeis.org/A006257
- [ ] J integration:
  - [ ] `viewmat 100 100 $ 1 2 1` `viewmat */~i:5` `viewmat +/~i.10`
- [ ] https://www.cygnus-software.com/downloads/downloads.htm
- [ ] https://laustep.github.io/stlahblog/posts/OnAMobiusTransformation.html
- [ ] https://laustep.github.io/stlahblog/posts/beautifulComplexFunctions.html
- [ ] https://hackage.haskell.org/package/weierstrass-functions-0.1.0.0
- [ ] n-body (benchmarks)
- [ ] https://rhodesmill.org/skyfield/
- [ ] https://aakinshin.net/posts/r-hodges-lehmann-problems/
- [ ] orbital densities!
- [ ] http://psa.es/sdg/sunpos.htm
- [ ] https://stat.ethz.ch/R-manual/R-devel/library/stats/html/00Index.html
  - [ ] https://stat.ethz.ch/R-manual/R-devel/library/stats/html/Tukey.html
- [ ] https://github.com/profConradi/Python_Simulations/blob/599e7c66903166c1e5997318878a6db6f1aaa3d8/Nice_orbits.ipynb
- [ ] http://falstad.com/mathphysics.html
- [ ] https://mathstodon.xyz/@bitartbot@botsin.space/111992137516554370
- [ ] https://math.ucr.edu/home/baez/roots/
- [ ] http://xahlee.info/math/algorithmic_math_art.html
- [ ] https://rosalind.info/problems/locations/
- [ ] https://en.wikipedia.org/wiki/Newton_fractal
- [ ] https://en.wikipedia.org/wiki/Table_of_spherical_harmonics
- [ ] https://prng.di.unimi.it/splitmix64.c
- [ ] https://github.com/tonio-m/python_neural_network/blob/main/main.py
- [ ] https://github.com/profConradi?tab=repositories
- [ ] factors : int -> [(int, int)]
- [ ] https://www.infinitepartitions.com/art001.html
- [ ] https://en.wikipedia.org/wiki/Bhattacharyya_distance
- [ ] better mnist
  - [ ] https://github.com/eliot-tron/curvcomputenn/blob/89410f64030e8b07434c30f7752b163df6e97f28/mnist_networks.py#L5
    - [ ] torch's Linear, Conv2d, Affine, ReLU, MaxPool2d
- [ ] multivariate adaptive regression spline
- [ ] https://github.com/rougier/scientific-visualization-book
- [ ] https://www.2dcurves.com/derived/caustic.html
  - [ ] https://mathworld.wolfram.com/Hippopede.html
- [ ] https://en.wikipedia.org/wiki/Mahalanobis_distance
- [ ] https://cran.r-project.org/web/packages/indicspecies/vignettes/IndicatorSpeciesAnalysis.html
- [ ] cellular automata
  - [ ] https://en.wikipedia.org/wiki/Rule_110
  - [ ] https://en.wikipedia.org/wiki/Rule_30
- [ ] https://www.bathsheba.com/math/gyroid/
- [x] https://oeis.org/A000081
- [ ] https://code.jsoftware.com/wiki/Essays
- [ ] https://en.wikipedia.org/wiki/Chebyshev_polynomials
- [ ] https://cran.r-project.org/web/packages/glmm/index.html
- [ ] https://www.jsoftware.com/papers/50/
  - [ ] APL "index-of" dyad: array of indices where item in right appears in
    left
- [ ] https://aplwiki.com/wiki/FinnAPL_idiom_library
- [ ] https://rosettacode.org/wiki/Cyclotomic_polynomial#Haskell
  - [ ] synthetic division alone would be good...
```haskell
shortDiv :: [Integer] -> [Integer] -> [Integer]
shortDiv p1 (_:p2) = unfoldr go (length p1 - length p2, p1)
  where
    go (0, _) = Nothing
    go (i, h:t) = Just (h, (i-1, zipWith (+) (map (h *) ker) t))
    ker = negate <$> p2 ++ repeat 0
```
# Project Euler
- [ ] https://projecteuler.net/problem=30
# Rosetta Code
- [ ] https://rosettacode.org/wiki/Averages/Pythagorean_means
# Debug
- [ ] sanity check negative dims
