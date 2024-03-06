- [ ] windows lol
- [ ] serialize (save on disk) REPL states
- [ ] documentation generated from types
- [ ] idioms... deforestation
- [x] `.🍎` file extension (`.🍏`)
  - [ ] ⍳ (apl iota)
  - [ ] ⩪ for filter
  - [ ] ℘ ⊲ ⊳ ⪫ ⪪
  - [ ] https://en.wikipedia.org/wiki/Guillemet#Encoding
  - [ ] https://en.wikipedia.org/wiki/Prime_(symbol)#Computer_encodings
  - [ ] script f https://en.wikipedia.org/wiki/Mathematical_Alphanumeric_Symbols#Latin_letters
  - [ ] https://en.wikipedia.org/wiki/Mathematical_operators_and_symbols_in_Unicode
  - [x] dfns like k, APL (J)
- [ ] mapMaybe hm
- [ ] numpy einstein summation
  - [ ] https://ajcr.net/Basic-guide-to-einsum/
- [ ] documentation from types
  - [ ] quickcheck!
  - [ ] automatic differentiation (pytorch eh)
# Features
- [ ] special case for `𝔯 0 1 :: float` etc.
- [ ] folds/scans shouldn't take seeds
- [ ] allow type signatures in lambdas?
- [ ] mko executable - compile expression into .o file, with some name
- [ ] random number generation
- [ ] lift constants out of loops (precompute)
- [x] tuples idk.
  - [ ] float tuple return
- [ ] reshape arrays
- [ ] clz? (count leading zeroes = floor(log) -> digits)
- [ ] flatness check (arrays)
- [ ] generalize "diagonal"?
- [ ] fold-along-diagonal for poly mult. https://code.jsoftware.com/wiki/Vocabulary/slashdot
## Syntax
- [ ] https://en.wiktionary.org/wiki/Appendix:APL
- [ ] `Mᵢⱼ` should parse as `Arr (i `Cons` j `Cons` Nil)` maybe? (subscript
  unicode block)
- [ ] more laconic syntax for type signatures (no Arr... Cons) (`Vec`) (`M` for matrix)
- [x] `zipWith` builtin (2-ary)
  - [x] rename ` because it's only used as `2 1 anyway
- [ ] https://en.wikipedia.org/wiki/Mathematical_operators_and_symbols_in_Unicode
- [ ] https://commons.wikimedia.org/wiki/Unicode_circle_shaped_symbols
- [ ] https://www.compart.com/en/unicode/U+1D66
## Optimization
- [ ] arrays in assembler: register indirection?
```
T13 = A_0
T16 = T13.dim[0]
```
- [ ] `neg` instruction, not `0-`...
- [x] Back/forward loops (compare to 0 or whatever)
- [ ] Break dependency chains: use e.g. four accumulators per loop cycle when
  summing float array (see agner fog)
# Performance
- [ ] Use `Word64` for sets of registers
- [ ] Modify state (+1) instead of using lazy list to supply e.g. temps
# Bugs
- [ ]
```
(λx. 1 ⊲ x ') : Arr (i `Cons` i `Cons` Nil) a → Arr (i `Cons` i + 1 `Cons` Nil) a
 > ([1.0<|x]')
(λx. 1.0 ⊲ x ') : Arr (i `Cons` i `Cons` Nil) float → Arr (i `Cons` i + 1 `Cons` Nil) float
 > ((1.0<|)')
1:1: could not unify 'float' with 'Arr (i `Cons` Nil) a' in expression '1.0'
```
- [ ] embarassing!
```
[|:(x::Arr(i`Cons`j`Cons`Nil)float)] ⟨⟨1::int,2⟩,⟨3,4⟩,⟨5,6⟩⟩
```
- [ ] Should display constraints
```
 > :ty (+)
a → a → a
 > :ty (⋉)
o → o → o
```
- [ ]  `> (𝔯 _10 10) :: int 26`
- [ ] `> ⟨⟨2,1,1⟩,⟨5,4,1⟩⟩%.⟨⟨2,0⟩,⟨2,0⟩,⟨7,3::float⟩⟩ -> Arr (2×2) [6.0, 10.0, 12.0, 31.0]`
- [ ] `:asm [x(%.)(y::Arr(i`Cons`j`Cons`Nil)float)]` type inference??
- [ ] `xmm0` and `xmm1` incorrectly marked as clobbered when return value is not
  actually in `xmm0`/`xmm1` or whatever
- [ ] `fsin` instruction requires reduction module 2pi or w/e
- [ ] ` |: ⟨⟨1.0,2⟩,⟨3,4⟩,⟨5,6.0⟩⟩`
- [ ] beta-reduction with 'rand' or w/e (needs to be llet ... in)
- [ ]
```
{sig<-[1%(1+(e:(_x)))];sig'1 (frange _6 6 12)}
```
- [ ] Pass over to ensure everything is monomorphized
- [ ] `itof (:xs)` - would prefer w/o parens?
- [ ] it would be nice to write `_x%y` instead of `(_x)%y` (parse precedence)
- [ ] `(+)/1 0 ((_.'1) frange 2 6 5)`
- [ ] match doesn't check constraints on annotations
- [ ] check in assembler phase for labels not being duplicate
## Type system
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
- [ ] polynomial evaluation
- [ ] https://mathworld.wolfram.com/MotzkinNumber.html
- [ ] perceptual hash
  - [ ] median lol (indexing?)
- [ ] Pascal's triangle
- [ ] FFT
- [ ] generating functions
- [ ] continued fractions
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
- [ ] numpy meshgrid
- [ ] http://falstad.com/mathphysics.html
- [ ] https://mathstodon.xyz/@bitartbot@botsin.space/111992137516554370
- [ ] https://math.ucr.edu/home/baez/roots/
