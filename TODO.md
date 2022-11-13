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
  - [ ] dfns like k, APL (J)
- [ ] mapMaybe hm
- [ ] numpy einstein summation
  - [ ] https://ajcr.net/Basic-guide-to-einsum/
- [ ] documentation from types
  - [ ] quickcheck!
  - [ ] automatic differentiation (pytorch eh)
# Features
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
## Syntax
- [ ] more laconic syntax for type signatures (no Arr... Cons)
- [x] `zipWith` builtin (2-ary)
  - [x] rename ` because it's only used as `2 1 anyway
- [ ] https://en.wikipedia.org/wiki/Mathematical_operators_and_symbols_in_Unicode
- [ ] https://www.compart.com/en/unicode/U+1D66
## Optimization
- [ ] `neg` instruction, not `0-`...
- [ ] Back/forward loops (compare to 0 or whatever)
- [ ] Break dependency chains: use e.g. four accumulators per loop cycle when
  summing float array (see agner fog)
# Performance
- [ ] Modify state (+1) instead of using lazy list to supply e.g. temps
# Bugs
- [ ] beta-reduction with 'rand' or w/e (needs to be llet ... in)
- [ ]
```
{sig<-[1%(1+(e:(_x)))];sig'1 (frange _6 6 12)}
```
- [ ] Pass over to ensure everything is monomorphized
- [ ] `itof (:xs)` - would prefer w/o parens?
- [ ] it would be nice to write `_x%y` instead of `(_x)%y` (parse precedence)
- [ ] `((2 ^ (3 * 1)) + 2` (parses `2^3*1+2` wrong)
- [ ] `(+)/1 0 ((_.'1) frange 2 6 5)`
- [ ] match doesn't check constraints on annotations
```
vanessa@vanessa-desktop /development/haskell/apple 🌸 arepl
 > ((+1)'1) (frange 1 5 5)
Arr (5) [1.0, 2.0, 3.0, 4.0, 5.0]
 > ((+1)'1) (frange 1 5 5)
Arr (5) [2.0, 3.0, 4.0, 5.0, 6.0]
 > ((+1)'1) (frange 1 5 5)
Arr (5) [2.0, 3.0, 4.0, 5.0, 6.0]
```
## Type system
- [ ] Check that bindings are not too polymorphic
- [ ] `LLet` should not contain functions, add a pass to check
- [ ] print constraints
- [ ] indexing with tuples (3-tuple for rank 3 array...)
# Checks/Passes
- [ ] Warn if irange or frange will exceed?
- [ ] Sanity check pass to make sure xmm0 doesn't end up target of `movtemp` etc.
# Examples
- [ ] median
- [ ] https://optimized-einsum.readthedocs.io/en/stable/
- [ ] polynomial evaluation
- [ ] modulo
- [ ] http://blog.vmchale.com/article/numba-why
- [ ] https://mathworld.wolfram.com/MotzkinNumber.html
- [ ] perceptual hash
- [ ] elliptic fourier series
  - [ ] http://www.sci.utah.edu/~gerig/CS7960-S2010/handouts/Kuhl-Giardina-CGIP1982.pdf
- [ ] Pascal's triangle
- [ ] FFT
- [ ] generating functions
- [ ] continued fractions
- [ ] `+//. y` in J... maybe `/.` takes `∀n. (Arr (n `Cons` Nil)) -> ...`
- [ ] https://www.labri.fr/perso/nrougier/from-python-to-numpy/
- [ ] neural net!
- [ ] think: inner/outer product, wedge products (?)
  - [ ] permutations/indices (determinant...)
- [ ] https://en.wikipedia.org/wiki/Arithmetic–geometric_mean#Complete_elliptic_integral_K(sinα)
- [ ] https://github.com/justin2004/image-processing#image-processing-with-apl
- [ ] http://shvbsle.in/computers-are-fast-but-you-dont-know-it-p1/
- [ ] Python FFI: modify a numpy array or something; regression->matplotlib?
- [ ] SciPy t-test
- [ ] discrete cosine transformation
- [ ] full hypergeometric (analytically extended?)
- [ ] https://www.shamusyoung.com/twentysidedtale/?p=11874
- [ ] ANOVA
- [ ] http://www.paulbourke.net/fractals/burnship/
- [ ] kaplan-meier, clopper-pearson?
- [ ] https://forem.julialang.org/inphyt/ann-juliaepi-collaborative-computational-epidemiology-in-julia-19ng
- [ ] https://michaelmoroz.github.io/TracingGeodesics/
- [ ] https://palaiologos.rocks/posts/linalg-apl/
- [ ] FFI https://code.jsoftware.com/wiki/Guides/DLLs/Calling_DLLs
- [ ] https://code.jsoftware.com/wiki/Essays
- [ ] j901 integration:
  - [ ] `viewmat 100 100 $ 1 2 1` `viewmat */~i:5` `viewmat +/~i.10`
- [ ] https://www.cygnus-software.com/downloads/downloads.htm
