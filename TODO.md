- [ ] Figure out log/exp... eh
- [ ] inspiration: J,Haskell,C?,Remora
- [ ] windows lol
- [ ] apple (array system)
- [ ] serialize (save on disk) REPL states
- [ ] bidirectional type inference (rank-polymorphism aka dimension functor)
- [ ] documentation generated from types
- [ ] "array server" architecture like J? (figure out refcounting, copy-on-write -> efficient polymorphism/(static) reuse analysis?)
  - [ ] example `(2&*) "0`
- [ ] idioms... deforestation
- [ ] types... (linear? remora; integer-indexed)
- [x] `.🍎` file extension (`.🍏`)
  - [ ] ⍳ (apl iota)
  - [ ] remora-like type system
  - [ ] ⩪ for filter
  - [ ] ℘ ⊲ ⊳ ⪫ ⪪
  - [ ] script f https://en.wikipedia.org/wiki/Mathematical_Alphanumeric_Symbols#Latin_letters
  - [ ] https://en.wikipedia.org/wiki/Mathematical_operators_and_symbols_in_Unicode
  - [ ] dfns like k, APL (J)
- [ ] big three: map, reduce, zip (dyadic map)
  - [ ] unfold
  - [ ] map is naturally functorial, zip then is a bifunctor (etc.) n-functor
    over dimension
    - [ ] our 'map' is a family of functors... arrays being functorial over
      cells
    - [ ] mapMaybe hm
    - [x] problem: filter (#) (existential types... PITA?)
    EXAMPLE: (*2)"0
    filt. -> Vec i Bool -> Vec i Int -> ∃n. Vec n Int

- [ ] numpy einstein summation
  - [ ] https://ajcr.net/Basic-guide-to-einsum/
- [ ] documentation from types
  - [ ] quickcheck!
  - [ ] automatic differentiation (pytorch eh)
- [ ] deforestation
- [ ] Note: !-modality is functorial, so we get some polymorphism that way?
# Features
- [ ] `zipWith` builtin (2-ary)
- [ ] allow type signatures in lambdas?
- [ ] mko executable - compile expression into .o file, with some name
- [ ] random number generation
- [ ] lift constants out of loops (precompute)
- [ ] tuples idk.
- [ ] reshape arrays...
- [ ] clz? (count leading zeroes = floor(log) -> digits)
## Syntax
- [ ] https://en.wikipedia.org/wiki/Mathematical_operators_and_symbols_in_Unicode
- [ ] https://www.compart.com/en/unicode/U+1D66
## Optimization
- [ ] `neg` instruction, not `0-`...
- [ ] Back/forward loops (compare to 0 or whatever)
- [ ] Break dependency chains: use e.g. four accumulators per loop cycle when
  summing float array (see agner fog)
# Performance
- [ ] `-O2` perhaps (investigate with further pipeline)
- [ ] Modify state (+1) instead of using lazy list to supply e.g. temps
- [ ] Live intervals/linear allocator is stupid as shit
  - [ ] need to do backwards/forwards thing and stitch it up at basic block
    boundaries
- [ ] entropy: vfmadd231sd could take address directly as argument!
# Modules
- [x] Assembler
- [x] linear register allocator
- [ ] deforestation
# Bugs
- [ ] `gammaln` generates `vaddsd rsp, xmm3, xmm1` lol
- [ ] Pass over to ensure everything is monomorphized
- [ ] `itof (:xs)` - would prefer w/o parens?
- [ ] it would be nice to write `_x%y` instead of `(_x)%y` (parse precedence)
- [ ] x+y-1 parsed as (x + (y - 1))
## Type system
- [ ] Check that bindings are not too polymorphic
- [ ] `LLet` cannot contain functions (lol)
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
- [ ] matrix multiplication
  - [ ] rearrange: note that I implicitly coerce
  `Arr (i `Cons` Nil) (Arr (j `Cons` Nil) a)` into (Arr (i `Cons` j `Cons` Nil) a)
  which I guess needs a function (annoying?)
  - [ ] my `map` is too underpowered I think... compared to true rank (remora
    paper?)
    - [ ] could have a matmul builtin lol
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
- [ ] convolution (image processing)
- [ ] http://www.paulbourke.net/fractals/burnship/
- [ ] kaplan-meier, clopper-pearson?
- [ ] https://forem.julialang.org/inphyt/ann-juliaepi-collaborative-computational-epidemiology-in-julia-19ng
