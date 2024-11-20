- [ ] fold-of-seed on scalar?
- [ ] fold-of-array
- [ ] zip of array
- [ ] fold-of-zip on array
- [x] `di.` builtin
- [ ] scan-of-array
```
 > \xs.\ys.[(+)`x y]Λ xs
λxs. (λys. ((λx. (λy. ((+) ` x y))) Λ xs)) : ( IsNum c ) :=> Arr (i + 1 `Cons` sh) (Vec i c) → a → Arr (i + 1 `Cons` sh) (Vec i c)
 > \xs.\ys.[(+)`x y]Λ (xs::M int)
1:9: could not unify 'int' with 'Vec i c' in expression 'xs :: Arr (i × j) int'
```
- [ ] rank for scalars...
  - [x] : 0.0 (size)
- [ ] scan-of-tup
- [ ] fold-tup
- [x] snoc array, cons array
- [ ] fold, producing tuple
- [ ] filter-on-array
- [x] don't crash when stack-allocated tuples, arrays containing bools
- [x] equality on arrays
  - [ ] equality on tup
- [ ] non-inlined functions
- [ ] various things only compile when rank is known...
- [ ] more rank cases...
# Types
- [ ] `hasbits` instance for boolean arrays?
