- [ ] fold-of-seed on scalar?
- [ ] fold
  - [ ] array
    - [ ] "cat" "dim-reduce-1" is ((++)/)
- [ ] fold-gen
  - [ ] Π
  - [ ] arr
- [ ] take, drop
  - [ ] A
- [ ] zip
  - [ ] A A A
  - [ ] A S A
  - [ ] A S S
  - [ ] S A S
- [ ] "at"
  - [ ] array
- [ ] succ
  - [ ] array
- [ ] fold-of-zip
  - [ ] array
- [ ] outer product
  - [ ] A A A
  - [x] S S A
  - [x] A A S
  - [ ] A S S
  - [ ] S A S
  - [x] A S A
  - [ ] S A A
  - [x] S S S
- [ ] filter
  - [ ] array
- [ ] scan
  - [ ] array
```
 > \xs.\ys.[(+)`x y]Λ xs
λxs. (λys. ((λx. (λy. ((+) ` x y))) Λ xs)) : ( IsNum c ) :=> Arr (i + 1 `Cons` sh) (Vec i c) → a → Arr (i + 1 `Cons` sh) (Vec i c)
 > \xs.\ys.[(+)`x y]Λ (xs::M int)
1:9: could not unify 'int' with 'Vec i c' in expression 'xs :: Arr (i × j) int'
```
- [ ] rank for scalars...
- [x] don't crash when stack-allocated tuples, arrays containing bools
- [x] equality on arrays
  - [ ] equality on tup
- [ ] non-inlined functions
- [ ] various things only compile when rank is known...
# Types
- [ ] `hasbits` instance for boolean arrays?
