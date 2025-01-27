- [x] tuples in C's FnTy
- [½] slotted/efficient array allocations
- [ ] Abstract function intermediates
  - [x] better tuple-iteration
  - [ ] write-to-destination without allocation for arrays
    (separate `fill` function)
      - [ ] also when passing array args, dim &c. in an `AD` structure
- [ ] lift integer constants out of loop
- [x] worklist for use-defs (p. 402 appel)
- [ ] loop tiling/blocking for re-rank (and map etc.)
- [ ] efficient index-associated-with-loop: specify "at" addressed bound to
  for-loop, which is turned into something w/ strength reduction, post-index
    offset
    - [ ] see index in debug, no need for strength reduction...
- [ ] DNA type
- [ ] C type
- [ ] Einstein notation
- [ ] specify iteration by wallpaper group...
- [ ] simd exp by polynomial...?
- [ ] assembly "prettyprinter" class
- [ ] map, zip, outer as rank?
- [ ] less ignominious sort
  - [ ] no padding?
  - [ ] don't copy in/out from slab
- [ ] typed scatter... input matrix of indices = outputs matrix?
