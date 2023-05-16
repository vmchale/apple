# Fix IR-gen step
## Broken/Core Features
- [½] map
- [½] fold
- [½] rank
- [ ] convolve
- [x] transpose
- [x] concat
- [x] array literals
  - [ ] multidimensional arrays
- [x] matmul
## Add Features
- [x] outer product
- [ ] reverse
- [ ] `reshape`
- [◦] random
- [ ] at/index of
# Performance
- [ ] lift constants out of loops
- [ ] lift `fninit` out of loops
- [ ] liveness analysis on basic blocks
# Allocations
- [ ] consolidate allocations up-front when possible (dim from types)
# ABI
- [ ] Tuples (stack-allocate) (induction)
  - [ ] cos/sin crash against the stack (refer to rsp+8, rsp+16?)
  - [ ] stack-allocations: ensure 16-byte??
- [ ] arrays of tuples
# Register Allocation
- [x] Finish graph-based allocator
  - [x] analyze clobbers in `call`s
  - [?] spill
