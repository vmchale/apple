# Fix IR-gen step
## Broken/Core Features
- [x] map
- [½] fold
- [x] rank
- [x] transpose
- [x] concat
- [x] array literals
  - [ ] multidimensional arrays
- [x] matmul
- [x] convolve
## Add Features
- [x] outer product
- [½] reverse
- [ ] `reshape`
- [½] random
- [x] at/index of
# Performance
- [x] lift constants out of loops
- [ ] lift `fninit` out of loops
- [x] liveness analysis on basic blocks
  - [ ] liveness analysis on basic blocks (IR)
# Allocations
- [ ] consolidate allocations up-front when possible (dim from types)
# ABI
- [x] Tuples (stack-allocate) (induction)
  - [x] cos/sin crash against the stack (refer to rsp+8, rsp+16?)
  - [x] stack-allocations: ensure 16-byte??
- [x] arrays of tuples
# Register Allocation
- [x] Finish graph-based allocator
  - [x] analyze clobbers in `call`s
  - [?] spill
