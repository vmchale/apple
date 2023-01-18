# Fix IR-gen step
## Broken/Core Features
- [ ] map(-n)
- [ ] fold
- [ ] rank
- [ ] convolve
- [◦] transpose
- [x] concat
- [x] array literals
  - [ ] multidimensional arrays
## Add Features
- [ ] outer product
- [ ] matmul
- [ ] reverse
- [ ] `reshape`
# Performance
- [ ] lift constants out of loops
- [ ] lift `fninit` out of loops
- [ ] liveness analysis on basic blocks
# Allocations
- [ ] consolidate allocations up-front when possible (dim from types)
# ABI
- [ ] Tuples (stack-allocate) (induction)
  - [ ] cos/sin crash against the stack (refer to rsp+8, rsp+16?)
# Register Allocation
- [x] Finish graph-based allocator
  - [x] analyze clobbers in `call`s
  - [?] spill
