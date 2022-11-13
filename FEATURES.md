# Fix IR-gen step
## Broken/Core Features
- [ ] map(-n)
- [ ] fold(-n)
- [ ] rank
- [ ] convolve
- [ ] transpose
- [ ] concat
- [x] array literals
  - [ ] multidimensional arrays
## Add Features
- [ ] outer product
- [ ] dyadic infix
- [ ] successive application
- [ ] matmul
# Performance
- [ ] lift constants out of loops
- [ ] lift `fninit` out of loops
- [ ] liveness analysis on basic blocks
# Allocations
- [ ] consolidate allocations up-front when possible (dim from types)
# ABI
- [ ] Tuples (stack-allocate) (induction)
# Register Allocation
- [x] Finish graph-based allocator
  - [ ] analyze clobbers in `call`s
  - [ ] spill
