- [x] refactor, avoid gigapolymorphism with freg2 and use aliases instead (works
  same on x86/arm) (at least by the assembly stage)
  - [ ] make sure f2 constants are lifted out of loops..
- [½] properly preserve volatile parts of registers across calls...
- [ ] make calls more efficient (all at once + give offsets)
- [ ] successive difference simd
- [½] constant lift-out-of-loop (SIMD)
