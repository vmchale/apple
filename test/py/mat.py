import numpy as np
N=1024
bs=np.random.rand(N,N)

import apple
mul=apple.jit(f"[(x::Arr ({N}×{N}) float)%.(y::Arr ({N}×{N}) float)]")
assert (bs@bs==mul(bs,bs)).all()
