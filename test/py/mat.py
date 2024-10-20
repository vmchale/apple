import numpy as np
bs=np.random.rand(1024,1024)

import apple
mul=apple.jit("[(x::M ₁₀₂₄,₁₀₂₄ float)%.(y::M ₁₀₂₄,₁₀₂₄ float)]")
assert (bs@bs==mul(bs,bs)).all()
