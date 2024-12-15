import numpy as np;import apple

from numpy.testing import assert_array_equal,assert_allclose

np.random.seed(17)

def vs(M,N):
    A=np.random.rand(M,N);x=np.random.rand(N)
    v=apple.jit(f"[(x::Arr ({M}×{N}) 𝞈)%:y]")
    assert_allclose(v(A,x),A@x,rtol=1e-15,strict=True)

vs(100,32)
def test(M,N,K):
    bs=np.random.rand(M,N);cs=np.random.rand(N,K)
    msz=apple.jit(f"[(x::Arr ({M}×{N}) 𝞈)%.(y::Arr ({N}×{K}) 𝞈)]")
    assert_allclose(msz(bs,cs),bs@cs,rtol=1e-15,strict=True)

    m=apple.jit(f"[(x::M 𝞈)%.y]")
    assert_allclose(m(bs,cs),bs@cs,rtol=1e-15,strict=True)

    ds=np.random.rand(K,N)
    mT=apple.jit(f"[(x::Arr ({M}×{N}) 𝞈)%.|:(y::Arr ({K}×{N}) 𝞈)]")
    assert_allclose(mT(bs,ds),bs@ds.T,rtol=1e-14,strict=True)

test(4,512,8);test(2,100,16)
test(10000,784,128)
test(64,5,64); test(100,2,15)
test(257,32,32);test(3,3,3)
