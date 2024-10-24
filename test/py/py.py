import apple

p=apple.jit("λn.¬((∨)/ₒ #f ([(n|x)=0]'(⍳ 2 (⌊(√(ℝn))) 1)))")
assert not(p(8));assert p(7)

import numpy as np

d=apple.jit("λxs. ⸎n ⟜ 𝓉 xs; }:((*)`(𝒻 (ℝn-1) 0 n) xs)")
assert (d(np.array([1.,2,1]))==np.array([2.,2])).all()

moving_avg=apple.jit('([((+)/x)%(ℝ(:x))]\\`7)')
assert repr(moving_avg)=='<fn : Vec (i + 7) float → Vec i float>'
assert (moving_avg(np.arange(0.,10))==np.array([3,4,5,6])).all()

xs=np.array([[0.,4,2],[0,1,3]])

def softmax(x):
    exp_element=np.exp(x-x.max())
    return exp_element/np.sum(exp_element,axis=0)

ssoftmax=apple.jit('''
λxs.
  { m ⟜ (⋉)/* _1 xs; a ⟜ [e:(x-m)]`{0} xs
  ; sum ← [(+)/x]
  ; n ⟜ sum`{1} (a::M float)
  ; |:(([(%x)'y]`{0,1} n a))
  }
''')
assert (ssoftmax(xs)==softmax(xs)).all()

luhn=apple.jit('''
λxs.
  { digitSum ← [?x>10,.x-9,.x]
  ; t ← (+)/ [digitSum (x*y)]`(~(}:xs)) (}: (cyc. ⟨2,1::int⟩ 8))
  ; 10-(t|10)=}.xs
  }
''')
assert luhn(np.array([4,0,1,2,8,8,8,8,8,8,8,8,1,8,8,1]))
del luhn

def unstring(isbn):
    return np.array([int(c) for c in isbn.replace('-','')])

isbn13=apple.jit("λxs. {t ← (+)/(*)`xs (}:(cyc. ⟨1,3::int⟩ 7)); (t|10)=0}")

assert isbn13(unstring("978-0596528126"))
assert not(isbn13(unstring("978-1788399083")))
del isbn13

any_v=apple.jit("λbs. (∨)/ₒ #f bs :: bool")
assert any_v(np.array([False,False,False,True]))
assert not(any_v(np.array([False,False,False])))

prime_mask=apple.jit("λN. (λn.¬((∨)/ₒ #f ([(n|x)=0]'(⍳ 2 (⌊(√(ℝn))) 1))))'(irange 2 N 1)")
assert (prime_mask(9)==np.array([True,True,False,True,False,True,False,False])).all()

def vs(M,N):
    A=np.random.rand(M,N);x=np.random.rand(N)
    v=apple.jit(f"[(x::Arr ({M}×{N}) float)%:y]")
    print(A@x);print(v(A,x))

def test(M,N,K):
    bs=np.random.rand(M,N);cs=np.random.rand(N,K)
    m=apple.jit(f"[(x::Arr ({M}×{N}) float)%.(y::Arr ({N}×{K}) float)]")
    assert (bs@cs==m(bs,cs)).all()

vs(50,64);vs(64,64)
test(512,512,512);test(64,64,64)
test(512,128,128);test(512,256,128)
