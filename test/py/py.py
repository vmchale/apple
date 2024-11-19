import numpy as np;import apple

d=apple.jit("λxs. ⸎n ⟜ 𝓉 xs; }:((*)`(𝒻 (ℝn-1) 0 n) xs)")
assert (d(np.array([1.,2,1]))==np.array([2.,2])).all()

moving_avg=apple.jit('([(+)/x%ℝ(:x)]⑄7)')
assert repr(moving_avg)=='<fn : Vec (i + 7) float → Vec i float>'
assert (moving_avg(np.arange(0.,10))==np.array([3,4,5,6])).all()

xs=np.array([[0.,4,2],[0,1,3]])

def softmax(x):
    exp_element=np.exp(x-x.max())
    return exp_element/np.sum(exp_element,axis=0)

ssoftmax=apple.jit('''
λxs.
  { m ⟜ (⋉)/* _1 xs; a ⟜ [e:(x-m)]`{0} xs
  ; |:((λxs. {s⟜ (+)/xs; (%s)'xs})`{1} (a::M float))
  }
''')
assert (ssoftmax(xs)==softmax(xs)).all()

pf=apple.jit('''
λn.
  { ni ⟜ ⌊(√(ℝn))
  ; pns ← (⍳ 2 ni 1)
  ; isPrime ← λn.¬((∨)/ₒ #f ([(n|x)=0]'(⍳ 2 (⌊(√(ℝn))) 1))); pf ⇐ (isPrime #.)
  ; pps ⟜  ((λk. ((n|k)=0)) #. pns)
  ; ?ni^2=n
    ,.pf (pps⧺((n/.)'(}:? pps)))
    ,.pf (pps⧺(n⊳((n/.)'pps)))
  }
''')
assert (pf(60)==np.array([2,3,5])).all()

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

isbn13=apple.jit("λxs. {t ← (+)/(*)`xs (}:(𝔸13⊙7)); t|10=0}")

assert isbn13(unstring("978-0596528126"))
assert not(isbn13(unstring("978-1788399083")))
del isbn13

any_v=apple.jit("λbs. (∨)/ₒ #f bs :: bool")
assert any_v(np.array([False,False,False,True]))
assert not(any_v(np.array([False,False,False])))

prime_mask=apple.jit("λN. (λn.¬((∨)/ₒ #f ([(n|x)=0]'(⍳ 2 (⌊(√(ℝn))) 1))))'(irange 2 N 1)")
assert (prime_mask(9)==np.array([True,True,False,True,False,True,False,False])).all()
