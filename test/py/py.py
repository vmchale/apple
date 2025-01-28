import numpy as np;import apple

d=apple.jit("λxs. ⸎n ⟜ 𝓉 xs; }:((*)`(𝒻 (ℝn-1) 0 n) xs)")
assert (d(np.array([1.,2,1]))==np.array([2.,2])).all()

moving_avg=apple.jit('([(+)/x%ℝ(:x)]⑄7)')
assert repr(moving_avg)=='<fn : Vec (i + 7) float → Vec (i + 1) float>'
assert (moving_avg(np.arange(0.,10))==np.array([3,4,5,6])).all()

bit_matrix=apple.jit("[x (=)⊗ xᶥ]");i_vec=apple.jit("(([x]@.)')")
assert (i_vec(bit_matrix(np.array([2,0,1])))==np.array([2,0,1])).all()

base=apple.jit("λa.λn. {log ← (%)⑂_.; N ⟜ ⌊((log⑂ℝ) a n)+1; ~(ug. (λs. (s/.n, s|n)) a N)}")
assert (base(17,3)==np.array([1,2,2])).all()

hms=apple.jit("λs. (->1)'({: ((λqr.λb. (qr->2|b, qr->2/.b)) Λₒ (0,s) ⟨24,60,60⟩))")
assert (hms(86399)==np.array([23,59,59])).all()

ruffini=apple.jit("λp.λa. {:((λs.λc. (a*s+c)) Λₒ 0 (p::𝟙𝟘))")
assert (ruffini(np.array([1,2,1]),-1)==np.array([1,1,0])).all()

def softmax(x):
    exp_element=np.exp(x-x.max())
    return exp_element/np.sum(exp_element,axis=0)

xs=np.array([[0.,4,2],[0,1,3]])

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
  ; pns ← ⍳ 2 ni
  ; isPrime ← λn.¬((∨)/ₒ #f ([n|x=0]'(⍳ 2 (⌊(√(ℝn)))))); pf ⇐ (isPrime #.)
  ; pps ⟜  (λk. (n|k)=0) #. pns
  ; ?ni^2=n
    ,.pf (pps⧺((n/.)'}:?pps))
    ,.pf (pps⧺(n⊳((n/.)'pps)))
  }
''')
assert (pf(60)==np.array([2,3,5])).all()

luhn=apple.jit('''
λxs.
  { digitSum ← [?x>10,.x-9,.x]
  ; t ← (+)/ [digitSum (x*y)]`(~(}:xs)) (}: (⟨2,1::int⟩⊙8))
  ; 10-t|10=}.xs
  }
''')
assert luhn(np.array([4,0,1,2,8,8,8,8,8,8,8,8,1,8,8,1]))
del luhn

def unstring(isbn):
    return np.array([int(c) for c in isbn.replace('-','')])

isbn13=apple.jit("[(x⋅(}:(𝔸13⊙7)))|10=0]")

dec=apple.jit("λn. (⊻)/ₒ n ((n>>)'⍳ 1 63)")
assert dec(8)==15

assert isbn13(unstring("978-0596528126"))
assert not(isbn13(unstring("978-1788399083")))
del isbn13

any_v=apple.jit("λbs. (∨)/ₒ #f bs :: bool")
assert any_v(np.array([False,False,False,True]))
assert not(any_v(np.array([False,False,False])))

prime_mask=apple.jit("λN. (λn.¬((∨)/ₒ #f ([(n|x)=0]'⍳ 2 (⌊(√(ℝn))))))'⍳ 2 N")
assert (prime_mask(9)==np.array([True,True,False,True,False,True,False,False])).all()

xs=np.random.rand(100)
sort=apple.jit("[⍋(x::Vec n float)]")
res=sort(xs);xs.sort()
assert (res==xs).all()
