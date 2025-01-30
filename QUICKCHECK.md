- [ ] gray code enc<->dec roundtrip
- [ ] amgm is between its arguments
- [ ] [(+)/x%ℝ(:x)]'([x]\`7 (frange 0 9 10)) equal to [(+)/x%ℝ(:x)]\` 7
- [ ] ~`{1∘[2]}⟨⟨0,1⟩,⟨1,0::int⟩,⟨2,4⟩⟩ vs ~'⟨⟨0,1⟩,⟨1,0::int⟩,⟨2,4⟩⟩
- [ ] |: ((2 ⊖)`{1} ⟨⟨1,2⟩,⟨3,4⟩,⟨5,6.0⟩⟩)
- [ ]  > [(⋉)/x] ⨳ {3} ⟨_1,0,4,_2,3,3,1,1,0,_5.0⟩
Vec 8 [4.0, 4.0, 4.0, 3.0, 3.0, 3.0, 1.0, 1.0]
 > [(⋉)/x] \`3 ⟨_1,0,4,_2,3,3,1,1,0,_5.0⟩
Vec 8 [4.0, 4.0, 4.0, 3.0, 3.0, 3.0, 1.0, 1.0]
- [ ] :qc \xs. (xs˙)'even.⩪xs = even. § xs
- [ ] \xs.\ys. (∧)/((=)`xs ys)
- [ ] Jensen-Shannon: H(p+q/2)-(H(p)+H(q))/2
  - [ ] gte 0 (Hellinger as well)

- Index of all values >0.5
```
(->2)'([x->1>0.5]#.([(x,y)]`(𝔯 0 1::Vec n float) (⍳9)))
```

gen. / iter

also tail/init ^

# Identity-fill

(\n. [?x=n,.1::int,.0]'(⍳9))'⍳ 0 9

# Identities

n choose k = (n-1) choose (k-1) + (n-1) choose k
https://en.wikipedia.org/wiki/Stirling_numbers_of_the_second_kind#Recurrence_relation
