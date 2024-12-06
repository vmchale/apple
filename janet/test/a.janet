(import apple)

(def dp (apple/jit "[(+)/(*)`(x::Vec n float) y]"))
(assert (= (dp @[1.0 3.0 5.0] @[2.0 4.0 6.0]) 44.0))

(def moving-average (apple/jit ``([(+)/x%ℝ(:x)]\`7)``))
(assert (= (string moving-average) ``Vec (i + 7) float → Vec i float``))
(assert (deep= (moving-average @[1.0 2.0 3.0 4.0 5.0 6.0 7.0 8.0 9.0 10.0]) @[4.0 5.0 6.0 7.0]))

(def is-prime (apple/jit ``λn.¬((∨)/ₒ #f ([n|x=0]'⍳ 2 (⌊(√(ℝn))) 1))``))
(assert (is-prime 7)) (assert (not (is-prime 8))) (assert (not (is-prime 9)))

(def prime-mask (apple/jit ``λN. (λn.¬((∨)/ₒ #f ([n|x=0]'⍳ 2 (⌊(√(ℝn))) 1)))'irange 2 N 1``))
(assert (deep= (prime-mask 9) @[true true false true false true false false]))

(def cat (apple/jit ``[(x::Vec n int)++y]``))
(assert (deep= (cat @[1 2] @[4 3]) @[1 2 4 3]))

(def any (apple/jit ``λbs. (∨)/ₒ #f bs :: bool``))
(assert (= (any @[false false true]) true))

(def isbn-13 (apple/jit ``λxs. ((+)/(*)`xs (}:(𝔸13⊙7)))|10=0``))
(assert (isbn-13 @[9 7 8 0 5 9 6 5 2 8 1 2 6]))
(assert (not (isbn-13 @[9 7 8 1 7 8 8 3 9 9 0 8 3])))

(def fibs (apple/jit ``λN. [x˙0˙1]'{A⟜⟨⟨1,1⟩,⟨1,0::int⟩⟩; gen. A (A%.) N}``))
(assert (deep= (fibs 6) @[1 1 2 3 5 8]))
