(import apple)

(def dp (apple/jit "[(+)/(*)`(x::𝟙𝞈) y]"))
(assert (= (dp @[1.0 3.0 5.0] @[2.0 4.0 6.0]) 44.0))

(def moving-average (apple/jit ``([(+)/x%ℝ(:x)]\`7)``))
(assert (= (string moving-average) ``Vec (i + 7) float → Vec i float``))
(assert (deep= (moving-average @[1.0 2.0 3.0 4.0 5.0 6.0 7.0 8.0 9.0 10.0]) @[4.0 5.0 6.0 7.0]))

(def is-prime (apple/jit ``λn.¬((∨)/ₒ #f ([n|x=0]'⍳ 2 (⌊(√(ℝn)))))``))
(assert (is-prime 7)) (assert (not (is-prime 8))) (assert (not (is-prime 9)))

(def prime-mask (apple/jit ``λN. (λn.¬((∨)/ₒ #f ([n|x=0]'⍳ 2 (⌊(√(ℝn))))))'irange 2 N``))
(assert (deep= (prime-mask 9) @[true true false true false true false false]))

(def ruffini (apple/jit ``λp.λa. {:((λs.λc. (a*s+c)) Λₒ 0 (p::𝟙𝟘))``))
(assert (deep= (ruffini @[1 2 1] -1) @[1 1 0]))

(def base (apple/jit ``λa.λn. {log ← (%)⑂_.; N ⟜ ⌊((log⑂ℝ) a n)+1; ~(ug. (λs. (s/.n, s|n)) a N)}``))
(assert (deep= (base 15 5) @[3 0]))

(def cat (apple/jit ``[x::𝟙𝟘++y]``))
(assert (deep= (cat @[1 2] @[4 3]) @[1 2 4 3]))

(def any (apple/jit ``λbs. (∨)/ₒ #f bs :: bool``))
(assert (= (any @[false false true]) true))

(def isbn-13 (apple/jit ``xs ↦ (xs⋅(}:(𝔸13⊙7)))|10=0``))
(assert (isbn-13 @[9 7 8 0 5 9 6 5 2 8 1 2 6]))
(assert (not (isbn-13 @[9 7 8 1 7 8 8 3 9 9 0 8 3])))

(def fibs (apple/jit ``λN. [x˙0˙1]'{A⟜⟨⟨1,1⟩,⟨1,0::int⟩⟩; gen. A (A%.) N}``))
(assert (deep= (fibs 6) @[1 1 2 3 5 8]))

(def hms (apple/jit ``λs. (->1)'({: ((λqr.λb. (qr->2|b, qr->2/.b)) Λₒ (0,s) ⟨24,60,60⟩))``))
(assert (deep= (hms 86399) @[23 59 59]))
