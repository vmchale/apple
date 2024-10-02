(import apple)

(def dp (apple/jit "[(+)/ ((*)`(x::Vec n float) y)]"))
(dp @[1.0 2.0] @[2.0 4.0])

(def rot (apple/jit "[2 ⊖ (x::Vec n float)]"))
(rot @[1.0 2.0 3.0])
