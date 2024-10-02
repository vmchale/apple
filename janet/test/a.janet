(import apple)

(def dp (apple/jit "[(+)/ ((*)`(x::Vec n float) y)]"))
(dp @[1.0 2.0] @[2.0 4.0])

(def moving-average (apple/jit
                               ``
                               ([((+)/x)%ℝ(:x)]\`7)
                               ``))
(moving-average @[1.0 2.0 3.0 4.0 5.0 6.0 7.0 8.0 9.0 10.0])
