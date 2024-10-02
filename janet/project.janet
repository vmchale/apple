(declare-project :name "apple")

(def [o-r o-w] (os/pipe))

(os/execute
  @("ghc-pkg" "field" "rts" "include-dirs")
  :px
  {:out o-w})

(def out
  (:read o-r math/int32-max))

(def include-dir
  (string/trim ((string/split "\n" out) 1)))

(declare-native
  :name "apple"
  :source ["japple.c"]
  :cflags ["-lapple" "-I" include-dir "-rpath" "/usr/local/lib"]
  :ldflags ["-L" "/usr/local/lib"])
