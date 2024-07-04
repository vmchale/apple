(declare-project :name "apple")

(declare-native
  :name "apple"
  :source ["japple.c"]
  :cflags ["-lapple" "-rpath" "/usr/local/lib"])
