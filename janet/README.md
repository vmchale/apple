```
jpm install --local
jpm -l janet -e '(import apple)' -r
repl:1:> (def dp (apple/jit "[(+)/ ((*)`(x::Vec n float) y)]"))
<jit 0x6000003F0070>
repl:2:> (dp @[1.0 2.0] @[2.0 4.0])
10
```
