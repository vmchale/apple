```
jpm install --local
jpm -l janet -e '(import apple)' -r
repl:1:> (def f (apple/jit "[1.0+x]"))
<jit 0x6000031A4110>
repl:2:> (f 1.0)
2
```
