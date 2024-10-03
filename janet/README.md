```
jpm install --local
jpm -l janet -e '(import apple)' -r
repl:1:> (import apple)
@{_ @{:value <cycle 0>} apple/jit @{:private true} apple/tyof @{:private true}}
repl:2:> (def moving-average (apple/jit ``([((+)/x)%ℝ(:x)]\`7)``))
<jit 0x6000014240C0>
repl:3:> (moving-average @[1.0 2.0 3.0 4.0 5.0 6.0 7.0 8.0 9.0 10.0])
@[4 5 6 7]
(doc apple/jit)
```

# Documentation

```janet
repl:2:> (import apple)
@{_ @{:value <cycle 0>} apple/jit @{:private true} apple/tyof @{:private true}}
repl:3:> (doc apple)


    module (native)
    /Users/vanessa/dev/haskell/apple/janet/jpm_tree/lib/apple.so

    no documentation found.


nil
repl:4:> (doc apple/tyof)


    cfunction

    type of expression


nil
```
