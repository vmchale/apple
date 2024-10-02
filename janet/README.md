```
jpm install --local
jpm -l janet
repl:1:> (import apple)
@{_ @{:value <cycle 0>} apple/tyof @{:private true}}
repl:2:> (print (apple/tyof "[1.0+x]"))
float → float
nil
```
