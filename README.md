# Apple Array System

## Compiler-As-a-Library

Rather than an environment-based interpreter or a compiler invoked on the
command line and generating object files, Apple generates machine code which can
be used by a JIT compiler or in general.

Thus the same implementation can be used interpreted, compiled, or called from
another language.

```
 > ([((+)/x)%ℝ(:x)]\`7) (frange 1 10 10)
Arr (4) [4.0, 5.0, 6.0, 7.0]
```

```python
>>> import apple
>>> import numpy as np
>>> apple.apple('([((+)/x)%(ℝ(:x))]\`7)',np.arange(0,10,dtype=np.float64))
array([3., 4., 5., 6.])
>>>
```

```R
> source("R/apple.R")
> apple("([((+)/x)%ℝ(:x)]\\`7)",seq(0,10,1.0))
[1] 3 4 5 6 7
```

## Dimension As a Functor

This is based on J (and APL?). Looping is replaced by functoriality (rerank);

## Moving Code vs. Moving Data

> For a computation to take place the data and the program have to be at the
> same point in space-time - this is just physics. You can move the data to the
> program or the program to the data, or both somewhere else. ...
> data movement predominates.

- [Joe Armstrong](https://twitter.com/joeerl/status/1087678726911987712)
