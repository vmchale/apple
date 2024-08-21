import numpy as np
from scipy.cluster.vq import whiten

xs=np.array([[1.9,2.3,1.7],[1.5,2.5,2.2],[0.8,0.6,1.7]])

print(whiten(xs))

def r(fp):
    with open(fp) as file:
        data=file.read()
    return data

import apple

w=apple.jit(r('math/stats/whiten.🍏'))
ys=apple.f(w,xs)

print(ys)
