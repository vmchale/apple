import apple
import numpy as np

def regress(x,y):
  regress_apple = '''
      λxs.λys.
      {
        Σ ← [(+)/1 0 x];
        n ⟜ 𝑖(:xs);
        xbar ⟜ (Σ xs) % n; ybar ← (Σ ys) % n;
        xy ← Σ ((*)`xs ys); x2 ← Σ ((^2)'1 xs);
        denom ← (x2-(n*(xbar^2)));
        (xy-(n*xbar*ybar))%denom
      }'''
  return apple.apple(regress_apple,x,y)

def erf(x):
  erf_apple = '''
    λz.
    {
      f11 ← λz.
      {
        rf ← [(*)/1 1 (frange x (x+y-1) (⌊y))];
        ix ← irange 0 99 1;
        mkIx ← [(z^x)%(rf 1.5 (itof x))];
        (+)/1 0 (mkIx'1 ix)
      };
      2*z*(e: (_(z^2)))*(f11 (z^2))%(√𝜋)
    }'''
  return apple.apple(erf_apple,x)
