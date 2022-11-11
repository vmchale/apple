import apple
import numpy as np

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

regress_f=apple.cache(regress_apple)

def regress(x,y):
  return apple.f(regress_f,x,y)

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

ncdf_apple = 'λz. {erf ← ' + erf_apple + '; zz ⟜ z%(√2); 0.5*(1+erf(zz))}'

erf_f=apple.cache(erf_apple)
ncdf_f=apple.cache(ncdf_apple)

def erf(x):
  return apple.f(erf_f,x)

def ncdf(x):
  return apple.f(ncdf_f,x)
