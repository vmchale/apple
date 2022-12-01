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

bessel_apple = '''
  λ𝛼.λx.
    { fact ← [(*)/1 1 (irange 1 x 1)]
    ; f10 ← λx.λk.
      { rf ← [(*)/1 1 (frange x (x+y-1) (⌊y))]; ffact ← rf 1
      ; mkIx ← \k. {kk⟜itof k; ((_((x^2)%4))^k%(rf (itof (𝛼+1)) kk))%(ffact kk)}
      ; (+)/1 0 (mkIx'1(irange 0 k 1))
      }
    ; (((x%2)^𝛼)%(itof (fact 𝛼)))*f10 x 100
    }'''

erf_f=apple.cache(erf_apple)
ncdf_f=apple.cache(ncdf_apple)
bessel_f=apple.cache(bessel_apple)

def erf(x):
  return apple.f(erf_f,x)

def ncdf(x):
  return apple.f(ncdf_f,x)

def bessel1(a,x):
  return apple.f(bessel_f,a,x)
