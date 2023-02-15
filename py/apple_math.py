import apple
import numpy as np

def fs(fp):
    with open(fp) as f:
        bs = f.read()
    return bs

regress_apple = fs('../test/examples/b.🍎')
erf_apple = fs('../math/erf.🍏')
ncdf_apple = fs('../math/ncdf.🍎')
bessel_apple = fs('../math/bessel.🍏')
chisqcdf_apple = fs('../math/chisqcdf.🍎')
gamma_apple = fs('../math/gamma.🍏')

regress_f=apple.cache(regress_apple)
erf_f=apple.cache(erf_apple)
ncdf_f=apple.cache(ncdf_apple)
bessel_f=apple.cache(bessel_apple)
chisqcdf_f=apple.cache(chisqcdf_apple)
gamma_f=apple.cache(gamma_apple)

def regress(x,y):
  return apple.f(regress_f,x,y)

def erf(x):
  return apple.f(erf_f,x)

def ncdf(x):
  return apple.f(ncdf_f,x)

def bessel1(a,x):
  return apple.f(bessel_f,a,x)

def chisqcdf(k,x):
  return apple.f(chisqcdf_f,k,x)

def gamma(x):
  return apple.f(gamma_f,x)
