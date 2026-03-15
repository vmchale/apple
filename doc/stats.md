---
title: Special Functions in Apple
author: Vanessa McHale
date: 25 Feb. 2026
keywords: [special functions, hypergeometric functions, gamma function, cdf, numerical methods, statistics, gamma function]
bibliography: doc/stats.bib
---

# Gamma Function

We use the Lanczos approximation as described in @num, extended to work on
negative numbers by

$$\Gamma(1-z)=\displaystyle\frac{\pi}{\Gamma(z)\sin(\pi z)}$$

<!-- For $z \leq \frac{1}{2}$,  -->

```apple
λz.
  { gammaln ← λz.
    {
      zz ⟜ z-1;
      c0 ← 0.999999999999997092;
      𝛾 ← 607%128;
      coeffs ← ⟨ 57.1562356658629235
               , _59.5979603554754912
               , 14.1360979747417471
               , _0.491913816097620199
               , 0.339946499848118887e-4
               , 0.465236289270485756e-4
               , _0.983744753048795646e-4
               , 0.158088703224912494e-3
               , _0.210264441724104883e-3
               , 0.217439618115212643e-3
               , _0.164318106536763890e-3
               , 0.844182239838527433e-4
               , _0.261908384015814087e-4
               , 0.368991826595316234e-5
               ⟩;
      ss ← (+)/([y%(zz+itof x)]`(1..14) coeffs);
      ((zz+0.5)*_.(zz+𝛾+0.5)-(zz+𝛾+0.5))+_.(√(2*𝜋)*(c0+ss))
    };
    e:(?z≥0.5,.gammaln z,._.𝜋-_.(sin.(𝜋*z))-gammaln(1-z))
  }
```

# Hypergeometric Functions

The hypergeometric function is given by

$$_pF_q(a_1,\ldots,a_p;b_1,\ldots,b_q;z) = \displaystyle\sum_{n=0}^\infty\frac{(a_1)_n\cdots(a_p)_n}{(b_1)_n\cdots(b_q)_n}\frac{z^n}{n!}$$

where $(a)_n=a(a+1)\cdots(a+n-1)$, $(a)_0=1$ is the rising factorial, also called the Pochhammer symbol.

We can define the rising factorial to work on real numbers like so:

```apple
 > :store rf [(*)/ₒ 1 (𝒻 x (x+y-1) (⌊y))]
 > :ty rf
float → float → float
```

`rf 1` is then the factorial:

```apple
 > rf 1'frange 1 7 7
Vec 7 [1.0, 2.0, 6.0, 24.0, 120.0, 720.0, 5040.0]
```

The hypergeometric function in Apple:

```apple
λa.λb.λz.
{
  rf ← [(*)/ₒ 1 (frange x (x+y-1) (⌊y))]; fact ← rf 1;
  Σ ← λN.λa. (+)/ₒ 0 (a'⍳N); Π ⇐ λa.((*)/)∴(a');
  Σ 30 (λn. {nn⟜ℝ n; (Π (λa.rf a nn) a%Π (λb. rf b nn) b)*(z^n%fact nn)})
}
```

This takes $a_1\ldots a_p$ and $b_1\ldots b_q$ as array arguments:

```apple
 > :yank H math/hypergeometric.🍏
 > :ty H
Vec (i + 1) float → Vec (j + 1) float → float → float
```

# erf

The error function $\text{erf}(z)$ satisfies the following [@mathworld]:

$$
    \begin{align*}
    \text{erf}(z)&=\frac{2}{\sqrt{\pi}}\int_0^ze^{-t^2}dt \\
                 &=\frac{2z}{\sqrt{\pi}} {}_1F_1\left(\frac{1}{2};\frac{3}{2};-z^2\right) \\
                 &=\frac{2ze^{-z^2}}{\sqrt{\pi}} {}_1F_1\left(1;\frac{3}{2};z^2\right)
    \end{align*}
$$

The former has convergence problems [@shaw2002]. Simplifying the latter:

```{.apple include="math/erf.🍏" startLine=2}
```

# Normal Distribution CDF

The CDF for the standard normal distribution $N(0,1)$ can be calculated as $\displaystyle\frac{1}{2}\left(1+\text{erf}\left(\frac{z}{\sqrt{2}}\right)\right)$:

```apple
λz.
{
  erf ← λz.
        { ffact ← [(*)/ₒ 1 (𝒻 1 x (⌊x))]
        ; Σ ← λN.λa. (+)/ₒ 0 (a'⍳N);
        ; (2%√𝜋)*Σ 30 (λn. {nf⟜ℝn; ((_1^n)*z^(2*n+1))%(ffact nf*(2*nf+1))})
        };
  zz ⟜ z%√2;
  0.5*(1+erf(zz))
}
```

# t-Distribution CDF

For $\nu$ degrees of freedom we have [@amos]:

$$\frac{1}{2}+x\frac{\Gamma(\frac{1}{2}(\nu+1))}{\sqrt{\pi\nu}\Gamma(\frac{\nu}{2})}{}_2F_1\left(\frac{1}{2},\frac{1}{2}(\nu+1);\frac{3}{2};-\frac{x^2}{\nu}\right)$$

for $|x|<\sqrt{\nu}$ ($_2F_1$ converges if and only if $|z|<1$).
