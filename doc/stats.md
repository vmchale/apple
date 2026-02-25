---
title: Special Functions in Apple
author: Vanessa McHale
date: 25 Feb. 2026
keywords: [special functions, hypergeometric functions, gamma function, cdf, numerical methods, statistics]
bibliography: doc/stats.bib
---

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

See the familiar factorial:

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

This takes the $a_1\ldots a_p$ and $b_1\ldots b_q$ as array arguments:

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
