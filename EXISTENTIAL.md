Notes on existential types:

1. Only introduced by filter, iota (on RHS)

2. Eliminate by pulling out ∃n. int(n) as int(#n). We don't ever need ∃n. int(n)
   to check against ∀n. int(n) (say) because we don't allow higher-rank types
   (also it wouldn't work).

3. #n unifies with any (universal) type variable as a constant; #n doesn't unify
   with 2 (say), ∃n. int(n) shouldn't unify with int(2) (the former being, say,
   the result of a filter...)

4. DISPLAY: I think we can restrict so that all #n are on the RHS of the arrow,
   then display as ∃n. ...


existential vs. universal:

int(4) checks against ∃n. int(n) BUT it is not an instance of ∀n. int(n)

however, a term of type ∀n. int(n) could be instantiated to type int(4) if
wanted.
term of type ∃n. int(n) does not check against ∀n. int(n)
term of type ∀n. int(n) checks against ∃n. int(n) (can be instantiated etc.)
∃n. int(n) can be used as an argument to a function of type ∀n. int(n) → bool
(instantiate with virtual #n, then #n = n..., treating #n as a constant)

# Python

Note that Python gets this precisely wrong! In Python, an integer can have type
`Any`, and a value of type `Any` can be used as a string.

What *should* happen is that an integer checks against the type ∃a:type. a. Now,
a value of type ∃a:type. a should *not* check against the type string.

HOWEVER, a value of type ∀a:type. a SHOULD check against integer and also
string...
