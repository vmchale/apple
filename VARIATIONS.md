# Take/Drop

Drop 6:

```
[}.\`7 x]
```

Take 7:

```
{. ([x] \`7 (irange 0 9))
```

```
 > {take ← λn.λxs. ⍳ 0 (n-1)⊂xs; take 7 (irange 0 9)}
Vec 7 [0, 1, 2, 3, 4, 5, 6]
```

## Delete

Delete the `i`th element of vector `xs`:

```
 > ⍳ 0 9 \\ 4
Vec 9 [0, 1, 2, 3, 5, 6, 7, 8, 9]
 > {del ← λi.λxs. ((≠i)#.xsᶥ)⊂xs; del 4 (irange 0 9)}
Vec 9 [0, 1, 2, 3, 5, 6, 7, 8, 9]
```

## Argmax

```
 > [{m⟜(⋉)/x; (=m)@.x}] ⟨1::int,0,_2,9,3,4,1,2,0⟩
3
 > (λas. {ixed ← λxs. [(x,y)]`xs xsᶥ; ((λ(x,i).λ(y,j).?x>y,.(x,i),.(y,j))/(ixed as))->2}) ⟨1::int,0,_2,9,3,4,1,2,0⟩
3
```
## Scatter

We can imitate scatter (`⊂`) with

```
λis.λxs. (xs˙)'is
```

# Filter

One can replicate the functionality of filter (`#.`) with `⩪` (indices-of) and

```
λp.λxs. p⩪xs⊂xs
```

# Outer Product (Self)

```
[x(*)⊗x]
λxs. {x ⟜ ♯'xs; x%.|:x};
λxs. {x ⟜ ♯'xs; x%.♯xs};
```

# Map, Rank

Map (`'`) has type

```
(') : (a → b) → Vec i a → Vec i b
```

It cuts across the leading axis.

# Sum Augmented Axis

```
 > (λa.λb. [(+)/ₒ x y]`{0,1∘[2]} a (b::M float)) ⟨1,2,3⟩ ⟨⟨4.0,5⟩,⟨6,7⟩,⟨8,9⟩⟩
Vec 3 [10.0, 15.0, 20.0]
 > (λa.λb. [(+)/x]`{1∘[2]} ((<|)`{0,1∘[2]} a (b::M float))) ⟨1,2,3⟩ ⟨⟨4.0,5⟩,⟨6,7⟩,⟨8,9⟩⟩
Vec 3 [10.0, 15.0, 20.0]
```

# Infix, Convolve

```
 > [(+)/x%ℝ(:x)]\`7 (frange 0 9 10)
Vec 4 [3.0, 4.0, 5.0, 6.0]
 > [(+)/x%ℝ(:x)]⨳{7} (frange 0 9 10)
Vec 4 [3.0, 4.0, 5.0, 6.0]
```

# Indices

```
[xᶥ]
```

```
([#t]⩪)
```

```
λxs. {n ← 𝓉xs; gen. (0,xs˙0) (λx. {i⟜x->1+1; (i,xs˙i)}) n}
```

```
λxs. {i ← (λx.λy. x+1) Λₒ (0::int) xs; [(x,y)]`({:i) xs}
```

# Diagonal

```
 > [{ix<-xᶥ;(˙)`(x::M int) ix}] (irange 1 4 〃 4)
Vec 4 [1, 2, 3, 4]
 > 𝐒 (λx.λy.(˙)`x y) [xᶥ] (irange 1 4 〃 4)
Vec 4 [1, 2, 3, 4]
 > di. (irange 1 4 〃 4)
Vec 4 [1, 2, 3, 4]
```

# Successive Application

```
 > (-)\~(irange 0 9)
Vec 9 [1, 1, 1, 1, 1, 1, 1, 1, 1]
 > [}.x-{.x]\`2 (irange 0 9)
Vec 9 [1, 1, 1, 1, 1, 1, 1, 1, 1]
```
<!-- with fib/strong induction? -->

# Delete
```
 > {delete ← λxs.λi. ((≠i)#.xsᶥ)⊂xs; delete ⟨1::int,2,3,4⟩ 2}
Vec 3 [1, 2, 4]
 > {delete ← λxs.λi. (⍳ 0 (i-1)⊂xs)⧺(⍳ (i+1) (𝓉xs-1)⊂xs); delete ⟨1::int,2,3,4⟩ 2}
Vec 3 [1, 2, 4]
```
