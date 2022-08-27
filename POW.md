Exponentiation with integer exponent:
  1. To check parity, `AND` with 0x1 instead of `idiv`

See:

```ats
// Fast integer exponentiation. This performs O(log n) multiplications. This
// function is mostly useful for exponentiation in modular arithmetic, as
// it can overflow.
fun exp {n:nat} .<n>. (x : int, n : int(n)) : int =
  case+ x of
    | 0 => 0
    | x =>> 
      begin
        if n > 0 then
          let
            var n2 = half(n)
            var i2 = n % 2
          in
            if i2 = 0 then
              exp(x * x, n2)
            else
              let
                var y = x * exp(x * x, n2)
              in
                y
              end
          end
        else
          1
      end
```
