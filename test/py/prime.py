import apple

p=apple.jit("λn.¬((∨)/ₒ #f ([(n|x)=0]'(⍳ 2 (⌊(√(ℝn))) 1)))")
print(8, apple.f(p,8))
print(7, apple.f(p,7))
