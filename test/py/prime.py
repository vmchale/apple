import apple

p=apple.jit("λn.¬((∨)/ₒ #f ([(n|x)=0]'(⍳ 2 (⌊(√(ℝn))) 1)))")
assert apple.f(p,8)==False
assert apple.f(p,7)==True
