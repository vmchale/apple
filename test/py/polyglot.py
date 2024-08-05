import numpy as np

inputs = np.array([[0.,0],[0,1],[1,0],[1,1]])
expected_output = np.array([0.,1,1,0])

# weights and bias initialization
hidden_weights=np.array([[0.51426693,0.56885825],[0.48725347,0.15041493]])
hidden_bias=np.array([0.79726405,0.67601843])
output_weights=np.array([0.14801747,0.37182892])
output_bias=0.57823076

import apple

h=apple.jit('''
λX.λwh.λbh.
{
  sigmoid ← [1%(1+ℯ(_x))];
  sigmoid`{0} ([(+)`bh x]'(X%.wh))
}
''')
hidden_layer_output=apple.f(h,inputs,hidden_weights,hidden_bias)

o=apple.jit('''
λho.λwo.λbo.
{
  sigmoid ← [1%(1+ℯ(_x))];
  sigmoid'((+bo)'(ho%:wo))
}
''')
predicted_output=apple.f(o,hidden_layer_output,output_weights,output_bias)

dpo=apple.jit('''
λprediction.
{
  Y ⟜ ⟨0.0,1,1,0⟩;
  sDdx ← [x*(1-x)];
  l1E ← (-)`Y prediction;
  (*)`(sDdx'prediction) l1E
}
''')
d_predicted_output = apple.f(dpo,predicted_output)

dhe=apple.jit('''
λl1Δ.λwo.λho.
{
  sDdx ← [x*(1-x)];
  he ← l1Δ (*)⊗ (wo::Vec 4 float);
  (*)`{0,0} (sDdx`{0} ho) he
}
''')
d_hidden_layer=apple.f(dhe,d_predicted_output,output_weights,hidden_layer_output)

hw=apple.jit('''
λwh.λhΔ.
{
  X ⟜ ⟨⟨0.0,0⟩,⟨0,1⟩,⟨1,0⟩,⟨1,1⟩⟩;
  (+)`{0,0} wh ((|:X)%.hΔ)
}
''')
hidden_weights=apple.f(hw,hidden_weights,d_hidden_layer)
print('hidden_weights\n',hidden_weights)

hb=apple.jit("λbh.λhΔ. [(+)/ₒ x y]`{0,1} bh (hΔ::M float)")
hidden_bias=apple.f(hb,hidden_bias,d_hidden_layer)
print('hidden_bias\n',hidden_bias)

bo=apple.jit("λbo.λl1Δ. {sum ← [(+)/x]; bo + sum (l1Δ::Vec 4 float)}")
output_bias=apple.f(bo,output_bias,d_predicted_output)
print('output_bias\n',output_bias)
