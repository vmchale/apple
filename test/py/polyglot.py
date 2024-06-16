import numpy as np

inputs = np.array([[0.,0],[0,1],[1,0],[1,1]])
expected_output = np.array([0.,1,1,0])

# weights and bias initialization
hidden_weights=np.array([[0.51426693,0.56885825],[0.48725347,0.15041493]])
hidden_bias=np.array([0.79726405,0.67601843])
output_weights=np.array([0.14801747,0.37182892])
output_bias=0.57823076

import apple

h=apple.jit("λX.λwh.λbh. [1%(1+ℯ(_x))]`{0} ([(+)`bh x]'(X%.wh))")
hidden_layer_output=apple.f(h,inputs,hidden_weights,hidden_bias)

o=apple.jit("λho.λwo.λbo. [1%(1+ℯ(_x))]'((+bo)'(ho%:wo))")
predicted_output=apple.f(o,hidden_layer_output,output_weights,output_bias)

dpo=apple.jit('''
λprediction.
{
  Y ⟜ ⟨0.0,1,1,0⟩;
  l1E ← (-)`Y prediction;
  (*)`([x*(1-x)]'prediction) l1E
}
''')
d_predicted_output = apple.f(dpo,predicted_output)

ehe=apple.jit("λl1Δ.λwo. l1Δ (*)⊗ (wo::Vec n float)")
error_hidden_layer=apple.f(ehe,d_predicted_output,output_weights)
print('error_hidden_layer\n', error_hidden_layer)

#  dhe=apple.jit("λprediction.λhe. (*)`{0,0} ([x*(1-x)]`{0} (prediction::Vec n float)) he")
#  d_hidden_layer=apple.f(dhe,predicted_output,error_hidden_layer)
#  print('d_hidden_layer\n', d_hidden_layer)
