import numpy as np

def sigmoid (x):
    return 1/(1+np.exp(-x))

inputs = np.array([[0.,0],[0,1],[1,0],[1,1]])
expected_output = np.array([0.,1,1,0])

# weights and bias initialization
hidden_weights=np.array([[0.51426693,0.56885825],[0.48725347,0.15041493]])
hidden_bias=np.array([0.79726405,0.67601843])
output_weights=np.array([[0.14801747],[0.37182892]])
output_bias=0.57823076

import apple

h=apple.jit("λX.λwh.λbh. [1%(1+ℯ(_x))]`{0} ([(+)`bh x]'(X%.wh))")
hidden_layer_output=apple.f(h,inputs,hidden_weights,hidden_bias)
print(hidden_layer_output)

o=apple.jit("λho.λwo.λbo. [1%(1+ℯ(_x))]'((+bo)'(ho%:wo))")
output_weights = output_weights.reshape([2])
predicted_output=apple.f(o,hidden_layer_output,output_weights,output_bias)
print(predicted_output)

ehe=apple.jit('''
λY.λwo.λprediction.
{
  sDdx ← [x*(1-x)];
  l1E ← (-)`(Y::Vec n float) prediction;
  l1Δ ← (*)`(sDdx'prediction) l1E;
  l1Δ (*)⊗ wo
}
''')
error_hidden_layer=apple.f(ehe,expected_output,output_weights,predicted_output)
print(error_hidden_layer)
