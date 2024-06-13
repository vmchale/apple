import numpy as np

def sigmoid (x):
    return 1/(1+np.exp(-x))

inputs = np.array([[0.,0],[0,1],[1,0],[1,1]])

# weights and bias initialization
hidden_weights=np.array([[0.51426693,0.56885825],[0.48725347,0.15041493]])
hidden_bias=np.array([0.79726405,0.67601843])
output_weights=np.array([[0.14801747],[0.37182892]])
output_bias=0.57823076

import apple

code=apple.jit("λX.λwh.λbh. [1%(1+ℯ(_x))]`{0} ([(+)`bh x]'(X%.wh))")
hidden_layer_output=apple.f(code,inputs,hidden_weights,hidden_bias)
print(hidden_layer_output)

src=apple.jit("λho.λwo.λbo. [1%(1+ℯ(_x))]'((+bo)'(ho%:wo))")
output_weights = output_weights.reshape([2])
predicted_output=apple.f(src,hidden_layer_output,output_weights,output_bias)
print(predicted_output)
