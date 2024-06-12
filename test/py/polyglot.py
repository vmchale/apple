import numpy as np

def sigmoid (x):
    return 1/(1+np.exp(-x))

inputs = np.array([[0.,0],[0,1],[1,0],[1,1]])

# weights and bias initialization
hidden_weights=np.array([[0.51426693,0.56885825],[0.48725347,0.15041493]])
hidden_bias=np.array([0.79726405,0.67601843])
output_weights=np.array([[0.14801747],[0.37182892]])
output_bias=0.57823076

def fs(fp):
    with open(fp) as f:
        bs = f.read()
    return bs

import apple

code=apple.jit('''
    λX.λwh.λbh.
      { sigmoid ← [1%(1+ℯ(_x))]
      ; sigmoid`{0} ([(+)`bh x]'(X%.wh))
      }
''')
hidden_layer_output=apple.f(code,inputs,hidden_weights,hidden_bias)
print(hidden_layer_output)

output_layer_activation = np.dot(hidden_layer_output,output_weights)
output_layer_activation += output_bias
predicted_output = sigmoid(output_layer_activation)
print(predicted_output)

#  src=fs('test/data/prediction.🍎')
#  p=apple.jit(src)
#  predicted_output=apple.f(p,hidden_weights,output_weights.reshape([2]),hidden_bias,output_bias)
#  print(predicted_output)
