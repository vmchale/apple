import numpy as np
inputs = np.array([[0.,0],[0,1],[1,0],[1,1]])

# weights and bias initialization
hidden_weights=np.array([[0.51426693,0.56885825],[0.48725347,0.15041493]])
hidden_bias=np.array([0.79726405,0.67601843])
output_weights=np.array([0.14801747,0.37182892])
output_bias=0.57823076

def fs(fp):
    with open(fp) as f:
        bs = f.read()
    return bs

import apple

ho=fs('test/data/hiddenOutput.🍏')
code=apple.jit(ho)
hidden_layer_output=apple.f(code,inputs,hidden_weights,hidden_bias)
print(hidden_layer_output)

src=fs('test/data/prediction.🍎')
p=apple.jit(src)
predicted_output=apple.f(p,hidden_weights,output_weights,hidden_bias,output_bias)
print(predicted_output)
