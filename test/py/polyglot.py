# from https://towardsdatascience.com/implementing-the-xor-gate-using-backpropagation-in-neural-networks-c1f255b4f20d

import numpy as np

def sigmoid (x):
    return 1/(1+np.exp(-x))

inputs = np.array([[0,0],[0,1],[1,0],[1,1]])

# weights and bias initialization
hidden_weights=np.array([[0.51426693,0.56885825],[0.48725347,0.15041493]])
hidden_bias=np.array([0.79726405,0.67601843])
output_weights=np.array([0.14801747,0.37182892])
output_bias=0.57823076
print(hidden_weights)
print(output_weights)
print(hidden_bias)
print(output_bias)

import apple

def fs(fp):
    with open(fp) as f:
        bs = f.read()
    return bs

ho=fs('test/data/hiddenOutput.🍎')
# segfaults creating cif
code=apple.jit(ho)
hidden_layer_output=apple.f(code,hidden_weights,hidden_bias)
print(hidden_layer_output)

src=fs('test/data/prediction.🍎')
p=apple.jit(src)
predicted_output=apple.f(p,hidden_weights,output_weights,hidden_bias,output_bias)
print(predicted_output)
