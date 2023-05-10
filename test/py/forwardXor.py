# from https://towardsdatascience.com/implementing-the-xor-gate-using-backpropagation-in-neural-networks-c1f255b4f20d

import numpy as np

def sigmoid (x):
    return 1/(1+np.exp(-x))

inputs = np.array([[0,0],[0,1],[1,0],[1,1]])

# weights and bias initialization
hidden_weights=np.array([[0.51426693,0.56885825],[0.48725347,0.15041493]])
hidden_bias=np.array([0.79726405,0.67601843])
output_weights=np.array([[0.14801747],[0.37182892]])
output_bias=0.57823076
print(hidden_weights)
print(output_weights)
print(hidden_bias)
print(output_bias)

#Forward Propagation
hidden_layer_activation = np.dot(inputs,hidden_weights)
hidden_layer_activation += hidden_bias
hidden_layer_output = sigmoid(hidden_layer_activation)
print(hidden_layer_output)

output_layer_activation = np.dot(hidden_layer_output,output_weights)
output_layer_activation += output_bias
predicted_output = sigmoid(output_layer_activation)
print(predicted_output)
