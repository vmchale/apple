import numpy as np

def sigmoid (x):
    return 1/(1+np.exp(-x))

inputs = np.array([[0.,0],[0,1],[1,0],[1,1]])

hidden_weights=np.array([[0.51426693,0.56885825],[0.48725347,0.15041493]])
hidden_bias=np.array([0.79726405,0.67601843])
output_weights=np.array([0.14801747,0.37182892])
output_bias=0.57823076

import apple

fw=apple.jit("λX.λwh.λbh. [⅟(1+ℯ(_x))]`{0} ([(+)`bh x]'(X%.wh))")
hidden_layer_output=fw(inputs,hidden_weights,hidden_bias)
print(hidden_layer_output)

def forward(inputs,hidden_weights,hidden_bias):
    hidden_layer_activation = np.dot(inputs,hidden_weights)
    hidden_layer_activation += hidden_bias
    hidden_layer_output = sigmoid(hidden_layer_activation)
    return hidden_layer_output

print(forward(inputs,hidden_weights,hidden_bias))

def prediction(hidden_layer_output,output_weights,output_bias):
    output_layer_activation = np.dot(hidden_layer_output,output_weights.reshape(2,1))
    output_layer_activation += output_bias
    predicted_output = sigmoid(output_layer_activation)
    return predicted_output.reshape(4)

print(prediction(hidden_layer_output,output_weights,output_bias))

o=apple.jit("λho.λwo.λbo. [1%(1+ℯ(_x))]'((+bo)'(ho%:wo))")
print(o(hidden_layer_output,output_weights,output_bias))
