import numpy as np

def sigmoid (x):
    return 1/(1+np.exp(-x))

def sigmoid_derivative(x):
    return x*(1-x)

inputs = np.array([[0.,0],[0,1],[1,0],[1,1]])
out=np.array([0.,1,1,0])
expected_output=out.reshape(4,1)

hidden_weights=np.array([[0.51426693,0.56885825],[0.48725347,0.15041493]])
hidden_bias=np.array([0.79726405,0.67601843])
ow=np.array([0.14801747,0.37182892])
output_weights=ow.reshape(2,1)
output_bias=0.57823076

import apple

fw=apple.jit("λX.λwh.λbh. [⅟(1+ℯ(_x))]`{0} ([(+)`bh x]'(X%.wh))")
hl=fw(inputs,hidden_weights,hidden_bias)
print(hl)

def forward(inputs,hidden_weights,hidden_bias):
    hidden_layer_activation = np.dot(inputs,hidden_weights)
    hidden_layer_activation += hidden_bias
    hidden_layer_output = sigmoid(hidden_layer_activation)
    return hidden_layer_output

print(forward(inputs,hidden_weights,hidden_bias))

def prediction(hidden_layer_output,output_weights,output_bias):
    output_layer_activation = np.dot(hidden_layer_output,output_weights)
    output_layer_activation += output_bias
    predicted_output = sigmoid(output_layer_activation)
    return predicted_output

predicted_output=prediction(hl,output_weights,output_bias)
print(predicted_output)

o=apple.jit("λho.λwo.λbo. [1%(1+ℯ(_x))]'((+bo)'(ho%:wo))")
po=o(hl,ow,output_bias)
print(po)
