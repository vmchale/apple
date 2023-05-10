import numpy as np

def sigmoid (x):
    return 1/(1+np.exp(-x))

def sigmoid_derivative(x):
    return x*(1 - x)

inputs = np.array([[0,0],[0,1],[1,0],[1,1]])
expected_output = np.array([[0],[1],[1],[0]])

#weights and bias initialization
hidden_weights=np.array([[0.51426693,0.56885825],[0.48725347,0.15041493]])
hidden_bias=np.array([[0.79726405,0.67601843]])
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

#Backpropagation
error = expected_output - predicted_output
d_predicted_output = error * sigmoid_derivative(predicted_output)

error_hidden_layer = d_predicted_output.dot(output_weights.T)
d_hidden_layer = error_hidden_layer * sigmoid_derivative(hidden_layer_output)

#Updating Weights and Biases
output_weights += hidden_layer_output.T.dot(d_predicted_output)
output_bias += np.sum(d_predicted_output,axis=0,keepdims=True)
hidden_weights += inputs.T.dot(d_hidden_layer)
hidden_bias += np.sum(d_hidden_layer,axis=0,keepdims=True)
