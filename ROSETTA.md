# XOR Neural Network

## Forward Propagation

```j
X =: 4 2 $ 0 0  0 1  1 0  1 1

mp =: +/ . *
sigmoid =: monad define
    % 1 + ^ - y
)

forward =: dyad define
    'WH WO BH BO' =. x
    hidden_layer_output =. sigmoid (BH +"1 X (mp "1 2) WH)
    prediction =. sigmoid (BO + WO mp"1 hidden_layer_output)
    (hidden_layer_output;prediction)
)
```

```
λwh.λwo.λbh.λbo.
  { X ⟜ ⟨⟨0,0⟩,⟨0,1⟩,⟨1,0⟩,⟨1,1⟩⟩
  ; sigmoid ← [1%(1+ℯ(_x))]
  ; ho ← sigmoid`{0} ([(+)`bh x]'(X%.wh))
  ; prediction ← sigmoid'((+bo)'(ho%:wo))
  ; (ho,prediction)
  }
```

```python
# https://towardsdatascience.com/implementing-the-xor-gate-using-backpropagation-in-neural-networks-c1f255b4f20d

import numpy as np

def sigmoid (x):
    return 1/(1+np.exp(-x))

inputs = np.array([[0,0],[0,1],[1,0],[1,1]])

hidden_layer_activation = np.dot(inputs,hidden_weights)
hidden_layer_activation += hidden_bias
hidden_layer_output = sigmoid(hidden_layer_activation)

output_layer_activation = np.dot(hidden_layer_output,output_weights)
output_layer_activation += output_bias
predicted_output = sigmoid(output_layer_activation)
```
