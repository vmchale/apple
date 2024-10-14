import n as n
import a as a

assert (a.hidden_weights==n.hidden_weights).all()
print(n.hidden_bias.reshape(2))
print(a.hidden_bias)
assert n.output_bias[0][0]==a.output_bias
