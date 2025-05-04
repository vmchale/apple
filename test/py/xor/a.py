import numpy as np

inputs = np.array([[0.,0],[0,1],[1,0],[1,1]])

hidden_weights=np.array([[0.51426693,0.56885825],[0.48725347,0.15041493]])
hidden_bias=np.array([0.79726405,0.67601843])
output_weights=np.array([0.14801747,0.37182892])
output_bias=0.57823076

import apple

train=apple.jit('''
λwh.λwo.λbh.λbo.
{ X ⟜ ⟨⟨0,0⟩,⟨0,1⟩,⟨1,0⟩,⟨1,1⟩⟩;
  Y ← ⟨0,1,1,0⟩;
  sigmoid ← [⅟(1+e:(_x))];
  sDdx ← [x*(1-x)];
  sum ⇐ [(+)/x];
  -- ho: 4x2
  -- prediction: 4
  ho ⟜ sigmoid`{0} ([(+)`(bh::Vec 2 float) x]'X%.wh);
  prediction ⟜ sigmoid ∴ (+bo)'ho%:wo;
  l1E ← (-)`Y prediction;
  l1Δ ⟜ (*)`(sDdx'prediction) l1E; -- 4
  he ← l1Δ (*)⊗ wo; -- 4x2
  hΔ ⟜ (*)`{0,0} (sDdx`{0} ho) he; -- 4x2
  wha ← (+)`{0,0} wh (|:X%.hΔ);
  woa ← (+)`wo (|:ho%:l1Δ);
  bha ← [(+)/ₒ x y]`{0,1} bh hΔ;
  boa ← bo + sum l1Δ;
  (wha,woa,bha,boa)
}
''')

hidden_weights,output_weights,hidden_bias,output_bias=train(hidden_weights,output_weights,hidden_bias,output_bias)
