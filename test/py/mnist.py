import apple
import numpy as np

train_images=np.load('nb/data/train-images.npy')
train_labels=np.load('nb/data/train-labels.npy')

train_images_v=apple.jit("[♭`{3∘[2,3,4]} (x::Arr (60000 × 28 × 28 × 1) float)]")(train_images)

def init(x,y):
    return np.random.uniform(-1.,1.,(x,y))/np.sqrt(x*y)

np.random.seed(17)

l1=init(28*28,128)
l2=init(128,10)

train_labels_v=apple.jit("((λn.[?x=n,.1::float,.0]'⍳9)')")(train_labels)

train=apple.jit('''
-- x: 60000x784
-- targets: 60000x10
λx.λtargets.
λl1.λl2.
  {
    dsigmoid ← ((λx.⸎x⟜e:(_x);x%(1+x)^2)`{0});
    -- fw
    xl1p ⟜ x%.l1;
    xSigmoid ← [1%(1+e:(_x))]`{0} xl1p;
    xl2p ⟜ xSigmoid%.l2;
    m ⟜ (⋉)/* _1 xl2p; a ⟜ [e:(x-m)]`{0} xl2p;
    sum ← [(+)/x];
    n ← sum`{1} (a::M𝞈);
    out ⟜ ⍉([(%x)'y]`{0,1} n a);
    dsoftmax_l2 ← [x*(1-x)]`{0} out;
    -- bw
    error ⟜ (*)`{0,0} ({n⟜2%(ℝ(𝓉out)); [x*n]`{0} ((-)`{0,0} out targets)}) dsoftmax_l2;
    ul2 ← ⍉xSigmoid%.error;
    ul1 ← ⍉x%.((*)`{0,0} (⍉(l2%.⍉error)) (dsigmoid xl1p));
    ((+)`{0,0} l1 ul1, (+)`{0,0} l2 ul2)
  }''')

update_l1,update_l2=train(train_images_v,train_labels_v,l1,l2)
print('update_l1\n',update_l1)
print('update_l2\n',update_l2)
