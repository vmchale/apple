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

ssoftmax=apple.jit('''
λxs.
  { m ⟜ (⋉)/* _1 xs; a ⟜ [e:(x-m)]`{0} xs
  ; sum ← [(+)/x]
  ; n ⟜ sum`{1} (a::M float)
  ; ⍉([(%x)'y]`{0,1} n a)
  }
''')

vsigmoid=apple.jit("([1%(1+ℯ(_x))]`{0})")
def mul(M,N,K):
    return apple.jit(f"[(x::Arr ({M}×{N}) float)%.(y::Arr ({N}×{K}) float)]")
ml1=mul(60000,784,128)
ml2=mul(60000,128,10)

train_labels_v=apple.jit("((λn.[?x=n,.1::float,.0]'irange 0 9 1)')")(train_labels)

def fw(l1,l2,x):
    x_l1p=ml1(x,l1)
    x_sigmoid=vsigmoid(x_l1p)
    x_l2p=ml2(x_sigmoid,l2)
    out=ssoftmax(x_l2p)
    return x_l1p,x_sigmoid,x_l2p,out

errorjit=apple.jit('''
λout.λtargets.λxl2p.
{
  dsoftmax ← λxs.
    { m ⟜ (⋉)/* _1 xs; a ⟜ [e:(x-m)]`{0} xs
    ; sum ← [(+)/x]
    ; n ⟜ sum`{1} (a::M float)
    ; [x*(1-x)]`{0} (⍉([(%x)'y]`{0,1} n a))
    };
  (*)`{0,0} ({n⟜2%(ℝ(𝓉out)); [x*n]`{0} ((-)`{0,0} out targets)}) (dsoftmax xl2p)
}
''')
u_l2jit=apple.jit('[(|:(x::Arr (60000×128) float))%.y]')
u_l1jit=apple.jit('''
λx.λl2.λerror.λxl1p.
{
  dsigmoid ← ((λx.⸎x⟜ℯ(_x);x%(1+x)^2)`{0});
  (⍉(x::Arr (60000×784) float))%.((*)`{0,0} (⍉((l2::Arr (128×10) float)%.(⍉(error::Arr (60000×10) float)))) (dsigmoid xl1p))
}
''')

train=apple.jit('''
-- x: 60000x784
-- targets: 60000x10
λx.λtargets.
λl1.λl2.
  {
    softmax ← λxs.
      { m ⟜ (⋉)/* _1 xs; a ⟜ [e:(x-m)]`{0} xs
      ; sum ← [(+)/x]
      ; n ⟜ sum`{1} (a::M float)
      ; ⍉([(%x)'y]`{0,1} n a)
      };
    dsoftmax ← λxs.
        { m ⟜ (⋉)/* _1 xs; a ⟜ [e:(x-m)]`{0} xs
        ; sum ← [(+)/x]
        ; n ⟜ sum`{1} (a::M float)
        ; ⍉([x*(1-x)]`{0} ([(%x)'y]`{0,1} n a))
        };
    dsigmoid ← ((λx.⸎x⟜ℯ(_x);x%(1+x)^2)`{0});
    -- fw
    xl1p ⟜ x%.l1;
    xSigmoid ← [1%(1+ℯ(_x))]`{0} xl1p;
    xl2p ⟜ xSigmoid%.l2;
    out ← softmax xl2p;
    -- bw
    error ⟜ (*)`{0,0} ({n⟜2%(ℝ(𝓉out)); [x*n]`{0} ((-)`{0,0} out targets)}) (dsoftmax xl2p);
    ul2 ← (⍉xSigmoid)%.error;
    ul1 ← (⍉x)%.((*)`{0,0} (⍉(l2%.(⍉error))) (dsigmoid xl1p));
    ((+)`{0,0} l1 ul1, (+)`{0,0} l2 ul2)
  }''')

update_l1,update_l2=train(train_images_v,train_labels_v,l1,l2)
print('update_l2\n',update_l2)

def fw_bw(x,targets):

    x_l1p,x_sigmoid,x_l2p,out=fw(l1,l2,x)

    error=errorjit(out,targets,x_l2p)

    update_l2=u_l2jit(x_sigmoid,error)
    update_l1=u_l1jit(x,l2,error,x_l1p)

    return out,update_l1,update_l2

out, update_l1, update_l2 = fw_bw(train_images_v,train_labels_v)
print('out\n',out)
print('update_l2\n',update_l2)
