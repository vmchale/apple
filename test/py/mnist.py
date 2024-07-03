#  import numpy as np
#  import tensorflow_datasets as tfds

#  train = tfds.load('mnist', split='train', as_supervised=True, batch_size=-1)
#  train_image_data, train_labels = tfds.as_numpy(train)
#  train_images = train_image_data.astype('float64')/255

#  test = tfds.load('mnist', split='test', as_supervised=True, batch_size=-1)
#  test_image_data, test_labels = tfds.as_numpy(test)
#  test_images = test_image_data.astype('float64')/255

import numpy as np

def init(x,y):
    return np.random.uniform(-1.,1.,(x,y))/np.sqrt(x*y)

np.random.seed(17)

l1=init(28*28,128)
l2=init(128,10)

import apple

vize=apple.jit("λn.[?x=n,.1::float,.0]'irange 0 9 1")
print(apple.f(vize,4))

# safe softmax
softmax=apple.jit('''
λxs.
  { m ⟜ (⋉)/xs; a ⟜ [e:(x-m)]'xs
  ; sum ← [(+)/x]
  ; n ⟜ sum a
  ; (%n)'a
  }
''')
