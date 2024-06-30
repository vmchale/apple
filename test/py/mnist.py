#  import numpy as np
#  import tensorflow_datasets as tfds

#  train = tfds.load('mnist', split='train', as_supervised=True, batch_size=-1)
#  train_image_data, train_labels = tfds.as_numpy(train)
#  train_images = train_image_data.astype('float64')/255

#  test = tfds.load('mnist', split='test', as_supervised=True, batch_size=-1)
#  test_image_data, test_labels = tfds.as_numpy(test)
#  test_images = test_image_data.astype('float64')/255

import apple
import numpy as np

vize=apple.jit("λn.[?x=n,.1::float,.0]'irange 0 9 1")
print(apple.f(vize,4))

ssoftmax=apple.jit('''
λxs.
  { m ⟜ (⋉)/xs; a ⟜ [e:(x-m)]'xs
  ; sum ← [(+)/x]
  ; n ⟜ sum a
  ; (%n)'a
  }
''')
