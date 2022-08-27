{-# LANGUAGE OverloadedStrings #-}

module C.FP ( cArr ) where

import qualified Data.ByteString as BS
import qualified Data.Text       as T
import           Prettyprinter   (Doc)

cArr :: T.Text -> BS.ByteString -> Doc ann
cArr f = "char " <> f <> undefined
 -- int my_array[3][3] ={10, 23, 42, 1, 654, 0, 40652, 22, 0};
