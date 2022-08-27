module CGen ( irTy
            ) where

import           A
import           Control.Exception (Exception)
import           Data.Bifunctor    (first)
import qualified Data.Text         as T

data CType = CDouble | CLong -- todo: int64_t?

data CF = CF !T.Text [CType] CType

-- type translation error
data TTE = HigherOrder | Poly deriving Show

instance Exception TTE where

irTy :: T a -> Either TTE ([T a], T a)
irTy F                 = pure ([], F)
irTy I                 = pure ([], I)
irTy B                 = pure ([], B)
irTy t@Arr{}           = pure ([], t)
irTy (Arrow Arrow{} _) = Left HigherOrder
irTy (Arrow t0 t1)     = first (t0:) <$> irTy t1
irTy TVar{}            = Left Poly
