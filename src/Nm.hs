{-# LANGUAGE DeriveFunctor #-}

module Nm ( Nm (..)
          , TyNm
          ) where

import           Control.DeepSeq (NFData (..))
import qualified Data.Text       as T
import           Prettyprinter   (Pretty (..))
import           U

type TyNm a = Nm a

data Nm a = Nm { name   :: T.Text
               , unique :: !U
               , loc    :: a
               } deriving (Functor)

instance Eq (Nm a) where
    (==) (Nm _ u _) (Nm _ u' _) = u == u'

instance Ord (Nm a) where
    compare (Nm _ u _) (Nm _ u' _) = compare u u'

instance Pretty (Nm a) where
    pretty (Nm n _ _) = pretty n
    -- pretty (Nm n (U i) _) = pretty n <> pretty i

instance Show (Nm a) where
    show = show . pretty

instance NFData a => NFData (Nm a) where
    rnf (Nm _ u x) = rnf x `seq` u `seq` ()
