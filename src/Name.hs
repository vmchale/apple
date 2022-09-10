{-# LANGUAGE DeriveFunctor #-}

module Name ( Name (..)
            , TyName
            ) where

import           Control.DeepSeq (NFData (..))
import qualified Data.Text       as T
import           Prettyprinter   (Pretty (..))
import           U

type TyName a = Name a

data Name a = Name { name   :: T.Text
                   , unique :: !U
                   , loc    :: a
                   } deriving (Functor)

instance Eq (Name a) where
    (==) (Name _ u _) (Name _ u' _) = u == u'

instance Ord (Name a) where
    compare (Name _ u _) (Name _ u' _) = compare u u'

-- TODO: prettyprinter library?
instance Pretty (Name a) where
    pretty (Name n _ _) = pretty n
    -- pretty (Name n (U i) _) = pretty n <> pretty i

instance Show (Name a) where
    show = show . pretty

instance NFData a => NFData (Name a) where
    rnf (Name _ u x) = rnf x `seq` u `seq` ()
