module Asm.M ( CFunc (..)
             , WM
             , Label
             , pCFunc
             , nI
             , nL
             , foldMapA
             , prettyAsm
             , aArr, mFree
             ) where

import           Control.DeepSeq                  (NFData (rnf), rwhnf)
import           Control.Monad.Trans.State.Strict (State, state)
import           Data.Foldable                    (fold, traverse_)
import qualified Data.IntMap                      as IM
import           Data.List                        (scanl')
import qualified Data.Text                        as T
import           Data.Word                        (Word8)
import           Foreign.Marshal.Alloc            (free)
import           Foreign.Marshal.Array            (mallocArray, pokeArray)
import           Foreign.Ptr                      (Ptr, plusPtr)
import qualified IR
import           Prettyprinter                    (Doc, Pretty (pretty))
import           Prettyprinter.Ext

type WM = State IR.WSt

type Label = Word

foldMapA :: (Applicative f, Traversable t, Monoid m) => (a -> f m) -> t a -> f m
foldMapA = (fmap fold .) . traverse

pAsm :: Pretty isn => [isn] -> Doc ann
pAsm = prettyLines.fmap pretty

prettyAsm :: (Pretty isn) => (IR.AsmData, [isn]) -> Doc ann
prettyAsm (ds,is) = pAD ds <#> pAsm is

nI :: WM Int
nI = state (\(IR.WSt l i) -> (i, IR.WSt l (i+1)))

nL :: WM Label
nL = state (\(IR.WSt i t) -> (i, IR.WSt (i+1) t))

data CFunc = Malloc | Free | JR | DR | Exp | Log | Pow

instance NFData CFunc where rnf=rwhnf

pCFunc :: CFunc -> T.Text
pCFunc Malloc="malloc"; pCFunc Free="free"
pCFunc JR="lrand48";    pCFunc DR="drand48"
pCFunc Exp="exp"; pCFunc Log="log"; pCFunc Pow="pow"

instance Pretty CFunc where pretty=pretty.pCFunc

mFree :: Maybe (Ptr a) -> IO ()
mFree = traverse_ free

aArr :: IM.IntMap [Word8] -> IO (IM.IntMap (Ptr Word8))
aArr as = do
    let bls = fmap length as; bl = sum bls
    p <- mallocArray bl
    let bs = concat (IM.elems as)
    pokeArray p bs
    pure $ case IM.toList bls of
        []             -> IM.empty
        -- TODO: fromAscList? idk
        ((k0,l0):bls') -> IM.fromList . fmap (\(x,_,z) -> (x,z)) $ scanl' (\(_, lϵ, pϵ) (k, l) -> (k, l, pϵ `plusPtr` lϵ)) (k0, l0, p) bls'
