{-# LANGUAGE FlexibleInstances #-}

module IR ( Exp (..), FExp (..), FE, F2E
          , Stmt (..)
          , Temp (..), FTemp (..), F2 (..)
          , Label, AsmData
          , AE (..)
          , WSt (..)
          , prettyIR
          ) where

import           CF.AL
import           Data.Int          (Int64)
import qualified Data.IntMap       as IM
import           Data.Void         (Void)
import           Data.Word         (Word64)
import           Op
import           Prettyprinter     (Doc, Pretty (..), hardline, parens, (<+>))
import           Prettyprinter.Ext

-- see https://my.eng.utah.edu/~cs4400/sse-fp.pdf
type Label = Word; type AsmData = IM.IntMap [Word64]

data WSt = WSt { wlabel :: !Label, wtemps :: !Int }

prettyLabel :: Label -> Doc ann
prettyLabel l = "apple_" <> pretty l

data FTemp = FTemp !Int
           | F0 | F1 | F2 | F3 | F4 | F5
           | FRet | FRet1
           deriving (Eq, Ord)

newtype F2 = F2Temp { f2ToInt :: Int } deriving (Eq, Ord)

instance Pretty F2 where pretty (F2Temp i) = "f2_" <> pretty i

data Temp = ITemp !Int | ATemp !Int
          | C0 | C1 | C2 | C3 | C4 | C5
          | CRet
          deriving Eq

instance Pretty Temp where
    pretty (ITemp i) = "r_" <> pretty i
    pretty (ATemp i) = "a_" <> pretty i
    pretty C0        = "r_arg0"
    pretty C1        = "r_arg1"
    pretty C2        = "r_arg2"
    pretty C3        = "r_arg3"
    pretty C4        = "r_arg4"
    pretty C5        = "r_arg5"
    pretty CRet      = "r_ret"

instance Pretty FTemp where
    pretty (FTemp i) = "f_" <> pretty i
    pretty F0        = "f_arg0"
    pretty F1        = "f_arg1"
    pretty F2        = "f_arg2"
    pretty F3        = "f_arg3"
    pretty F4        = "f_arg4"
    pretty F5        = "f_arg5"
    pretty FRet      = "f_ret"
    pretty FRet1     = "f_ret1"

instance Show Temp where show=show.pretty

data Stmt = L Label
          | MJ Exp Label
          | J Label
          | MT Temp Exp | MX FTemp FE -- move targeting xmm0 &c.
          | MX2 F2 F2E
          -- happd on x86
          | S2 FBin FTemp F2
          | Fill2 F2 FTemp
          | Ma AL Temp Exp -- label, register, size
          | Free Temp | RA !AL -- "return array" no-op
          | Wr AE Exp | WrF AE FE | WrF2 AE F2E | WrB AE Exp
          | Cmov Exp Temp Exp | Fcmov Exp FTemp FE
          | Cset Temp Exp
          | Sa8 Temp Exp
          | Pop8 Exp
          | Sa Temp Exp -- register, size
          | Pop Exp -- pop salloc
          | Cpy AE AE Exp
          | Cpy1 AE AE Exp
          | C Label | R Label
          | IRnd Temp | FRnd FTemp

instance Pretty Stmt where
    pretty (L l)         = hardline <> prettyLabel l <> ":"
    pretty (MT t e)      = parens ("movtemp" <+> pretty t <+> pretty e)
    pretty (MX t e)      = parens ("movf" <+> pretty t <+> pretty e)
    pretty (MX2 t e)     = parens ("movf2" <+> pretty t <+> pretty e)
    pretty (MJ e l)      = parens ("mjump" <+> pretty e <+> prettyLabel l)
    pretty (J l)         = parens ("j" <+> prettyLabel l)
    pretty (Wr p e)      = parens ("write" <+> pretty p <+> pretty e)
    pretty (WrF p e)     = parens ("write" <+> pretty p <+> pretty e)
    pretty (WrB p e)     = parens ("write-1" <+> pretty p <+> pretty e)
    pretty (WrF2 p e)    = parens ("write2" <+> pretty p <+> pretty e)
    pretty (Ma _ t e)    = parens ("malloc" <+> pretty t <+> ":" <+> pretty e)
    pretty (Free t)      = parens ("free" <+> pretty t)
    pretty (Cmov p t e)  = parens ("cmov" <+> pretty p <+> pretty t <+> pretty e)
    pretty (Fcmov p t e) = parens ("fcmov" <+> pretty p <+> pretty t <+> pretty e)
    pretty RA{}          = parens "return-array"
    pretty (Sa t e)      = parens ("salloc" <+> pretty t <+> ":" <+> pretty e)
    pretty (Pop e)       = parens ("spop" <+> pretty e)
    pretty (Sa8 t e)     = parens ("salloc" <+> pretty t <+> ":" <+> pretty e)
    pretty (Pop8 e)      = parens ("spop" <+> pretty e)
    pretty (Cpy p p' e)  = parens ("cpy" <+> pretty p <> "," <+> pretty p' <+> pretty e)
    pretty (Cpy1 p p' e) = parens ("cpy-byte" <+> pretty p <> "," <+> pretty p' <+> pretty e)
    pretty (C l)         = parens ("call" <+> prettyLabel l)
    pretty R{}           = parens "ret" <> hardline
    pretty (IRnd t)      = parens (pretty t <+> "<- rnd")
    pretty (FRnd t)      = parens (pretty t <+> "<- xrnd")
    pretty (Cset t e)    = parens ("cset" <+> pretty t <+> "<-" <+> pretty e)
    pretty (S2 o t r)    = parens ("split" <> pretty o <+> pretty t <+> pretty r)
    pretty (Fill2 r t)   = parens ("fill" <+> pretty r <> "," <+> pretty t)

instance Show Stmt where show = show . pretty

data AE = AP Temp (Maybe Exp) (Maybe AL) -- offset, label for tracking liveness

instance Pretty AE where
    pretty (AP t Nothing _)  = parens ("ptr" <+> pretty t)
    pretty (AP t (Just e) _) = parens ("ptr" <+> pretty t <> "+" <> pretty e)

data FExp t c e = ConstF c
                | FB FBin (FExp t c e) (FExp t c e)
                | FConv e
                | FReg t
                | FU FUn (FExp t c e)
                | FAt AE

type FE=FExp FTemp Double Exp; type F2E=FExp F2 (Double, Double) Void

instance Num Exp where
    (+) = IB IPlus; (*) = IB ITimes; (-) = IB IMinus; fromInteger = ConstI . fromInteger

instance Num (FExp ftemp Double e) where
    (+) = FB FPlus; (*) = FB FTimes; (-) = FB FMinus; fromInteger = ConstF . fromInteger

instance Fractional (FExp ftemp Double e) where
    (/) = FB FDiv; fromRational = ConstF . fromRational

data Exp = ConstI Int64
         | Reg Temp
         | IB IBin Exp Exp
         | FRel FRel FE FE
         | IRel IRel Exp Exp | Is Temp
         | IU IUn Exp
         | BU BUn Exp
         | IRFloor FE
         | EAt AE | BAt AE
         | LA !Int -- assembler data

instance (Pretty t, Pretty c, Pretty e) => Pretty (FExp t c e) where
    pretty (ConstF x)   = parens ("double" <+> pretty x)
    pretty (FConv e)    = parens ("itof" <+> pretty e)
    pretty (FReg t)     = parens ("freg" <+> pretty t)
    pretty (FB op e e') = parens (pretty op <+> pretty e <+> pretty e')
    pretty (FU op e)    = parens (pretty op <+> pretty e)
    pretty (FAt p)      = "f@" <> pretty p

instance (Pretty t, Pretty c, Pretty e) => Show (FExp t c e) where show=show.pretty

instance Pretty Exp where
    pretty (ConstI i)     = parens ("int" <+> pretty i)
    pretty (Reg t)        = parens ("reg" <+> pretty t)
    pretty (IRel op e e') = parens (pretty op <+> pretty e <+> pretty e')
    pretty (IB op e e')   = parens (pretty op <+> pretty e <+> pretty e')
    pretty (IU op e)      = parens (pretty op <+> pretty e)
    pretty (BU op e)      = pretty op <> pretty e
    pretty (IRFloor e)    = parens ("floor" <+> pretty e)
    pretty (EAt p)        = "@" <> pretty p
    pretty (BAt p)        = "b@" <> pretty p
    pretty (FRel op e e') = parens (pretty op <+> pretty e <+> pretty e')
    pretty (Is e)         = parens ("is?" <+> pretty e)
    pretty (LA n)         = "arr_" <> pretty n

instance Show Exp where show = show.pretty

prettyIR :: (AsmData, [Stmt]) -> Doc ann
prettyIR (ds,ss) = pAD ds <#> prettyLines (pretty<$>ss)
