{-# LANGUAGE OverloadedStrings #-}

module IR ( Exp (..)
          , FExp (..)
          , Stmt (..)
          , Temp (..)
          , FBin (..)
          , FUn (..)
          , AE (..)
          , IBin (..)
          , IUn (..)
          , IRel (..)
          , Label
          , WSt (..)
          , prettyIR
          ) where

import           Data.Int          (Int64)
import           Data.Semigroup    ((<>))
import           Prettyprinter     (Doc, Pretty (..), hardline, parens, (<+>))
import           Prettyprinter.Ext

-- see https://my.eng.utah.edu/~cs4400/sse-fp.pdf

type Label = Word

data WSt = WSt { wlabels :: [Label], wtemps :: [Int] }

prettyLabel :: Label -> Doc ann
prettyLabel l = "apple_" <> pretty l

data Temp = ITemp !Int
          | ATemp !Int
          | C0 | C1 | C2 | C3 | C4 | C5
          | CRet
          | FTemp !Int
          -- 512-bit
          | F8Temp !Int -- ZMM0-ZMM31
          | F0 | F1 | F2 | F3 | F4 | F5
          | FRet | FRet1
          | StackPointer
          deriving (Eq)

instance Pretty Temp where
    pretty (ITemp i)    = "r_" <> pretty i
    pretty (ATemp i)    = "a_" <> pretty i
    pretty C0           = "r_arg0"
    pretty C1           = "r_arg1"
    pretty C2           = "r_arg2"
    pretty C3           = "r_arg3"
    pretty C4           = "r_arg4"
    pretty C5           = "r_arg5"
    pretty CRet         = "r_ret"
    pretty (FTemp i)    = "f_" <> pretty i
    pretty F0           = "f_arg0"
    pretty F1           = "f_arg1"
    pretty F2           = "f_arg2"
    pretty F3           = "f_arg3"
    pretty F4           = "f_arg4"
    pretty F5           = "f_arg5"
    pretty FRet         = "f_ret"
    pretty FRet1        = "f_ret1"
    pretty StackPointer = "stack_pointer"

instance Show Temp where show = show . pretty

data Stmt = L Label
          | MJ Exp Label
          | J Label
          | MT Temp Exp
          | MX Temp FExp -- move targeting xmm0, etc.
          | Ma Temp Exp -- size
          | Free Temp
          | Wr AE Exp
          | WrF AE FExp
          | Cmov Exp Temp Exp
          -- TODO: ccall?

instance Pretty Stmt where
    pretty (L l)        = hardline <> prettyLabel l <> ":"
    pretty (MT t e)     = parens ("movtemp" <+> pretty t <+> pretty e)
    pretty (MX t e)     = parens ("movf" <+> pretty t <+> pretty e)
    pretty (MJ e l)     = parens ("mjump" <+> pretty e <+> prettyLabel l)
    pretty (J l)        = parens ("j" <+> prettyLabel l)
    pretty (Wr p e)     = parens ("write" <+> pretty p <+> pretty e)
    pretty (WrF p e)    = parens ("write" <+> pretty p <+> pretty e)
    pretty (Ma t e)     = parens ("malloc" <+> pretty t <+> ":" <+> pretty e)
    pretty (Cmov p t e) = parens ("cmov" <+> pretty p <+> pretty t <+> pretty e)

instance Show Stmt where show = show . pretty

data AE = AP Temp (Maybe Exp) (Maybe Int) -- offset, label for tracking liveness

instance Pretty AE where
    pretty (AP t Nothing _)  = parens ("ptr" <+> pretty t)
    pretty (AP t (Just e) _) = parens ("ptr" <+> pretty t <> "+" <> pretty e)

data FExp = ConstF Double
          | FB FBin FExp FExp
          | FConv Exp
          | FReg Temp
          | FU FUn FExp
          | FAt AE

data Exp = ConstI Int64
         | Reg Temp
         | IB IBin Exp Exp
         | IRel IRel Exp Exp
         | IU IUn Exp
         | IRFloor FExp
         | EAt AE

instance Pretty FExp where
    pretty (ConstF x)   = parens ("double" <+> pretty x)
    pretty (FConv e)    = parens ("itof" <+> pretty e)
    pretty (FReg t)     = parens ("freg" <+> pretty t)
    pretty (FB op e e') = parens (pretty op <+> pretty e <+> pretty e')
    pretty (FU op e)    = parens (pretty op <+> pretty e)
    pretty (FAt p)      = "f@" <> pretty p

instance Show FExp where show=show.pretty

instance Pretty Exp where
    pretty (ConstI i)     = parens ("int" <+> pretty i)
    pretty (Reg t)        = parens ("reg" <+> pretty t)
    pretty (IRel op e e') = parens (pretty op <+> pretty e <+> pretty e')
    pretty (IB op e e')   = parens (pretty op <+> pretty e <+> pretty e')
    pretty (IU op e)      = parens (pretty op <+> pretty e)
    pretty (IRFloor e)    = parens ("floor" <+> pretty e)
    pretty (EAt p)        = "@" <> pretty p

instance Show Exp where show = show.pretty

data FUn = FSqrt | FLog

data IUn = ISgn | INot

data FBin = FPlus | FMinus | FTimes | FDiv | FMax | FMin | FExp

data IBin = IPlus | IMinus | ITimes | IAsr | IAnd | IMax | IMin | IDiv | IAsl

data IRel = IEq | INeq | IGt | ILt | ILeq | IGeq

instance Pretty IRel where
    pretty IEq  = "="
    pretty INeq = "!="
    pretty IGt  = ">"
    pretty ILt  = "<"
    pretty ILeq = "≤"
    pretty IGeq = "≥"

instance Pretty IBin where
    pretty IPlus  = "+"
    pretty IMinus = "-"
    pretty ITimes = "*"
    pretty IDiv   = "div"
    pretty IAsl   = "asl"
    pretty IAsr   = "asr"
    pretty IMax   = "max"
    pretty IMin   = "min"
    pretty IAnd   = "∧"

instance Pretty FBin where
    pretty FPlus  = "+"
    pretty FMinus = "-"
    pretty FTimes = "*"
    pretty FDiv   = "%"
    pretty FExp   = "^"

instance Pretty FUn where
    pretty FSqrt = "sqrt"
    pretty FLog  = "log"

instance Pretty IUn where
    pretty ISgn = "sgn"
    pretty INot = "¬"

prettyIR :: [Stmt] -> Doc ann
prettyIR = prettyLines . fmap pretty
