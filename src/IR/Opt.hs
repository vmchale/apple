module IR.Opt ( optIR ) where

import           Bits
import           Data.Bits (shiftL)
import           IR
import           Op

optIR :: [Stmt] -> [Stmt]
optIR = fmap opt

optE :: Exp -> Exp
optE (IB ITimes e0 e1) =
    case (optE e0, optE e1) of
        (ConstI 0, _)                             -> ConstI 0
        (_, ConstI 0)                             -> ConstI 0
        (ConstI 1, e1')                           -> e1'
        (e0', ConstI 1)                           -> e0'
        (ConstI i0, ConstI i1)                    -> ConstI$i0*i1
        (e0', ConstI i)        | Just s <- cLog i -> IB IAsl e0' (ConstI s)
        (ConstI i, e1')      | Just s <- cLog i   -> IB IAsl e1' (ConstI s)
        (e0', e1')                                -> IB ITimes e0' e1'
optE (IB IPlus e0 e1) =
    case (optE e0, optE e1) of
        (ConstI 0, e1')        -> e1'
        (e0', ConstI 0)        -> e0'
        (ConstI i0, ConstI i1) -> ConstI$i0+i1
        (e0', e1')             -> IB IPlus e0' e1'
optE (IB IMinus e0 e1) =
    case (optE e0, optE e1) of
        (ConstI i0, ConstI i1) -> ConstI$i0-i1
        (e0', ConstI 0)        -> e0'
        (e0', e1')             -> IB IMinus e0' e1'
optE (IB IAsl e0 e1) =
    case (optE e0, optE e1) of
        (ConstI i0, ConstI i1) -> ConstI$i0 `shiftL` fromIntegral i1
        (e0', ConstI 0)        -> e0'
        (e0',e1')              -> IB IAsl e0' e1'
optE (IB op e e')            = IB op (optE e) (optE e')
optE (IRel rel e e')         = IRel rel (optE e) (optE e')
optE (FRel rel fe fe')       = FRel rel (optF fe) (optF fe')
optE (IU u e)                = IU u (optE e)
optE (IRFloor fe)            = IRFloor (optF fe)
optE (EAt p)                 = EAt (optP p)
optE (BAt p)                 = BAt (optP p)
optE e                       = e

optF :: FE -> FE
optF (FAt p) = FAt (optP p)
optF (FConv e) =
    case optE e of
        ConstI i -> ConstF$fromIntegral i
        e'       -> FConv e'
optF (FU FLog e) =
    case optF e of
        ConstF d -> ConstF$log d
        e'       -> FU FLog e'
optF (FB FMinus e0 e1) =
    case (optF e0, optF e1) of
        (e0', ConstF 0) -> e0'
        (e0', e1')      -> FB FMinus e0' e1'
optF (FB FPlus e0 e1) =
    case (optF e0, optF e1) of
        (ConstF 0, e1')        -> e1'
        (e0', ConstF 0)        -> e0'
        (ConstF x0, ConstF x1) -> ConstF$x0+x1
        (e0',e1')              -> FB FPlus e0' e1'
optF (FB FTimes e0 e1) =
    case (optF e0, optF e1) of
        (ConstF 1, e1')        -> e1'
        (e0', ConstF 1)        -> e0'
        (ConstF x0, ConstF x1) -> ConstF$x0*x1
        (e0',e1')              -> FB FTimes e0' e1'
optF (FB FDiv e0 e1) =
    case (optF e0, optF e1) of
        (e0', ConstF 1)        -> e0'
        (ConstF x0, ConstF x1) -> ConstF$x0/x1
        (e0', ConstF x)        -> FB FTimes e0' (ConstF$1/x)
        (e0',e1')              -> FB FDiv e0' e1'
optF fe      = fe

optP :: AE -> AE
optP (AP t me l) = AP t (fmap optE me) l

opt :: Stmt -> Stmt
opt (Cpy s d n)   = Cpy (optP s) (optP d) (optE n)
opt (Cpy1 s d n)  = Cpy1 (optP s) (optP d) (optE n)
opt (MT r e)      = MT r (optE e)
opt (Ma l t e)    = Ma l t (optE e)
opt (Wr p e)      = Wr (optP p) (optE e)
opt (WrF p e)     = WrF (optP p) (optF e)
opt (WrB p e)     = WrB (optP p) (optE e)
opt (MX xr e)     = MX xr (optF e)
opt (Cmov e t e') = Cmov (optE e) t (optE e')
opt (MJ e l)      = MJ (optE e) l
opt s             = s
