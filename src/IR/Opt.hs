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
        (e0', (ConstI i))      | Just s <- cLog i -> IB IAsl e0' (ConstI s)
        ((ConstI i), e1')      | Just s <- cLog i -> IB IAsl e1' (ConstI s)
        (e0', e1')                                -> IB ITimes e0' e1'
optE (IB IPlus e0 e1) =
    case (optE e0, optE e1) of
        (ConstI 0, e1')        -> e1'
        (e0', ConstI 0)        -> e0'
        (ConstI i0, ConstI i1) -> ConstI$i0+i1
        (e0', e1')             -> IB IPlus e0' e1'
optE (IB IAsl e0 e1) =
    case (optE e0, optE e1) of
        (ConstI i0, ConstI i1) -> ConstI$i0 `shiftL` (fromIntegral i1)
        (e0',e1')              -> IB IAsl e0' e1'
optE (IB op e e')            = IB op (optE e) (optE e')
optE (IRel rel e e')         = IRel rel (optE e) (optE e')
optE (FRel rel fe fe')       = FRel rel (optF fe) (optF fe')
optE (IU u e)                = IU u (optE e)
optE (IRFloor fe)            = IRFloor (optF fe)
optE (EAt p)                 = EAt (optP p)
optE e                       = e

optF :: FExp -> FExp
optF (FAt p) = FAt (optP p)
optF fe      = fe

optP :: AE -> AE
optP (AP t me l) = AP t (fmap optE me) l

opt :: Stmt -> Stmt
opt (Cpy s d n)   = Cpy (optP s) (optP d) (optE n)
opt (MT r e)      = MT r (optE e)
opt (Ma l t e)    = Ma l t (optE e)
opt (Wr p e)      = Wr (optP p) (optE e)
opt (WrF p e)     = WrF (optP p) (optF e)
opt (MX xr e)     = MX xr (optF e)
opt (Cmov e t e') = Cmov (optE e) t (optE e')
opt (MJ e l)      = MJ (optE e) l
opt s             = s
