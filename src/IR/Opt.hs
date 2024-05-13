module IR.Opt ( optIR ) where

import           IR
import           Op

optIR :: [Stmt] -> [Stmt]
optIR = fmap opt

optE :: Exp -> Exp
optE (IB ITimes (ConstI 1) e) = optE e
optE (IB ITimes e (ConstI 1)) = optE e
optE (IB IPlus e (ConstI 0))  = optE e
optE (IB op e e')             = IB op (optE e) (optE e')
optE (IRel rel e e')          = IRel rel (optE e) (optE e')
optE (FRel rel fe fe')        = FRel rel (optF fe) (optF fe')
optE (IU u e)                 = IU u (optE e)
optE (IRFloor fe)             = IRFloor (optF fe)
optE (EAt p)                  = EAt (optP p)
optE e                        = e

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
