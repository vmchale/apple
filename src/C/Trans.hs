module C.Trans ( writeC ) where

import           A
import           C
import           Control.Monad.State.Strict (State, modify, runState, state)
import qualified Data.IntMap                as IM
import           Data.Word                  (Word64)
import           Nm
import           Nm.IntMap

data CSt = CSt { labels      :: [Label]
               , temps       :: [Int]
               , arrs        :: [Int]
               , assemblerSt :: !Int
               , vars        :: IM.IntMap Temp -- track vars so that (Var x) can be replaced at the site
               , dvars       :: IM.IntMap FTemp
               , avars       :: IM.IntMap (Maybe Int, Temp)
               , fvars       :: IM.IntMap (Label, [(Maybe Int, Temp)], (Maybe Int, Temp))
               , _aa         :: AsmData
               , mts         :: IM.IntMap Temp
               }

nextI :: CM Int
nextI = state (\(CSt l (tϵ:t) ar as v d a f aas ts) -> (tϵ, CSt l t ar as v d a f aas ts))

newITemp :: CM Temp
newITemp = ITemp <$> nextI

newFTemp :: CM FTemp
newFTemp = FTemp <$> nextI

addVar :: Nm a -> Temp -> CSt -> CSt
addVar n r (CSt l t ar as v d a f aas ts) = CSt l t ar as (insert n r v) d a f aas ts

addD :: Nm a -> FTemp -> CSt -> CSt
addD n r (CSt l t ar as v d a f aas ts) = CSt l t ar as v (insert n r d) a f aas ts

addAVar :: Nm a -> (Maybe Int, Temp) -> CSt -> CSt
addAVar n r (CSt l t ar as v d a f aas ts) = CSt l t ar as v d (insert n r a) f aas ts

type CM = State CSt

isF, isI :: T a -> Bool
isF F = True; isF _ = False
isI I = True; isI _ = False

writeC :: E (T ()) -> ([CS], LSt, AsmData, IM.IntMap Temp)
writeC = π.flip runState (CSt [0..] [0..] [0..] 0 IM.empty IM.empty IM.empty IM.empty IM.empty IM.empty) . writeCM . fmap rLi where π (s, CSt l t _ _ _ _ _ _ aa a) = (s, LSt l t, aa, a)

writeCM :: E (T ()) -> CM [CS]
writeCM eϵ = do
    cs <- traverse (\_ -> newITemp) [(0::Int)..5]; fs <- traverse (\_ -> newFTemp) [(0::Int)..5]
    (zipWith (\xr xr' -> MX xr' (FTmp xr)) [F0,F1,F2,F3,F4,F5] fs ++) . (zipWith (\r r' -> MT r' (Tmp r)) [C0,C1,C2,C3,C4,C5] cs ++) <$> go eϵ fs cs where
    go (Lam _ x@(Nm _ _ F) e) (fr:frs) rs = do
        modify (addD x fr)
        go e frs rs
    go (Lam _ (Nm _ _ F) _) [] _ = error "Not enough floating-point registers!"
    go (Lam _ x@(Nm _ _ I) e) frs (r:rs) = do
        modify (addVar x r)
        go e frs rs
    go (Lam _ x@(Nm _ _ Arr{}) e) frs (r:rs) = do
        modify (addAVar x (Nothing, r))
        go e frs rs
    go Lam{} _ [] = error "Not enough registers!"
    go e _ _ | isF (eAnn e) = do {f <- newFTemp ; (++[MX FRet0 (FTmp f)]) <$> feval e f} -- avoid clash with xmm0 (arg + ret)
             | isI (eAnn e) = eval e CRet

eval :: E (T ()) -> Temp -> CM [CS]
eval = undefined

feval :: E (T ()) -> FTemp -> CM [CS]
feval = undefined
