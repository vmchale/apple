{-# LANGUAGE TupleSections #-}

module C.Trans ( writeC ) where

import           A
import           C
import           Control.Monad.State.Strict (State, gets, modify, runState, state)
import           Data.Int                   (Int64)
import qualified Data.IntMap                as IM
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

nextArr :: CM Int
nextArr = state (\(CSt l t (a:ar) as v d aϵ f aas ts) -> (a, CSt l t ar as v d aϵ f aas ts))

newITemp :: CM Temp
newITemp = ITemp <$> nextI

newFTemp :: CM FTemp
newFTemp = FTemp <$> nextI

addMT :: Int -> Temp -> CSt -> CSt
addMT i tϵ (CSt l t ar as v d a f aas ts) = CSt l t ar as v d a f aas (IM.insert i tϵ ts)

addVar :: Nm a -> Temp -> CSt -> CSt
addVar n r (CSt l t ar as v d a f aas ts) = CSt l t ar as (insert n r v) d a f aas ts

addD :: Nm a -> FTemp -> CSt -> CSt
addD n r (CSt l t ar as v d a f aas ts) = CSt l t ar as v (insert n r d) a f aas ts

addAVar :: Nm a -> (Maybe Int, Temp) -> CSt -> CSt
addAVar n r (CSt l t ar as v d a f aas ts) = CSt l t ar as v d (insert n r a) f aas ts

getT :: IM.IntMap b -> Nm a -> b
getT st n = findWithDefault (error ("Internal error: variable " ++ show n ++ " not assigned to a temp.")) n st

type CM = State CSt

isF, isI, isIF :: T a -> Bool
isF F = True; isF _ = False
isI I = True; isI _ = False
isArr Arr{}=True; isArr _=False
isIF I=True; isIF F=True; isIF _=False

if1 :: T a -> Maybe (T a)
if1 (Arr (_ `Cons` Nil) I) = Just I; if1 (Arr (_ `Cons` Nil) F) = Just F; if1 _ = Nothing

if1p :: T a -> Bool
if1p t | Just{} <- if1 t = True | otherwise = False

f1 :: T a -> Bool
f1 (Arr (_ `Cons` Nil) F) = True; f1 _ = False

bT :: Integral b => T a -> b
bT (P ts) = sum (bT<$>ts)
bT F      = 8
bT I      = 8

staRnk :: Integral b => Sh a -> Maybe b
staRnk Nil           = Just 0
staRnk (_ `Cons` sh) = (1+) <$> staRnk sh
staRnk _             = Nothing

tRnk :: T a -> Maybe (T a, Int64)
tRnk (Arr sh t) = (t,) <$> staRnk sh
tRnk _          = Nothing

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
             | isArr (eAnn e) = do {i <- newITemp; (l,r) <- aeval e i; pure$r++[MT CRet (Tmp i)]++case l of {Just m -> [RA m]; Nothing -> []}}

rtemp :: T a -> CM (Either FTemp Temp)
rtemp F=Left<$>newFTemp; rtemp I=Right<$>newITemp

writeF :: E (T ())
       -> [(Maybe Int, Temp)] -- ^ registers for arguments
       -> [FTemp]
       -> Either FTemp Temp -- ^ register for return value
       -> CM (Maybe Int, [CS])
writeF (Lam _ x e) (r:rs) frs ret | isArr (loc x) = do
    modify (addAVar x r)
    writeF e rs frs ret
writeF (Lam _ x e) ((_,r):rs) frs ret | isI (loc x) = do
    modify (addVar x r)
    writeF e rs frs ret
writeF (Lam _ x e) rs (fr:frs) ret | isF (loc x) = do
    modify (addD x fr)
    writeF e rs frs ret
writeF (Lam _ x _) [] _ _ | isI (loc x) = error "Internal error: wrong number of registers to arguments."
writeF (Lam _ x _) _ [] _ | isF (loc x) = error "Internal error: wrong number of registers to arguments."
writeF e _ _ (Right ret) | isArr (eAnn e) = aeval e ret
writeF e _ _ (Right ret) = (Nothing,) <$> eval e ret
writeF e _ _ (Left fret) = (Nothing,) <$> feval e fret

writeRF :: E (T ()) -> [Temp] -> [FTemp] -> Either FTemp Temp -> CM [CS]
writeRF e rs frs = fmap snd . writeF e ((Nothing,) <$> rs) frs

mt :: ArrAcc -> Either FTemp Temp -> CS
mt p (Right t) = MT t (EAt p); mt p (Left t) = MX t (FAt p)

wt :: ArrAcc -> Either FTemp Temp -> CS
wt p (Right t) = Wr p (Tmp t); wt p (Left t) = WrF p (FTmp t)

aeval :: E (T ()) -> Temp -> CM (Maybe Int, [CS])
aeval (Var _ x) t = do
    st <- gets avars
    let (i, r) = getT st x
    pure (i, [MT t (Tmp r)])
aeval (EApp _ (EApp _ (Builtin _ Map) op) e) t | (Arrow tD tC) <- eAnn op, isIF tD && isIF tC= do
    a <- nextArr
    arrT <- newITemp
    (l, plE) <- aeval e arrT
    -- rank 1
    let sz=EAt (ADim t 0 l)
    rC <- rtemp tC; rD <- rtemp tD
    let (aD,dD) = case rD of
          Left rDϵ  -> (id,(rDϵ:))
          Right rDϵ -> ((rDϵ:),id)
    ss <- writeRF op (aD []) (dD []) rC
    iR <- newITemp; szR <- newITemp
    let loopBody=mt (AElem arrT 1 (Tmp iR) l 8) rD:ss++[wt (AElem t 1 (Tmp iR) (Just a) 8) rC]
        loop=For iR 0 C.Lte (Tmp szR) loopBody
    modify (addMT a t)
    pure (Just a, plE ++ MT szR sz:Ma a t 1 (Tmp szR) 8:Wr (ADim t 0 (Just a)) (Tmp szR):[loop])
aeval (EApp _ (EApp _ (Builtin _ CatE) x) y) t | Just (ty, 1) <- tRnk (eAnn x) = do
    a <- nextArr
    xR <- newITemp; yR <- newITemp
    xnR <- newITemp; ynR <- newITemp; tn <- newITemp
    let tyN=bT ty
    (lX, plX) <- aeval x xR; (lY, plY) <- aeval y yR
    modify (addMT a t)
    pure (Just a, plX ++ plY ++ MT xnR (EAt (ADim xR 0 lX)):MT ynR (EAt (ADim yR 0 lY)):MT tn (Tmp xnR+Tmp ynR):Ma a t 1 (Tmp tn) tyN:Wr (ADim t 0 (Just a)) (Tmp tn):CpyE (AElem t 1 0 (Just a) tyN) (AElem xR 1 0 lX tyN) (Tmp xnR) tyN:[CpyE (AElem t 1 (Tmp xnR) (Just a) tyN) (AElem yR 1 0 lY tyN) (Tmp ynR) tyN])
aeval (EApp _ (EApp _ (EApp _ (Builtin _ IRange) start) end) incr) t = do
    a <- nextArr
    n <- newITemp
    startR <- newITemp; endR <- newITemp; incrR <- newITemp
    i <- newITemp
    pStart <- eval start startR; pEnd <- eval end endR; pIncr <- eval incr incrR
    let pN=MT n (Bin C.IDiv (Tmp endR - Tmp startR) (Tmp incrR)+1)
        loop=For i 0 C.Lt (Tmp n) [Wr (AElem t 1 (Tmp i) (Just a) 8) (Tmp startR), MT startR (Tmp startR+Tmp incrR)]
    modify (addMT a t)
    pure (Just a, pStart++pEnd++pIncr++pN:Ma a t 1 (Tmp n) 8:Wr (ADim t 0 (Just a)) (Tmp n):[loop])
aeval (EApp res (EApp _ (Builtin _ Cyc) xs) n) t | if1p res = do
    a <- nextArr
    xR <- newITemp; i <- newITemp; nR <- newITemp; nO <- newITemp
    szR <- newITemp
    (lX, plX) <- aeval xs xR
    plN <- eval n nR
    ix <- newITemp
    let body=For i 0 C.Lt (Tmp nR) [CpyE (AElem t 1 (Tmp ix) (Just a) 8) (AElem xR 1 0 lX 8) (Tmp szR) 8, MT ix (Tmp ix+Tmp szR)]
    modify (addMT a t)
    pure (Just a, plX ++ plN ++ MT szR (EAt (ADim xR 0 lX)):MT nO (Tmp szR*Tmp nR):Ma a t 1 (Tmp nO) 8:Wr (ADim t 0 (Just a)) (Tmp nO):MT ix 0:[body])
aeval (EApp _ (EApp _ (Builtin _ VMul) a) x) t | f1 (eAnn x) = do
    aL <- nextArr
    xR <- newITemp; aR <- newITemp; i <- newITemp; j <- newITemp; m <- newITemp; n <- newITemp; z <- newFTemp
    (lA, plA) <- aeval a aR
    (lX, plX) <- aeval x xR
    modify (addMT aL t)
    let loop = For i 0 C.Lt (Tmp m) [MX z 0, For j 0 C.Lt (Tmp n) [MX z (FTmp z+FAt (AElem aR 2 (Tmp n*Tmp i+Tmp j) lA 8)*FAt (AElem xR 1 (Tmp j) lX 8))], WrF (AElem t 1 (Tmp i) (Just aL) 8) (FTmp z)]
    pure (Just aL, plA ++ plX ++ MT m (EAt (ADim aR 0 lA)):Ma aL t 1 (Tmp m) 8:Wr (ADim t 0 (Just aL)) (Tmp m):MT n (EAt (ADim xR 0 lX)):[loop])
aeval e _ = error (show e)

eval :: E (T ()) -> Temp -> CM [CS]
eval (ILit _ n) t = pure [MT t (fromInteger n)]
eval e _          = error (show e)

feval :: E (T ()) -> FTemp -> CM [CS]
feval (EApp F (EApp _ (Builtin _ Times) e0) e1) t = do
    t0 <- newFTemp; t1 <- newFTemp
    pl0 <- feval e0 t0; pl1 <- feval e1 t1
    pure $ pl0 ++ pl1 ++ [MX t (FTmp t0 * FTmp t1)]
feval (Var _ x) t = do
    st <- gets dvars
    pure [MX t (FTmp $ getT st x)]
feval (ILit _ x) t = pure [MX t (ConstF $ fromIntegral x)] -- if it overflows you deserve it
feval (FLit _ x) t = pure [MX t (ConstF x)]
feval e _ = error (show e)
