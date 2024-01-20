{-# LANGUAGE TupleSections #-}

module C.Trans ( writeC ) where

import           A
import           C
import           Control.Composition        (thread)
import           Control.Monad.State.Strict (State, gets, modify, runState, state)
import           Data.Either                (rights)
import           Data.Int                   (Int64)
import qualified Data.IntMap                as IM
import qualified Data.IntSet                as IS
import           Data.Word                  (Word64)
import           GHC.Float                  (castDoubleToWord64)
import           Nm
import           Nm.IntMap
import           Op

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

nextAA :: CM Int
nextAA = state (\(CSt l t ar as v d a f aas ts) -> (as, CSt l t ar (as+1) v d a f aas ts))

newITemp :: CM Temp
newITemp = ITemp <$> nextI

newFTemp :: CM FTemp
newFTemp = FTemp <$> nextI

addMT :: Int -> Temp -> CSt -> CSt
addMT i tϵ (CSt l t ar as v d a f aas ts) = CSt l t ar as v d a f aas (IM.insert i tϵ ts)

addAA :: Int -> [Word64] -> CSt -> CSt
addAA i aa (CSt l t ar as v d a f aas ts) = CSt l t ar as v d a f (IM.insert i aa aas) ts

addVar :: Nm a -> Temp -> CSt -> CSt
addVar n r (CSt l t ar as v d a f aas ts) = CSt l t ar as (insert n r v) d a f aas ts

addD :: Nm a -> FTemp -> CSt -> CSt
addD n r (CSt l t ar as v d a f aas ts) = CSt l t ar as v (insert n r d) a f aas ts

addAVar :: Nm a -> (Maybe Int, Temp) -> CSt -> CSt
addAVar n r (CSt l t ar as v d a f aas ts) = CSt l t ar as v d (insert n r a) f aas ts

getT :: IM.IntMap b -> Nm a -> b
getT st n = findWithDefault (error ("Internal error: variable " ++ show n ++ " not assigned to a temp.")) n st

type CM = State CSt

fop op e0 = EApp F (EApp (Arrow F F) (Builtin (Arrow F (Arrow F F)) op) e0)
eMinus = fop Minus
eDiv = fop Div

isF, isI, isIF :: T a -> Bool
isF F = True; isF _ = False
isI I = True; isI _ = False
isArr Arr{}=True; isArr _=False
isIF I=True; isIF F=True; isIF _=False

mIF :: T a -> Maybe (T a)
mIF (Arr _ F)=Just F; mIF (Arr _ I)=Just I; mIF _=Nothing

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

mIFs :: [E a] -> Maybe [Word64]
mIFs = traverse mIFϵ where mIFϵ (FLit _ d)=Just (castDoubleToWord64 d); mIFϵ (ILit _ n)=Just (fromIntegral n); mIFϵ _=Nothing

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
mt p = either (`MX` FAt p) (`MT` EAt p)

wt :: ArrAcc -> Either FTemp Temp -> CS
wt p = either (WrF p.FTmp) (Wr p.Tmp)

ax (Left x)  = (id,(x:))
ax (Right x) = ((x:),id)

eeval :: E (T ()) -> Either FTemp Temp -> CM [CS]
eeval e = either (feval e) (eval e)

data RI a b = Cell a | Index b deriving Show

cells :: [RI a b] -> [a]
cells []           = []
cells (Cell a:as)  = a:cells as
cells (Index{}:as) = cells as

indices :: [RI a b] -> [b]
indices []           = []
indices (Index a:as) = a:indices as
indices (Cell{}:as)  = indices as

plDim :: Int64 -> (Temp, Maybe Int) -> CM ([Temp], [CS])
plDim rnk (a,l) =
    unzip <$> traverse (\at -> do {dt <- newITemp; pure (dt, MT dt (EAt at))}) [ ADim a (ConstI$i-1) l | i <- [1..rnk] ]

offByDim :: [Temp] -> CM ([Temp], [CS])
offByDim dims = do
    sts <- traverse (\_ -> newITemp) (undefined:dims)
    let ss=zipWith3 (\s1 s0 d -> MT s1 (Tmp s0*Tmp d)) (tail sts) sts dims
    pure (reverse sts, MT (head sts) 1:ss)
    -- drop 1 for strides

-- each (Right t) specifies a dimension (bounds); each (Left e) specifies a (currently)
-- fixed index
extrCell :: [Either CE Temp] -> [Temp] -> (Temp, Maybe Int) -> Temp -> CM [CS]
extrCell fixedIxesDims sstrides (srcP, srcL) dest = do
    ts <- traverse (\_ -> newITemp) dims
    t <- newITemp; i <- newITemp
    pure $ (MT i 0:) $ thread (zipWith (\d tϵ -> (:[]) . For tϵ 0 ILt (Tmp d)) dims ts) $
        let ixes = either id Tmp <$> replaceZs fixedIxesDims ts
        -- FIXME: ixes... paired with wrong stride? (reverse one?)
        in [MT t (EAt (At srcP (Tmp <$> sstrides) ixes srcL 8)), Wr (Raw dest (Tmp i) Nothing 8) (Tmp t), MT i (Tmp i+1)]
    where dims = rights fixedIxesDims
          replaceZs (f@Left{}:ds) ts    = f:replaceZs ds ts
          replaceZs (Right{}:ds) (t:ts) = Right t:replaceZs ds ts
          replaceZs [] []               = []

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
    let sz=EAt (ADim arrT 0 l)
    rC <- rtemp tC; rD <- rtemp tD
    let (aD,dD) = ax rD
    ss <- writeRF op (aD []) (dD []) rC
    iR <- newITemp; szR <- newITemp
    let loopBody=mt (AElem arrT 1 (Tmp iR) l 8) rD:ss++[wt (AElem t 1 (Tmp iR) (Just a) 8) rC]
        loop=For iR 0 ILt (Tmp szR) loopBody
    modify (addMT a t)
    pure (Just a, plE ++ MT szR sz:Ma a t 1 (Tmp szR) 8:Wr (ADim t 0 (Just a)) (Tmp szR):[loop])
aeval (EApp tO (EApp _ (Builtin _ (Rank [(cr, Just ixs)])) f) xs) t | Just (tA, rnk) <- tRnk (eAnn xs), Just tOR <- mIF tO, (Arrow _ tF) <- eAnn f, isIF tF && isIF tA = do
    a <- nextArr
    xR <- newITemp
    (lX, plX) <- aeval xs xR
    slopP <- newITemp; y <- rtemp tOR
    let ixsIs = IS.fromList ixs; allIx = [ if ix `IS.member` ixsIs then Cell ix else Index ix | ix <- [1..fromIntegral rnk] ]
    oSz <- newITemp; slopSz <- newITemp
    (dts, dss) <- plDim rnk (xR, lX)
    (sts, sssϵ) <- offByDim (reverse dts)
    let _:sstrides = sts; sss=init sssϵ
    allts <- traverse (\i -> case i of {Cell{} -> Cell <$> newITemp; Index{} -> Index <$> newITemp}) allIx
    let complts = cells allts
        allDims = zipWith (\ix dt -> case ix of {Cell{} -> Cell dt; Index{} -> Index dt}) allIx dts
        complDims = indices allDims; oDims = cells allDims
        oRnk=rnk-fromIntegral cr; slopRnk=rnk-oRnk
        wrOSz = MT oSz 1:[MT oSz (Tmp oSz*Tmp dϵ) | dϵ <- oDims]
        wrSlopSz = MT slopSz 1:[MT slopSz (Tmp slopSz*Tmp dϵ) | dϵ <- complDims]++[MT slopSz (Tmp slopSz+ConstI (slopRnk+1))]
    (_, ss) <- writeF f [(Nothing, slopP)] [] y
    let ecArg = zipWith (\d tt -> case (d,tt) of (dϵ,Index{}) -> Right dϵ; (_,Cell tϵ) -> Left (Tmp tϵ)) dts allts
    xRd <- newITemp; slopPd <- newITemp
    place <- extrCell ecArg sstrides (xRd, lX) slopPd
    di <- newITemp
    -- FIXME: oDims but complts? maybe I just named it wrong lol
    let loop=thread (zipWith (\d tϵ -> (:[]) . For tϵ 0 ILt (Tmp d)) oDims complts) $ place ++ ss ++ [wt (AElem t (ConstI oRnk) (Tmp di) Nothing 8) y, MT di (Tmp di+1)]
    modify (addMT a t)
    pure (Just a, plX++dss++wrOSz++Ma a t (ConstI oRnk) (Tmp oSz) 8:zipWith (\d i -> Wr (ADim t (ConstI i) (Just a)) (Tmp d)) oDims [0..]++wrSlopSz++Sa slopP (Tmp slopSz):Wr (ARnk slopP Nothing) (ConstI slopRnk):zipWith (\d i -> Wr (ADim slopP (ConstI i) Nothing) (Tmp d)) complDims [0..]++sss++MT xRd (DP xR rnk):MT slopPd (DP slopP slopRnk):MT di 0:loop++[Pop (Tmp slopSz)])
aeval (EApp _ (EApp _ (Builtin _ CatE) x) y) t | Just (ty, 1) <- tRnk (eAnn x) = do
    a <- nextArr
    xR <- newITemp; yR <- newITemp
    xnR <- newITemp; ynR <- newITemp; tn <- newITemp
    let tyN=bT ty
    (lX, plX) <- aeval x xR; (lY, plY) <- aeval y yR
    modify (addMT a t)
    pure (Just a, plX ++ plY ++ MT xnR (EAt (ADim xR 0 lX)):MT ynR (EAt (ADim yR 0 lY)):MT tn (Tmp xnR+Tmp ynR):Ma a t 1 (Tmp tn) tyN:Wr (ADim t 0 (Just a)) (Tmp tn):CpyE (AElem t 1 0 (Just a) tyN) (AElem xR 1 0 lX tyN) (Tmp xnR) tyN:[CpyE (AElem t 1 (Tmp xnR) (Just a) tyN) (AElem yR 1 0 lY tyN) (Tmp ynR) tyN])
aeval (EApp _ (EApp _ (EApp _ (Builtin _ IRange) start) end) (ILit _ 1)) t = do
    a <- nextArr
    n <- newITemp
    startR <- newITemp; endR <- newITemp
    i <- newITemp
    pStart <- eval start startR; pEnd <- eval end endR
    let pN=MT n ((Tmp endR - Tmp startR)+1)
        loop=For i 0 ILt (Tmp n) [Wr (AElem t 1 (Tmp i) (Just a) 8) (Tmp startR), MT startR (Tmp startR+1)]
    modify (addMT a t)
    pure (Just a, pStart++pEnd++pN:Ma a t 1 (Tmp n) 8:Wr (ADim t 0 (Just a)) (Tmp n):[loop])
aeval (EApp _ (EApp _ (EApp _ (Builtin _ IRange) start) end) incr) t = do
    a <- nextArr
    n <- newITemp
    startR <- newITemp; endR <- newITemp; incrR <- newITemp
    i <- newITemp
    pStart <- eval start startR; pEnd <- eval end endR; pIncr <- eval incr incrR
    let pN=MT n (Bin Op.IDiv (Tmp endR - Tmp startR) (Tmp incrR)+1)
        loop=For i 0 ILt (Tmp n) [Wr (AElem t 1 (Tmp i) (Just a) 8) (Tmp startR), MT startR (Tmp startR+Tmp incrR)]
    modify (addMT a t)
    pure (Just a, pStart++pEnd++pIncr++pN:Ma a t 1 (Tmp n) 8:Wr (ADim t 0 (Just a)) (Tmp n):[loop])
aeval (EApp _ (EApp _ (EApp _ (Builtin _ FRange) start) end) steps) t = do
    a <- nextArr
    i <- newITemp
    startR <- newFTemp; incrR <- newFTemp; n <- newITemp
    putStart <- feval start startR; putN <- eval steps n
    putIncr <- feval ((end `eMinus` start) `eDiv` (EApp F (Builtin (Arrow I F) ItoF) steps `eMinus` FLit F 1)) incrR
    let loop=For i 0 ILt (Tmp n) [WrF (AElem t 1 (Tmp i) (Just a) 8) (FTmp startR), MX startR (FTmp startR+FTmp incrR)]
    modify (addMT a t)
    pure (Just a, putStart++putIncr++putN++Ma a t 1 (Tmp n) 8:Wr (ADim t 0 (Just a)) (Tmp n):[loop])
aeval (EApp res (EApp _ (Builtin _ Cyc) xs) n) t | if1p res = do
    a <- nextArr
    xR <- newITemp; i <- newITemp; nR <- newITemp; nO <- newITemp
    szR <- newITemp
    (lX, plX) <- aeval xs xR
    plN <- eval n nR
    ix <- newITemp
    let body=For i 0 ILt (Tmp nR) [CpyE (AElem t 1 (Tmp ix) (Just a) 8) (AElem xR 1 0 lX 8) (Tmp szR) 8, MT ix (Tmp ix+Tmp szR)]
    modify (addMT a t)
    pure (Just a, plX ++ plN ++ MT szR (EAt (ADim xR 0 lX)):MT nO (Tmp szR*Tmp nR):Ma a t 1 (Tmp nO) 8:Wr (ADim t 0 (Just a)) (Tmp nO):MT ix 0:[body])
aeval (EApp _ (EApp _ (Builtin _ VMul) a) x) t | f1 (eAnn x) = do
    aL <- nextArr
    xR <- newITemp; aR <- newITemp; i <- newITemp; j <- newITemp; m <- newITemp; n <- newITemp; z <- newFTemp
    (lA, plA) <- aeval a aR
    (lX, plX) <- aeval x xR
    modify (addMT aL t)
    let loop = For i 0 ILt (Tmp m) [MX z 0, For j 0 ILt (Tmp n) [MX z (FTmp z+FAt (AElem aR 2 (Tmp n*Tmp i+Tmp j) lA 8)*FAt (AElem xR 1 (Tmp j) lX 8))], WrF (AElem t 1 (Tmp i) (Just aL) 8) (FTmp z)]
    pure (Just aL, plA ++ plX ++ MT m (EAt (ADim aR 0 lA)):Ma aL t 1 (Tmp m) 8:Wr (ADim t 0 (Just aL)) (Tmp m):MT n (EAt (ADim xR 0 lX)):[loop])
aeval (EApp _ (EApp _ (Builtin _ Mul) a) b) t | Just (F, _) <- tRnk (eAnn a) = do
    aL <- nextArr
    aR <- newITemp; bR <- newITemp; i <- newITemp; j <- newITemp; k <- newITemp; m <- newITemp; n <- newITemp; o <- newITemp; z <- newFTemp
    (lA, plA) <- aeval a aR
    (lB, plB) <- aeval b bR
    modify (addMT aL t)
    let loop=For i 0 ILt (Tmp m) [For j 0 ILt (Tmp o) [MX z 0, For k 0 ILt (Tmp n) [MX z (FTmp z+FAt (AElem aR 2 (Tmp n*Tmp i+Tmp k) lA 8)*FAt (AElem bR 2 (Tmp k*Tmp o+Tmp j) lB 8))], WrF (AElem t 2 (Tmp i*Tmp o+Tmp j) (Just aL) 8) (FTmp z)]]
    pure (Just aL, plA++plB++MT m (EAt (ADim aR 0 lA)):MT n (EAt (ADim bR 0 lB)):MT o (EAt (ADim bR 1 lB)):Ma aL t 2 (Tmp m*Tmp o) 8:Wr (ADim t 0 (Just aL)) (Tmp m):Wr (ADim t 1 (Just aL)) (Tmp o):[loop])
aeval (EApp _ (EApp _ (Builtin _ ConsE) x) xs) t | tX <- eAnn x, isIF tX = do
    a <- nextArr
    xR <- rtemp tX; xsR <- newITemp
    plX <- eeval x xR
    (l, plXs) <- aeval xs xsR
    nR <- newITemp; nϵR <- newITemp
    modify (addMT a t)
    pure (Just a, plXs++plX++MT nϵR (EAt (ADim xsR 0 l)):MT nR (Tmp nϵR+1):Ma a t 1 (Tmp nR) 8:Wr (ADim t 0 (Just a)) (Tmp nR):wt (AElem t 1 0 (Just a) 8) xR:[CpyE (AElem t 1 1 (Just a) 8) (AElem xsR 1 0 l 8) (Tmp nϵR) 8])
aeval (EApp _ (EApp _ (Builtin _ Snoc) x) xs) t | tX <- eAnn x, isIF tX = do
    a <- nextArr
    xR <- rtemp tX; xsR <- newITemp
    plX <- eeval x xR
    (l, plXs) <- aeval xs xsR
    nR <- newITemp; nϵR <- newITemp
    modify (addMT a t)
    pure (Just a, plXs++plX++MT nϵR (EAt (ADim xsR 0 l)):MT nR (Tmp nϵR+1):Ma a t 1 (Tmp nR) 8:Wr (ADim t 0 (Just a)) (Tmp nR):wt (AElem t 1 (Tmp nR-1) (Just a) 8) xR:[CpyE (AElem t 1 0 (Just a) 8) (AElem xsR 1 0 l 8) (Tmp nϵR) 8])
aeval (EApp _ (EApp _ (Builtin _ Re) n) x) t | tX <- eAnn x, isIF tX = do
    a <- nextArr
    xR <- rtemp tX; nR <- newITemp
    i <- newITemp
    putN <- eval n nR; putX <- eeval x xR
    let loop=For i 0 ILt (Tmp nR) [wt (AElem t 1 (Tmp i) (Just a) 8) xR]
    modify (addMT a t)
    pure (Just a, putN++Ma a t 1 (Tmp nR) 8:Wr (ADim t 0 (Just a)) (Tmp nR):putX++[loop])
aeval (EApp oTy (Builtin _ Init) x) t | if1p oTy = do
    a <- nextArr
    xR <- newITemp; nR <- newITemp
    (lX, plX) <- aeval x xR
    modify (addMT a t)
    pure (Just a, plX++MT nR (EAt (ADim xR 0 lX)-1):Ma a t 1 (Tmp nR) 8:Wr (ADim t 0 (Just a)) (Tmp nR):[CpyE (AElem t 1 0 (Just a) 8) (AElem xR 1 0 lX 8) (Tmp nR) 8])
aeval (EApp oTy (Builtin _ Tail) x) t | if1p oTy = do
    a <- nextArr
    xR <- newITemp; nR <- newITemp
    (lX, plX) <- aeval x xR
    modify (addMT a t)
    pure (Just a, plX++MT nR (EAt (ADim xR 0 lX)-1):Ma a t 1 (Tmp nR) 8:Wr (ADim t 0 (Just a)) (Tmp nR):[CpyE (AElem t 1 0 (Just a) 8) (AElem xR 1 1 lX 8) (Tmp nR) 8])
aeval (EApp _ (EApp _ (EApp _ (Builtin _ Zip) op) xs) ys) t | (Arrow tX (Arrow tY tC)) <- eAnn op, isIF tX && isIF tY && isIF tC = do
    a <- nextArr
    aPX <- newITemp; aPY <- newITemp
    (lX, plEX) <- aeval xs aPX; (lY, plEY) <- aeval ys aPY
    x <- rtemp tX; y <- rtemp tY; z <- rtemp tC
    let (aX,dX) = ax x; (aY,dY) = ax y
    ss <- writeRF op (aX.aY$[]) (dX.dY$[]) z
    nR <- newITemp; i <- newITemp
    let loopBody=mt (AElem aPX 1 (Tmp i) lX 8) x:mt (AElem aPY 1 (Tmp i) lY 8) y:ss++[wt (AElem t 1 (Tmp i) (Just a) 8) z]
        loop=For i 0 ILt (Tmp nR) loopBody
    modify (addMT a t)
    pure (Just a, plEX++plEY++MT nR (EAt (ADim aPX 0 lX)):Ma a t 1 (Tmp nR) 8:Wr (ADim t 0 (Just a)) (Tmp nR):[loop])
aeval (EApp _ (EApp _ (EApp _ (Builtin _ ScanS) op) seed) e) t | (Arrow tX (Arrow tY _)) <- eAnn op, isIF tX && isIF tY = do
    a <- nextArr
    aP <- newITemp
    acc <- rtemp tX; x <- rtemp tY
    plS <- eeval seed acc
    (l, plE) <- aeval e aP
    let (aAcc,dAcc)=ax acc; (aX,dX)=ax x
    ss <- writeRF op (aAcc.aX$[]) (dAcc.dX$[]) acc
    i <- newITemp; n <- newITemp
    let loopBody=wt (AElem t 1 (Tmp i) (Just a) 8) acc:mt (AElem aP 1 (Tmp i) l 8) x:ss
        loop=For i 0 ILt (Tmp n) loopBody
    modify (addMT a t)
    pure (Just a, plE++plS++MT n (EAt (ADim aP 0 l)+1):Ma a t 1 (Tmp n) 8:Wr (ADim t 0 (Just a)) (Tmp n):[loop])
aeval (EApp _ (EApp _ (Builtin _ Scan) op) xs) t | (Arrow tAcc (Arrow tX _)) <- eAnn op, isIF tAcc && isIF tX = do
    a <- nextArr
    aP <- newITemp
    acc <- rtemp tAcc; x <- rtemp tX
    (l, plE) <- aeval xs aP
    let (aAcc,dAcc)=ax acc; (aX,dX)=ax x
    ss <- writeRF op (aAcc.aX$[]) (dAcc.dX$[]) acc
    i <- newITemp; n <- newITemp
    let loopBody=wt (AElem t 1 (Tmp i-1) (Just a) 8) acc:mt (AElem aP 1 (Tmp i) l 8) x:ss
        loop=For i 1 ILeq (Tmp n) loopBody
    modify (addMT a t)
    pure (Just a, plE++MT n (EAt (ADim aP 0 l)):Ma a t 1 (Tmp n) 8:Wr (ADim t 0 (Just a)) (Tmp n):mt (AElem aP 1 0 l 8) acc:[loop])
aeval (EApp oTy (EApp _ (Builtin _ (DI n)) op) xs) t | Just{} <- if1 (eAnn xs), Just ot <- if1 oTy = do
    a <- nextArr
    aP <- newITemp
    slopP <- newITemp
    szR <- newITemp; sz'R <- newITemp; i <- newITemp
    fR <- rtemp ot
    ss <- writeRF op [slopP] [] fR
    let szSlop=fromIntegral$16+8*n
    (lX, plX) <- aeval xs aP
    let sz'=Tmp szR-fromIntegral(n-1)
    let loopBody=CpyE (AElem slopP 1 0 Nothing 8) (AElem aP 1 (Tmp i) lX 8) (fromIntegral n) 8:ss++[wt (AElem t 1 (Tmp i) (Just a) 8) fR]
        loop=For i 0 ILt (Tmp sz'R) loopBody
    modify (addMT a t)
    pure (Just a, plX++MT szR (EAt (ADim aP 0 lX)):MT sz'R sz':Ma a t 1 (Tmp sz'R) 8:Wr (ADim t 0 (Just a)) (Tmp sz'R):Sa slopP szSlop:Wr (ARnk slopP Nothing) 1:Wr (ADim slopP 0 Nothing) (fromIntegral n):loop:[Pop szSlop])
aeval (EApp _ (EApp _ (Builtin _ Rot) n) xs) t | if1p (eAnn xs) = do
    a <- nextArr
    xsR <- newITemp; nR <- newITemp; c <- newITemp; szR <- newITemp
    plN <- eval n nR
    (lX, plX) <- aeval xs xsR
    modify (addMT a t)
    pure (Just a, plX++plN++MT szR (EAt (ADim xsR 0 lX)):Ma a t 1 (Tmp szR) 8:Wr (ADim t 0 (Just a)) (Tmp szR):Ifn't (IRel IGeq (Tmp nR) 0) [MT nR (Tmp szR+ Tmp nR)]:MT c (Tmp szR-Tmp nR):[CpyE (AElem t 1 0 (Just a) 8) (AElem xsR 1 (Tmp nR) lX 8) (Tmp c) 8, CpyE (AElem t 1 (Tmp c) (Just a) 8) (AElem xsR 1 0 lX 8) (Tmp nR) 8])
aeval (Id _ (AShLit ns es)) t | Just ws <- mIFs es = do
    let rnk=fromIntegral$length ns
    n <- nextAA
    modify (addAA n (rnk:fmap fromIntegral ns++ws))
    pure (Nothing, [MT t (LA n)])
aeval (EApp _ (Builtin _ T) x) t | Just (ty, rnk) <- tRnk (eAnn x) = do
    a <- nextArr
    let sze=bT ty; dO=ConstI$8+8*rnk
    xR <- newITemp; xd <- newITemp; td <- newITemp
    (l, plX) <- aeval x xR
    (dts, plDs) <- plDim rnk (xR, l)
    (sts, plSs) <- offByDim (reverse dts)
    (std, plSd) <- offByDim dts
    let n:sstrides = sts; (_:dstrides) = std
    is <- traverse (\_ -> newITemp) [1..rnk]
    let loop=thread (zipWith (\i tt -> (:[]) . For i 0 ILt (Tmp tt)) is dts) [CpyE (At td (Tmp<$>dstrides) (Tmp<$>reverse is) (Just a) sze) (At xd (Tmp<$>sstrides) (Tmp<$>is) l sze) 1 sze]
    modify (addMT a t)
    pure (Just a, plX++plDs++plSs++Ma a t (ConstI rnk) (Tmp n) sze:zipWith (\tϵ o -> Wr (ADim t (ConstI o) (Just a)) (Tmp tϵ)) (reverse dts) [0..]++init plSd++MT xd (Tmp xR+dO):MT td (Tmp t+dO):loop)
aeval (EApp _ (EApp _ (EApp _ (Builtin _ Outer) op) xs) ys) t | (Arrow tX (Arrow tY tC)) <- eAnn op, isIF tX && isIF tY && isIF tC = do
    a <- nextArr
    x <- rtemp tX; y <- rtemp tY; z <- rtemp tC
    xR <- newITemp; yR <- newITemp; szX <- newITemp; szY <- newITemp; i <- newITemp; j <- newITemp; k <- newITemp
    (lX, plX) <- aeval xs xR
    (lY, plY) <- aeval ys yR
    let (aX,dX)=ax x; (aY,dY)=ax y
    ss <- writeRF op (aX.aY$[]) (dX.dY$[]) z
    let loop=For i 0 ILt (Tmp szX) [For j 0 ILt (Tmp szY) (mt (AElem xR 1 (Tmp i) lX 8) x:mt (AElem yR 1 (Tmp j) lY 8) y:ss++[wt (AElem t 2 (Tmp k) (Just a) 8) z, MT k (Tmp k+1)])]
    modify (addMT a t)
    pure (Just a, plX++plY++MT szX (EAt (ADim xR 0 lX)):MT szY (EAt (ADim yR 0 lY)):Ma a t 2 (Tmp szX*Tmp szY) 8:Wr (ADim t 0 (Just a)) (Tmp szX):Wr (ADim t 1 (Just a)) (Tmp szY):MT k 0:[loop])
aeval (EApp _ (EApp _ (Builtin _ Succ) op) xs) t | Arrow tX (Arrow _ tD) <- eAnn op, isIF tX && isIF tD= do
    a <- nextArr
    xR <- newITemp
    szR <- newITemp; sz'R <- newITemp
    x <- rtemp tX; y <- rtemp tX; z <- rtemp tD
    (lX, plX) <- aeval xs xR
    let (aX,dX)=ax x; (aY,dY)=ax y
    i <- newITemp
    ss <- writeRF op (aX.aY$[]) (dX.dY$[]) z
    let loopBody = mt (AElem xR 1 (Tmp i+1) lX 8) x:mt (AElem xR 1 (Tmp i) lX 8) y:ss++[wt (AElem t 1 (Tmp i) (Just a) 8) z]
        loop=For i 0 ILt (Tmp sz'R) loopBody
    modify (addMT a t)
    pure (Just a, plX++MT szR (EAt (ADim xR 0 lX)):MT sz'R (Tmp szR-1):Ma a t 1 (Tmp sz'R) 8:Wr (ADim t 0 (Just a)) (Tmp sz'R):[loop])
aeval (EApp oTy (Builtin _ RevE) e) t | Just ty <- if1 oTy = do
    a <- nextArr
    eR <- newITemp; n <- newITemp; i <- newITemp; o <- rtemp ty
    (lE, plE) <- aeval e eR
    let loop=For i 0 ILt (Tmp n) [mt (AElem eR 1 (Tmp n-Tmp i-1) lE 8) o, wt (AElem t 1 (Tmp i) (Just a) 8) o]
    modify (addMT a t)
    pure (Just a, plE++MT n (EAt (ADim eR 0 lE)):Ma a t 1 (Tmp n) 8:Wr (ADim t 0 (Just a)) (Tmp n):[loop])
aeval e _ = error (show e)

plEV :: E (T ()) -> CM ([CS] -> [CS], Temp)
plEV (Var I x) = do
    st <- gets vars
    pure (id, getT st x)
plEV e = do
    t <- newITemp
    pl <- eval e t
    pure ((pl++), t)

eval :: E (T ()) -> Temp -> CM [CS]
eval (LLet _ (n,e') e) t = do
    eR <- newITemp
    plE <- eval e' eR
    modify (addVar n eR)
    (plE++) <$> eval e t
eval (ILit _ n) t = pure [MT t (fromInteger n)]
eval (Var _ x) t = do
    st <- gets vars
    pure [MT t (Tmp $ getT st x)]
eval (EApp _ (EApp _ (EApp _ (Builtin _ FoldS) op) seed) e) acc | (Arrow _ (Arrow tX _)) <- eAnn op, isIF tX = do
    x <- rtemp tX
    eR <- newITemp
    szR <- newITemp
    i <- newITemp
    (l, plE) <- aeval e eR
    plAcc <- eval seed acc
    let (aX,dX)=ax x
    ss <- writeRF op (aX [acc]) (dX []) (Right acc)
    let loopBody=mt (AElem eR 1 (Tmp i) l 8) x:ss
        loop=For i 0 ILt (Tmp szR) loopBody
    pure $ plE++plAcc++MT szR (EAt (ADim eR 0 l)):[loop]
eval (EApp _ (EApp _ (Builtin _ op) e0) e1) t | Just cop <- mOp op = do
    (pl0,t0) <- plEV e0; (pl1,t1) <- plEV e1
    pure $ pl0 $ pl1 [MT t (Bin cop (Tmp t0) (Tmp t1))]
eval (EApp _ (EApp _ (Builtin _ Max) e0) e1) t = do
    (pl0,t0) <- plEV e0
    -- in case t==t1
    t1 <- newITemp
    pl1 <- eval e1 t1
    pure $ pl0 $ pl1 ++ [MT t (Tmp t0), Cmov (IRel IGt (Tmp t1) (Tmp t0)) t (Tmp t1)]
eval (EApp _ (EApp _ (Builtin _ Min) e0) e1) t = do
    (pl0,t0) <- plEV e0
    -- in case t==t1
    t1 <- newITemp
    pl1 <- eval e1 t1
    pure $ pl0 $ pl1 ++ [MT t (Tmp t0), Cmov (IRel ILt (Tmp t1) (Tmp t0)) t (Tmp t1)]
eval (EApp _ (EApp _ (Builtin (Arrow I _) Eq) e0) e1) t = do
    e0R <- newITemp; e1R <- newITemp
    plE0 <- eval e0 e0R; plE1 <- eval e1 e1R
    pure $ plE0 ++ plE1 ++ [Cset (IRel IEq (Tmp e0R) (Tmp e1R)) t]
eval (EApp _ (EApp _ (Builtin (Arrow I _) Neq) e0) e1) t = do
    e0R <- newITemp; e1R <- newITemp
    plE0 <- eval e0 e0R; plE1 <- eval e1 e1R
    pure $ plE0 ++ plE1 ++ [Cset (IRel INeq (Tmp e0R) (Tmp e1R)) t]
eval (EApp _ (EApp _ (Builtin (Arrow I _) Gt) e0) e1) t = do
    e0R <- newITemp; e1R <- newITemp
    plE0 <- eval e0 e0R; plE1 <- eval e1 e1R
    pure $ plE0 ++ plE1 ++ [Cset (IRel IGt (Tmp e0R) (Tmp e1R)) t]
eval (EApp _ (EApp _ (Builtin (Arrow I _) Lt) e0) e1) t = do
    e0R <- newITemp; e1R <- newITemp
    plE0 <- eval e0 e0R; plE1 <- eval e1 e1R
    pure $ plE0 ++ plE1 ++ [Cset (IRel ILt (Tmp e0R) (Tmp e1R)) t]
eval (EApp _ (EApp _ (Builtin (Arrow I _) Gte) e0) e1) t = do
    e0R <- newITemp; e1R <- newITemp
    plE0 <- eval e0 e0R; plE1 <- eval e1 e1R
    pure $ plE0 ++ plE1 ++ [Cset (IRel IGeq (Tmp e0R) (Tmp e1R)) t]
eval (EApp _ (EApp _ (Builtin (Arrow I _) Lte) e0) e1) t = do
    e0R <- newITemp; e1R <- newITemp
    plE0 <- eval e0 e0R; plE1 <- eval e1 e1R
    pure $ plE0 ++ plE1 ++ [Cset (IRel ILeq (Tmp e0R) (Tmp e1R)) t]
eval (EApp _ (EApp _ (Builtin (Arrow F _) Eq) e0) e1) t = do
    e0R <- newFTemp; e1R <- newFTemp
    plE0 <- feval e0 e0R; plE1 <- feval e1 e1R
    pure $ plE0 ++ plE1 ++ [Cset (FRel FEq (FTmp e0R) (FTmp e1R)) t]
eval (EApp _ (EApp _ (Builtin (Arrow F _) Neq) e0) e1) t = do
    e0R <- newFTemp; e1R <- newFTemp
    plE0 <- feval e0 e0R; plE1 <- feval e1 e1R
    pure $ plE0 ++ plE1 ++ [Cset (FRel FNeq (FTmp e0R) (FTmp e1R)) t]
eval (EApp _ (EApp _ (Builtin (Arrow F _) Gt) e0) e1) t = do
    e0R <- newFTemp; e1R <- newFTemp
    plE0 <- feval e0 e0R; plE1 <- feval e1 e1R
    pure $ plE0 ++ plE1 ++ [Cset (FRel FGt (FTmp e0R) (FTmp e1R)) t]
eval (EApp _ (EApp _ (Builtin (Arrow F _) Lt) e0) e1) t = do
    e0R <- newFTemp; e1R <- newFTemp
    plE0 <- feval e0 e0R; plE1 <- feval e1 e1R
    pure $ plE0 ++ plE1 ++ [Cset (FRel FLt (FTmp e0R) (FTmp e1R)) t]
eval (EApp _ (EApp _ (Builtin (Arrow F _) Gte) e0) e1) t = do
    e0R <- newFTemp; e1R <- newFTemp
    plE0 <- feval e0 e0R; plE1 <- feval e1 e1R
    pure $ plE0 ++ plE1 ++ [Cset (FRel FGeq (FTmp e0R) (FTmp e1R)) t]
eval (EApp _ (EApp _ (Builtin (Arrow F _) Lte) e0) e1) t = do
    e0R <- newFTemp; e1R <- newFTemp
    plE0 <- feval e0 e0R; plE1 <- feval e1 e1R
    pure $ plE0 ++ plE1 ++ [Cset (FRel FLeq (FTmp e0R) (FTmp e1R)) t]
eval (EApp _ (Builtin _ Head) xs) t = do
    a <- newITemp
    (l, plX) <- aeval xs a
    pure $ plX ++ [MT t (EAt (AElem a 1 0 l 8))]
eval (EApp _ (Builtin _ Last) xs) t = do
    a <- newITemp
    (l, plX) <- aeval xs a
    pure $ plX ++ [MT t (EAt (AElem a 1 (EAt (ADim a 0 l)-1) l 8))]
eval (EApp _ (Builtin _ Size) xs) t = do
    xsR <- newITemp
    (l, plE) <- aeval xs xsR
    rnkR <- newITemp; i <- newITemp
    pure $ plE ++ [MT rnkR (EAt (ARnk xsR l)), MT t (EAt (ADim xsR 0 l)), For i 1 ILt (Tmp rnkR) [MT t (Tmp t*EAt (ADim xsR (Tmp i) l))]]
eval (EApp _ (Builtin _ Floor) x) t = do
    xR <- newFTemp
    plX <- feval x xR
    pure $ plX ++ [MT t (CFloor (FTmp xR))]
eval e _          = error (show e)

mFrel :: Builtin -> Maybe FRel
mFrel Gte=Just FGeq; mFrel Lte=Just FLeq; mFrel Eq=Just FEq; mFrel Neq=Just FNeq; mFrel Lt=Just FLt; mFrel Gt=Just FGt; mFrel _=Nothing

mFop :: Builtin -> Maybe FBin
mFop Plus=Just FPlus; mFop Times=Just FTimes; mFop Minus= Just FMinus; mFop Div=Just FDiv; mFop Exp=Just FExp; mFop Max=Just FMax; mFop Min=Just FMin; mFop _=Nothing

mOp :: Builtin -> Maybe IBin
mOp Plus=Just IPlus; mOp Times=Just ITimes; mOp Minus=Just IMinus; mOp _=Nothing

mFun :: Builtin -> Maybe FUn
mFun Sqrt=Just FSqrt; mFun Log=Just FLog; mFun Sin=Just FSin; mFun Cos=Just FCos; mFun Abs=Just FAbs; mFun _=Nothing

cond :: E (T ()) -> E (T ()) -> E (T ()) -> Either FTemp Temp -> CM (Maybe Int, [CS])
cond (EApp _ (EApp _ (Builtin (Arrow F _) rel) c0) c1) e0 e1 t | Just frel <- mFrel rel, isIF (eAnn e0) = do
    c0R <- newFTemp; c1R <- newFTemp
    plC0 <- feval c0 c0R; plC1 <- feval c1 c1R
    plE0 <- eeval e0 t; plE1 <- eeval e1 t
    pure (Nothing, plC0 ++ plC1 ++ [If (FRel frel (FTmp c0R) (FTmp c1R)) plE0 plE1])
cond p e0 e1 t | isIF (eAnn e0) = do
    pR <- newITemp
    plP <- eval p pR; plE0 <- eeval e0 t; plE1 <- eeval e1 t
    pure (Nothing, plP ++ [If (Is pR) plE0 plE1])

feval :: E (T ()) -> FTemp -> CM [CS]
feval (LLet _ (n,e') e) t | isF (eAnn e') = do
    eR <- newFTemp
    plE <- feval e' eR
    modify (addD n eR)
    (plE++) <$> feval e t
feval (LLet _ (n,e') e) t | isArr (eAnn e') = do
    t' <- newITemp
    (l, ss) <- aeval e' t'
    modify (addAVar n (l, t'))
    (ss ++) <$> feval e t
feval (ILit _ x) t = pure [MX t (ConstF $ fromIntegral x)] -- if it overflows you deserve it
feval (FLit _ x) t = pure [MX t (ConstF x)]
feval (Var _ x) t = do
    st <- gets dvars
    pure [MX t (FTmp $ getT st x)]
feval (EApp _ (EApp _ (Builtin _ op) (Var _ x0)) (Var _ x1)) t | Just fb <- mFop op = do
    st <- gets dvars
    pure [MX t (FBin fb (FTmp $ getT st x0) (FTmp $ getT st x1))]
feval (EApp _ (EApp _ (Builtin _ op) e0) (Var _ x)) t | Just fb <- mFop op = do
    st <- gets dvars
    t0 <- newFTemp
    pl0 <- feval e0 t0
    pure $ pl0 ++ [MX t (FBin fb (FTmp t0) (FTmp (getT st x)))]
feval (EApp _ (EApp _ (Builtin _ op) e0) e1) t | Just fb <- mFop op = do
    t0 <- newFTemp; t1 <- newFTemp
    pl0 <- feval e0 t0; pl1 <- feval e1 t1
    pure $ pl0 ++ pl1 ++ [MX t (FBin fb (FTmp t0) (FTmp t1))]
feval (EApp _ (EApp _ (Builtin _ IntExp) (FLit _ (-1))) n) t = do
    nR <- newITemp
    plR <- eval n nR
    pure $ plR ++ [MX t 1, Fcmov (IUn IOdd (Tmp nR)) t (ConstF (-1))]
feval (EApp _ (EApp _ (Builtin _ IntExp) x) n) t = do
    xR <- newFTemp; nR <- newITemp
    plX <- feval x xR; plN <- eval n nR
    pure $ plX ++ plN ++ [MX t 1, While nR IGt 0 [Ifn't (IUn IEven (Tmp nR)) [MX t (FTmp t*FTmp xR)], MT nR (Bin IAsr (Tmp nR) 1), MX xR (FTmp xR*FTmp xR)]]
feval (EApp _ (Builtin _ f) e) t | Just ff <- mFun f = do
    eR <- newFTemp
    plE <- feval e eR
    pure $ plE ++ [MX t (FUn ff (FTmp eR))]
feval (EApp _ (Builtin _ Neg) x) t = do
    fR <- newFTemp
    plX <- feval x fR
    pure $ plX ++ [MX t (negate (FTmp fR))]
feval (EApp _ (Builtin _ ItoF) e) t = do
    iR <- newITemp
    pl<- eval e iR
    pure $ pl ++ [MX t (IE $ Tmp iR)]
feval (Cond _ p e0 e1) t = snd <$> cond p e0 e1 (Left t)
feval (EApp _ (Builtin _ Head) xs) t = do
    a <- newITemp
    (l, plX)  <-  aeval xs a
    pure $ plX ++ [MX t (FAt (AElem a 1 0 l 8))]
feval (EApp _ (Builtin _ Last) xs) t = do
    a <- newITemp
    (l, plX) <- aeval xs a
    pure $ plX ++ [MX t (FAt (AElem a 1 (EAt (ADim a 0 l)-1) l 8))]
feval (Id _ (FoldOfZip zop op [p])) acc | Just tP <- if1 (eAnn p) = do
    x <- rtemp tP
    pR <- newITemp
    szR <- newITemp
    i <- newITemp
    (lP, plP) <- aeval p pR
    let (aX,dX)=ax x
    ss <- writeRF op (aX []) ((acc:).dX$[]) (Left acc)
    let step = mt (AElem  pR 1 (Tmp i) lP 8) x:ss
        loop = For i 1 ILt (Tmp szR) step
    sseed <- writeRF zop (aX []) (dX []) (Left acc)
    pure $ plP++MT szR (EAt (ADim pR 0 lP)):mt (AElem pR 1 0 lP 8) x:sseed++[loop]
feval (Id _ (FoldOfZip zop op [EApp _ (EApp _ (EApp _ (Builtin _ IRange) start) _) incr, ys])) acc | Just tQ <- if1 (eAnn ys) = do
    x <- newITemp; yR <- newITemp; y <- rtemp tQ
    incrR <- newITemp; szR <- newITemp; i <- newITemp
    plX <- eval start x; plY <- eeval (EApp tQ (Builtin undefined Head) ys) y; (lY, plYs) <- aeval ys yR; plI <- eval incr incrR
    let (aY,dY)=ax y
    seed <- writeRF zop ((x:).aY$[]) (dY []) (Left acc)
    ss <- writeRF op ((x:).aY$[]) ((acc:).dY$[]) (Left acc)
    pure $ plX ++ plY ++ plYs ++ seed ++ plI ++ MT szR (EAt (ADim yR 0 lY)):[For i 1 ILt (Tmp szR) (mt (AElem yR 1 (Tmp i) lY 8) y:MT x (Tmp x+Tmp incrR):ss)]
feval (Id _ (FoldOfZip zop op [p, q])) acc | Just tP <- if1 (eAnn p), Just tQ <- if1 (eAnn q) = do
    x <- rtemp tP; y <- rtemp tQ
    pR <- newITemp; qR <- newITemp
    szR <- newITemp
    i <- newITemp
    (lP, plP) <- aeval p pR; (lQ, plQ) <- aeval q qR
    let (aX,dX)=ax x; (aY,dY)=ax y
    ss <- writeRF op (aX.aY$[]) ((acc:).dX.dY$[]) (Left acc)
    let step = mt (AElem pR 1 (Tmp i) lP 8) x:mt (AElem qR 1 (Tmp i) lQ 8) y:ss
        loop = For i 1 ILt (Tmp szR) step
    seed <- writeRF zop (aX.aY$[]) (dX.dY$[]) (Left acc)
    pure $ plP++plQ++MT szR (EAt (ADim pR 0 lP)):mt (AElem pR 1 0 lP 8) x:mt (AElem qR 1 0 lQ 8) y:seed++[loop]
feval (EApp _ (EApp _ (Builtin _ Fold) op) e) acc | (Arrow tX _) <- eAnn op, isF tX = do
    x <- newFTemp
    aP <- newITemp
    szR <- newITemp
    i <- newITemp
    (l, plE) <- aeval e aP
    ss <- writeRF op [] [acc, x] (Left acc)
    let loopBody=MX x (FAt (AElem aP 1 (Tmp i) l 8)):ss
        loop=For i 1 ILt (Tmp szR) loopBody
    pure $ plE++MT szR (EAt (ADim aP 0 l)):MX acc (FAt (AElem aP 1 0 l 8)):[loop]
feval (EApp _ (EApp _ (EApp _ (Builtin _ Foldl) op) seed) e) acc | (Arrow _ (Arrow tX _)) <- eAnn op, isIF tX = do
    x <- rtemp tX
    eR <- newITemp
    i <- newITemp
    (l, plE) <- aeval e eR
    plAcc <- feval seed acc
    let (aX,dX)=ax x
    ss <- writeRF op (aX []) (dX [acc]) (Left acc)
    let loopBody=mt (AElem eR 1 (Tmp i) l 8) x:ss++[MT i (Tmp i-1)]
        loop=While i IGeq 0 loopBody
    pure $ plE++plAcc++MT i (EAt (ADim eR 0 l)-1):[loop]
feval (EApp _ (EApp _ (EApp _ (Builtin _ FoldS) op) seed) (EApp _ (EApp _ (EApp _ (Builtin _ IRange) start) end) incr)) acc = do
    i <- newITemp
    endR <- newITemp; incrR <- newITemp
    plStart <- eval start i; plAcc <- feval seed acc; plEnd <- eval end endR; plIncr <- eval incr incrR
    ss <- writeRF op [i] [acc] (Left acc)
    pure $ plStart ++ plAcc ++ plEnd ++ plIncr ++ [While i ILeq (Tmp endR) (ss++[MT i (Tmp i+Tmp incrR)])]
feval (EApp _ (EApp _ (EApp _ (Builtin _ FoldS) op) seed) (EApp _ (EApp _ (EApp _ (Builtin _ FRange) start) end) nSteps)) acc = do
    i <- newITemp; startR <- newFTemp; incrR <- newFTemp; xR <- newFTemp; endI <- newITemp
    plStart <- feval start startR
    plAcc <- feval seed acc
    plEnd <- eval nSteps endI
    plIncr <- feval ((end `eMinus` start) `eDiv` (EApp F (Builtin (Arrow I F) ItoF) nSteps `eMinus` FLit F 1)) incrR
    ss <- writeRF op [] [acc, xR] (Left acc)
    pure $ plStart ++ MX xR (FTmp startR):plEnd++plIncr++plAcc++[For i 0 ILt (Tmp endI) (ss++[MX xR (FTmp xR+FTmp incrR)])]
feval (EApp _ (EApp _ (EApp _ (Builtin _ FoldS) op) seed) e) acc | (Arrow _ (Arrow tX _)) <- eAnn op, isIF tX = do
    x <- rtemp tX
    eR <- newITemp
    szR <- newITemp
    i <- newITemp
    (l, plE) <- aeval e eR
    plAcc <- feval seed acc
    let (aX,dX)=ax x
    ss <- writeRF op (aX []) (dX [acc]) (Left acc)
    let loopBody=mt (AElem eR 1 (Tmp i) l 8) x:ss
        loop=For i 0 ILt (Tmp szR) loopBody
    pure $ plE++plAcc++MT szR (EAt (ADim eR 0 l)):[loop]
feval e _ = error (show e)
