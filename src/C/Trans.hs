{-# LANGUAGE TupleSections #-}

module C.Trans ( writeC ) where

import           A
import           C
import           CF.AL                      (AL (..))
import qualified CF.AL                      as AL
import           Control.Composition        (thread)
import           Control.Monad              (zipWithM)
import           Control.Monad.State.Strict (State, gets, modify, runState, state)
import           Data.Bifunctor             (first, second)
import           Data.Int                   (Int64)
import qualified Data.IntMap                as IM
import qualified Data.IntSet                as IS
import           Data.List                  (scanl')
import           Data.Maybe                 (catMaybes)
import           Data.Word                  (Word64)
import           GHC.Float                  (castDoubleToWord64)
import           Nm
import           Nm.IntMap
import           Op

data CSt = CSt { tempU       :: !Int
               , arrU        :: !AL
               , assemblerSt :: !Int
               , vars        :: IM.IntMap Temp -- track vars so that (Var x) can be replaced at the site
               , dvars       :: IM.IntMap FTemp
               , avars       :: IM.IntMap (Maybe AL, Temp)
               , fvars       :: IM.IntMap (Label, [(Maybe Int, Temp)], (Maybe Int, Temp))
               , _aa         :: AsmData
               , mts         :: IM.IntMap Temp
               }

nextI :: CM Int
nextI = state (\(CSt tϵ ar as v d a f aas ts) -> (tϵ, CSt (tϵ+1) ar as v d a f aas ts))

nextArr :: Temp -> CM AL
nextArr r = state (\(CSt t a@(AL i) as v d aϵ f aas ts) -> (a, CSt t (AL$i+1) as v d aϵ f aas (AL.insert a r ts)))

nextAA :: CM Int
nextAA = state (\(CSt t ar as v d a f aas ts) -> (as, CSt t ar (as+1) v d a f aas ts))

newITemp :: CM Temp
newITemp = ITemp <$> nextI

newFTemp :: CM FTemp
newFTemp = FTemp <$> nextI

addAA :: Int -> [Word64] -> CSt -> CSt
addAA i aa (CSt t ar as v d a f aas ts) = CSt t ar as v d a f (IM.insert i aa aas) ts

addVar :: Nm a -> Temp -> CSt -> CSt
addVar n r (CSt t ar as v d a f aas ts) = CSt t ar as (insert n r v) d a f aas ts

addD :: Nm a -> FTemp -> CSt -> CSt
addD n r (CSt t ar as v d a f aas ts) = CSt t ar as v (insert n r d) a f aas ts

addAVar :: Nm a -> (Maybe AL, Temp) -> CSt -> CSt
addAVar n r (CSt t ar as v d a f aas ts) = CSt t ar as v d (insert n r a) f aas ts

getT :: IM.IntMap b -> Nm a -> b
getT st n = findWithDefault (error ("Internal error: variable " ++ show n ++ " not assigned to a temp.")) n st

type CM = State CSt

tick t = MT t (Tmp t+1)

fop op e0 = EApp F (EApp (F ~> F) (Builtin (F ~> F ~> F) op) e0)
eMinus = fop Minus
eDiv = fop Div

isF, isI, isIF :: T a -> Bool
isF F = True; isF _ = False
isI I = True; isI _ = False
isArr Arr{}=True; isArr _=False
isIF I=True; isIF F=True; isIF _=False
isΠIF (P ts)=all isIF ts; isΠIF _=False

rel :: Builtin -> Maybe IRel
rel Eq=Just IEq; rel Neq=Just INeq; rel Lt=Just ILt; rel Gt=Just IGt; rel Lte=Just ILeq; rel Gte=Just IGeq; rel _=Nothing

mIF :: T a -> Maybe (T a)
mIF (Arr _ F)=Just F; mIF (Arr _ I)=Just I; mIF _=Nothing

if1 :: T a -> Maybe (T a)
if1 (Arr (_ `Cons` Nil) I) = Just I; if1 (Arr (_ `Cons` Nil) F) = Just F; if1 _ = Nothing

if1p :: T a -> Bool
if1p t | Just{} <- if1 t = True | otherwise = False

mAA :: T a -> Maybe ((T a, Int64), (T a, Int64))
mAA (Arrow t0 t1) = (,) <$> tRnk t0 <*> tRnk t1
mAA _             = Nothing

f1 :: T a -> Bool
f1 (Arr (_ `Cons` Nil) F) = True; f1 _ = False

bT :: Integral b => T a -> b
bT (P ts)=sum (bT<$>ts); bT F=8; bT I=8

szT = scanl' (\off ty -> off+bT ty::Int64) 0

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
writeC = π.flip runState (CSt 0 (AL 0) 0 IM.empty IM.empty IM.empty IM.empty IM.empty IM.empty) . writeCM . fmap rLi where π (s, CSt t _ _ _ _ _ _ aa a) = (s, LSt 0 t, aa, a)

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
             | isI (eAnn e) = do {t <- newITemp; (++[MT CRet (Tmp t)]) <$> eval e t} -- avoid clash when calling functions
             | isArr (eAnn e) = do {i <- newITemp; (l,r) <- aeval e i; pure$r++[MT CRet (Tmp i)]++case l of {Just m -> [RA m]; Nothing -> []}}
             | P [F,F] <- eAnn e = do {t <- newITemp; (_,_,_,p) <- πe e t; pure$Sa t 16:p++[MX FRet0 (FAt (Raw t 0 Nothing 8)), MX FRet1 (FAt (Raw t 1 Nothing 8)), Pop 16]}
             | ty@P{} <- eAnn e, b64 <- bT ty, (n,0) <- b64 `quotRem` 8 = let b=ConstI b64 in do {t <- newITemp; a <- nextArr CRet; (_,_,ls,pl) <- πe e t; pure (Sa t b:pl++MaΠ a CRet b:CpyE (Raw CRet 0 (Just a) 8) (Raw t 0 Nothing 8) (ConstI n) 8:Pop b:RA a:(RA<$>ls))}

rtemp :: T a -> CM (Either FTemp Temp)
rtemp F=Left<$>newFTemp; rtemp I=Right<$>newITemp

writeF :: E (T ())
       -> [Arg]
       -> Either FTemp Temp
       -> CM (Maybe AL, [CS])
writeF (Lam _ x e) (AA r l:rs) ret = do
    modify (addAVar x (l,r))
    writeF e rs ret
writeF (Lam _ x e) (IPA r:rs) ret = do
    modify (addVar x r)
    writeF e rs ret
writeF (Lam _ x e) (FA fr:rs) ret = do
    modify (addD x fr)
    writeF e rs ret
writeF e [] (Right r) | isArr (eAnn e) = aeval e r
writeF e [] (Right r) | isI (eAnn e) = (Nothing,)<$>eval e r
writeF e [] (Right r) | isΠIF (eAnn e) = (\ ~(_,_,_,ss) -> (Nothing, ss))<$>πe e r
writeF e [] (Left r) = (Nothing,)<$>feval e r

writeRF :: E (T ()) -> [Either FTemp Temp] -> Either FTemp Temp -> CM [CS]
writeRF e args = fmap snd.writeF e (ra<$>args)

data Arg = IPA !Temp | FA !FTemp | AA !Temp (Maybe AL)

mt :: ArrAcc -> Either FTemp Temp -> CS
mt p = either (`MX` FAt p) (`MT` EAt p)

wt :: ArrAcc -> Either FTemp Temp -> CS
wt p = either (WrF p.FTmp) (Wr p.Tmp)

ra (Left f)=FA f; ra (Right r)=IPA r

eeval :: E (T ()) -> Either FTemp Temp -> CM [CS]
eeval e = either (feval e) (eval e)

data RI a b = Cell a | Index b deriving Show

part :: [RI a b] -> ([a], [b])
part []           = ([], [])
part (Cell i:is)  = first (i:) $ part is
part (Index i:is) = second (i:) $ part is

plDim :: Int64 -> (Temp, Maybe AL) -> CM ([Temp], [CS])
plDim rnk (a,l) =
    unzip <$> traverse (\at -> do {dt <- newITemp; pure (dt, MT dt (EAt at))}) [ ADim a (ConstI i) l | i <- [0..rnk-1] ]

offByDim :: [Temp] -> CM ([Temp], [CS])
offByDim dims = do
    sts <- traverse (\_ -> newITemp) (undefined:dims)
    let ss=zipWith3 (\s1 s0 d -> MT s1 (Tmp s0*Tmp d)) (tail sts) sts dims
    pure (reverse sts, MT (head sts) 1:ss)
    -- drop 1 for strides

data Cell a b = Fixed -- set by the larger procedure
              | Bound b -- to be iterated over

forAll is bs = thread (zipWith (\t b -> (:[]) . For t 0 ILt (Tmp b)) is bs)

-- the resulting expressions/statement contain free variables that will be iterated over in the main rank-ification loop, these free variables are returned alongside
extrCell :: [Cell () Temp] -> [Temp] -> (Temp, Maybe AL) -> Temp -> CM ([Temp], [CS])
extrCell fixBounds sstrides (srcP, srcL) dest = do
    (dims, ts, arrIxes, complts) <- switch fixBounds
    t <- newITemp; i <- newITemp
    pure (complts, (MT i 0:) $ forAll ts dims
        [MT t (EAt (At srcP (Tmp<$>sstrides) (Tmp<$>arrIxes) srcL 8)), Wr (Raw dest (Tmp i) Nothing 8) (Tmp t), tick i])
    where switch (Bound d:ds) = do {t <- newITemp; qmap (d:) (t:) (t:) id <$> switch ds}
          switch (Fixed:ds)   = do {f <- newITemp; qmap id id (f:) (f:) <$> switch ds}
          switch []           = pure ([], [], [], [])

aeval :: E (T ()) -> Temp -> CM (Maybe AL, [CS])
aeval (LLet _ (n,e') e) t | isArr (eAnn e') = do
    t' <- newITemp
    (l, ss) <- aeval e' t'
    modify (addAVar n (l, t'))
    second (ss ++) <$> aeval e t
aeval (Var _ x) t = do
    st <- gets avars
    let (i, r) = getT st x
    pure (i, [MT t (Tmp r)])
aeval (EApp _ (EApp _ (Builtin _ Map) op) e) t | (Arrow tD tC) <- eAnn op, isIF tD && isIF tC= do
    a <- nextArr t
    arrT <- newITemp
    (l, plE) <- aeval e arrT
    -- rank 1
    let sz=EAt (ADim arrT 0 l)
    rC <- rtemp tC; rD <- rtemp tD
    ss <- writeRF op [rD] rC
    iR <- newITemp; szR <- newITemp
    let loopBody=mt (AElem arrT 1 (Tmp iR) l 8) rD:ss++[wt (AElem t 1 (Tmp iR) (Just a) 8) rC]
        loop=For iR 0 ILt (Tmp szR) loopBody
    pure (Just a, plE ++ MT szR sz:Ma a t 1 (Tmp szR) 8:Wr (ADim t 0 (Just a)) (Tmp szR):[loop])
aeval (EApp _ (EApp _ (Builtin _ Map) f) xs) t | Just (_, xRnk) <- tRnk (eAnn xs), Just ((ta0, rnk0), (ta1, rnk1)) <- mAA (eAnn f), isIF ta0 && isIF ta1 = do
    a <- nextArr t
    slopP <- newITemp; y <- newITemp
    xR <- newITemp; szR <- newITemp; slopSz <- newITemp; szY <- newITemp
    i <- newITemp; j <- newITemp; k <- newITemp; kL <- newITemp; xd <- newITemp; td <- newITemp
    (lX, plX) <- aeval xs xR
    (lY, ss) <- writeF f [AA slopP Nothing] (Right y)
    let slopDims=[EAt (ADim xR (ConstI l) lX) | l <- [rnk0..(xRnk-1)]]
        xDims=[EAt (ADim xR (ConstI l) lX) | l <- [0..(rnk0-1)]]
        yDims=[EAt (ADim y (ConstI l) lY) | l <- [0..(rnk1-1)]]
        slopE=Bin IAsl (Tmp slopSz) 3+fromIntegral (8+8*rnk0)
        dimsFromIn=ConstI$xRnk-rnk0
        oRnk=xRnk-rnk0+rnk1
        -- step=[MT i (Tmp i+Tmp slopSz), MT j (Tmp j+Tmp szY)]
        step=CpyE (AElem slopP (ConstI rnk0) 0 Nothing 8) (Raw xd (Tmp i) lX 8) (Tmp slopSz) 8:ss++[CpyE (Raw td (Tmp j) (Just a) 8) (Raw y 0 lY 8) (Tmp szY) 8, MT i (Tmp i+Tmp slopSz), MT j (Tmp j+Tmp szY)]
        loop=For k 0 ILt (Tmp kL) step
    pure (Just a,
        plX++PlProd slopSz slopDims:Sa slopP slopE:zipWith (\d n -> Wr (ADim slopP (ConstI n) Nothing) d) slopDims [0..]
        ++MT xd (DP xR (ConstI xRnk))
        :CpyE (AElem slopP (ConstI rnk0) 0 Nothing 8) (Raw xd 0 lX 8) (Tmp slopSz) 8
        :ss
        ++PlProd szR (xDims++yDims)
        :Ma a t (ConstI oRnk) (Tmp szR) 8:CpyD (ADim t 0 (Just a)) (ADim xR 0 lX) dimsFromIn:CpyD (ADim t dimsFromIn (Just a)) (ADim y 0 lY) (ConstI rnk1)
        :MT td (DP t (ConstI oRnk))
        :PlProd szY yDims:PlProd kL xDims
        :MT i 0:MT j 0:loop
        :[Pop slopE])
aeval (EApp _ (EApp _ (Builtin _ (Rank [(0, _)])) f) xs) t | (Arrow tX tY) <- eAnn f, isIF tX && isIF tY = do
    a <- nextArr t
    xR <- newITemp; rnkR <- newITemp; szR <- newITemp
    i <- newITemp; x <- rtemp tX; y <- rtemp tY; xRd <- newITemp; tD <- newITemp
    (lX, plX) <- aeval xs xR
    ss <- writeRF f [x] y
    let step=mt (Raw xRd (Tmp i) lX 8) x:ss++[wt (Raw tD (Tmp i) (Just a) 8) y]
        loop=For i 0 ILt (Tmp szR) step
    pure (Just a, plX++MT rnkR (EAt (ARnk xR lX)):SZ szR xR (Tmp rnkR) lX:Ma a t (Tmp rnkR) (Tmp szR) 8:CpyD (ADim t 0 (Just a)) (ADim xR 0 lX) (Tmp rnkR):MT xRd (DP xR (Tmp rnkR)):MT tD (DP t (Tmp rnkR)):[loop])
aeval (EApp _ (EApp _ (EApp _ (Builtin _ (Rank [(0, _), (0, _)])) op) xs) ys) t | Arrow tX (Arrow tY tC) <- eAnn op, isIF tX && isIF tY && isIF tC = do
    a <- nextArr t
    xR <- newITemp; yR <- newITemp; rnkR <- newITemp; szR <- newITemp
    x <- rtemp tX; y <- rtemp tY; z <- rtemp tC; xRd <- newITemp; yRd <- newITemp; tD <- newITemp
    (lX, plX) <- aeval xs xR
    (lY, plY) <- aeval ys yR
    i <- newITemp
    ss <- writeRF op [x,y] z
    let step=mt (Raw xRd (Tmp i) lX 8) x:mt (Raw yRd (Tmp i) lY 8) y:ss++[wt (Raw tD (Tmp i) (Just a) 8) z]
        loop=For i 0 ILt (Tmp szR) step
    pure (Just a, plX ++ plY ++ MT rnkR (EAt (ARnk xR lX)):SZ szR xR (Tmp rnkR) lX:Ma a t (Tmp rnkR) (Tmp szR) 8:CpyD (ADim t 0 (Just a)) (ADim xR 0 lX) (Tmp rnkR):MT xRd (DP xR (Tmp rnkR)):MT yRd (DP yR (Tmp rnkR)):MT tD (DP t (Tmp rnkR)):[loop])
aeval (EApp tO (EApp _ (Builtin _ (Rank [(cr, Just ixs)])) f) xs) t | Just (tA, rnk) <- tRnk (eAnn xs)
                                                                    , Just tOR <- mIF tO
                                                                    , (Arrow _ tF) <- eAnn f
                                                                    , isIF tF && isIF tA = do
    a <- nextArr t
    xR <- newITemp
    (lX, plX) <- aeval xs xR
    slopP <- newITemp; y <- rtemp tOR
    let ixsIs = IS.fromList ixs; allIx = [ if ix `IS.member` ixsIs then Cell() else Index() | ix <- [1..fromIntegral rnk] ]
    oSz <- newITemp; slopSz <- newITemp
    (dts, dss) <- plDim rnk (xR, lX)
    (sts, sssϵ) <- offByDim (reverse dts)
    let _:sstrides = sts; sss=init sssϵ
    let allts = fmap (\i -> case i of {Cell{} -> Cell(); Index{} -> Index()}) allIx
    let allDims = zipWith (\ix dt -> case ix of {Cell{} -> Cell dt; Index{} -> Index dt}) allIx dts
        ~(oDims, complDims) = part allDims
        oRnk=rnk-fromIntegral cr; slopRnk=rnk-oRnk
    (_, ss) <- writeF f [AA slopP Nothing] y
    let ecArg = zipWith (\d tt -> case (d,tt) of (dϵ,Index{}) -> Bound dϵ; (_,Cell{}) -> Fixed) dts allts
    xRd <- newITemp; slopPd <- newITemp
    (complts, place) <- extrCell ecArg sstrides (xRd, lX) slopPd
    di <- newITemp
    let loop=forAll complts oDims $ place ++ ss ++ [wt (AElem t (ConstI oRnk) (Tmp di) Nothing 8) y, tick di]
    pure (Just a, plX++dss++PlProd oSz (Tmp<$>oDims):Ma a t (ConstI oRnk) (Tmp oSz) 8:zipWith (\d i -> Wr (ADim t (ConstI i) (Just a)) (Tmp d)) oDims [0..]++PlProd slopSz (Tmp<$>complDims):MT slopSz (Tmp slopSz+ConstI (slopRnk+1)):Sa slopP (Tmp slopSz):Wr (ARnk slopP Nothing) (ConstI slopRnk):zipWith (\d i -> Wr (ADim slopP (ConstI i) Nothing) (Tmp d)) complDims [0..]++sss++MT xRd (DP xR (ConstI rnk)):MT slopPd (DP slopP (ConstI slopRnk)):MT di 0:loop++[Pop (Tmp slopSz)])
aeval (EApp tO (EApp _ (Builtin _ (Rank [(cr, Just ixs)])) f) xs) t | Just (tA, rnk) <- tRnk (eAnn xs)
                                                                    , Just tOR <- mIF tO
                                                                    , (Arrow _ Arr{}) <- eAnn f
                                                                    , isIF tA = do
    a <- nextArr t
    xR <- newITemp
    (lX, plX) <- aeval xs xR
    slopIP <- newITemp; slopOP <- newITemp
    let ixIs = IS.fromList ixs; allIx = [ if ix `IS.member` ixIs then Cell() else Index() | ix <- [1..fromIntegral rnk] ]
    (dts,dss) <- plDim rnk (xR,lX)
    pure (Just a, plX++dss++undefined)
aeval (EApp _ (EApp _ (Builtin _ CatE) x) y) t | Just (ty, 1) <- tRnk (eAnn x) = do
    a <- nextArr t
    xR <- newITemp; yR <- newITemp
    xnR <- newITemp; ynR <- newITemp; tn <- newITemp
    let tyN=bT ty
    (lX, plX) <- aeval x xR; (lY, plY) <- aeval y yR
    pure (Just a, plX ++ plY ++ MT xnR (EAt (ADim xR 0 lX)):MT ynR (EAt (ADim yR 0 lY)):MT tn (Tmp xnR+Tmp ynR):Ma a t 1 (Tmp tn) tyN:Wr (ADim t 0 (Just a)) (Tmp tn):CpyE (AElem t 1 0 (Just a) tyN) (AElem xR 1 0 lX tyN) (Tmp xnR) tyN:[CpyE (AElem t 1 (Tmp xnR) (Just a) tyN) (AElem yR 1 0 lY tyN) (Tmp ynR) tyN])
aeval (EApp _ (EApp _ (EApp _ (Builtin _ IRange) start) end) (ILit _ 1)) t = do
    a <- nextArr t
    n <- newITemp
    startR <- newITemp; endR <- newITemp
    i <- newITemp
    pStart <- eval start startR; pEnd <- eval end endR
    let pN=MT n ((Tmp endR - Tmp startR)+1)
        loop=For i 0 ILt (Tmp n) [Wr (AElem t 1 (Tmp i) (Just a) 8) (Tmp startR), tick startR]
    pure (Just a, pStart++pEnd++pN:Ma a t 1 (Tmp n) 8:Wr (ADim t 0 (Just a)) (Tmp n):[loop])
aeval (EApp _ (EApp _ (EApp _ (Builtin _ IRange) start) end) incr) t = do
    a <- nextArr t
    n <- newITemp
    startR <- newITemp; endR <- newITemp; incrR <- newITemp
    i <- newITemp
    pStart <- eval start startR; pEnd <- eval end endR; pIncr <- eval incr incrR
    let pN=MT n (Bin Op.IDiv (Tmp endR - Tmp startR) (Tmp incrR)+1)
        loop=For i 0 ILt (Tmp n) [Wr (AElem t 1 (Tmp i) (Just a) 8) (Tmp startR), MT startR (Tmp startR+Tmp incrR)]
    pure (Just a, pStart++pEnd++pIncr++pN:Ma a t 1 (Tmp n) 8:Wr (ADim t 0 (Just a)) (Tmp n):[loop])
aeval (EApp _ (EApp _ (EApp _ (Builtin _ FRange) start) end) steps) t = do
    a <- nextArr t
    i <- newITemp
    startR <- newFTemp; incrR <- newFTemp; n <- newITemp
    putStart <- feval start startR; putN <- eval steps n
    putIncr <- feval ((end `eMinus` start) `eDiv` (EApp F (Builtin (Arrow I F) ItoF) steps `eMinus` FLit F 1)) incrR
    let loop=For i 0 ILt (Tmp n) [WrF (AElem t 1 (Tmp i) (Just a) 8) (FTmp startR), MX startR (FTmp startR+FTmp incrR)]
    pure (Just a, putStart++putIncr++putN++Ma a t 1 (Tmp n) 8:Wr (ADim t 0 (Just a)) (Tmp n):[loop])
aeval (EApp res (EApp _ (Builtin _ Cyc) xs) n) t | if1p res = do
    a <- nextArr t
    xR <- newITemp; i <- newITemp; nR <- newITemp; nO <- newITemp
    szR <- newITemp
    (lX, plX) <- aeval xs xR
    plN <- eval n nR
    ix <- newITemp
    let body=For i 0 ILt (Tmp nR) [CpyE (AElem t 1 (Tmp ix) (Just a) 8) (AElem xR 1 0 lX 8) (Tmp szR) 8, MT ix (Tmp ix+Tmp szR)]
    pure (Just a, plX ++ plN ++ MT szR (EAt (ADim xR 0 lX)):MT nO (Tmp szR*Tmp nR):Ma a t 1 (Tmp nO) 8:Wr (ADim t 0 (Just a)) (Tmp nO):MT ix 0:[body])
aeval (EApp _ (EApp _ (Builtin _ VMul) a) x) t | f1 (eAnn x) = do
    aL <- nextArr t
    xR <- newITemp; aR <- newITemp; i <- newITemp; j <- newITemp; m <- newITemp; n <- newITemp; z <- newFTemp
    (lA, plA) <- aeval a aR
    (lX, plX) <- aeval x xR
    let loop = For i 0 ILt (Tmp m) [MX z 0, For j 0 ILt (Tmp n) [MX z (FTmp z+FAt (AElem aR 2 (Tmp n*Tmp i+Tmp j) lA 8)*FAt (AElem xR 1 (Tmp j) lX 8))], WrF (AElem t 1 (Tmp i) (Just aL) 8) (FTmp z)]
    pure (Just aL, plA ++ plX ++ MT m (EAt (ADim aR 0 lA)):Ma aL t 1 (Tmp m) 8:Wr (ADim t 0 (Just aL)) (Tmp m):MT n (EAt (ADim xR 0 lX)):[loop])
aeval (EApp _ (EApp _ (Builtin _ Mul) a) b) t | Just (F, _) <- tRnk (eAnn a) = do
    aL <- nextArr t
    aR <- newITemp; bR <- newITemp; i <- newITemp; j <- newITemp; k <- newITemp; m <- newITemp; n <- newITemp; o <- newITemp; z <- newFTemp
    (lA, plA) <- aeval a aR
    (lB, plB) <- aeval b bR
    let loop=For i 0 ILt (Tmp m) [For j 0 ILt (Tmp o) [MX z 0, For k 0 ILt (Tmp n) [MX z (FTmp z+FAt (AElem aR 2 (Tmp n*Tmp i+Tmp k) lA 8)*FAt (AElem bR 2 (Tmp k*Tmp o+Tmp j) lB 8))], WrF (AElem t 2 (Tmp i*Tmp o+Tmp j) (Just aL) 8) (FTmp z)]]
    pure (Just aL, plA++plB++MT m (EAt (ADim aR 0 lA)):MT n (EAt (ADim bR 0 lB)):MT o (EAt (ADim bR 1 lB)):Ma aL t 2 (Tmp m*Tmp o) 8:Wr (ADim t 0 (Just aL)) (Tmp m):Wr (ADim t 1 (Just aL)) (Tmp o):[loop])
aeval (EApp _ (EApp _ (Builtin _ ConsE) x) xs) t | tX <- eAnn x, isIF tX = do
    a <- nextArr t
    xR <- rtemp tX; xsR <- newITemp
    plX <- eeval x xR
    (l, plXs) <- aeval xs xsR
    nR <- newITemp; nϵR <- newITemp
    pure (Just a, plXs++plX++MT nϵR (EAt (ADim xsR 0 l)):MT nR (Tmp nϵR+1):Ma a t 1 (Tmp nR) 8:Wr (ADim t 0 (Just a)) (Tmp nR):wt (AElem t 1 0 (Just a) 8) xR:[CpyE (AElem t 1 1 (Just a) 8) (AElem xsR 1 0 l 8) (Tmp nϵR) 8])
aeval (EApp _ (EApp _ (Builtin _ Snoc) x) xs) t | tX <- eAnn x, isIF tX = do
    a <- nextArr t
    xR <- rtemp tX; xsR <- newITemp
    plX <- eeval x xR
    (l, plXs) <- aeval xs xsR
    nR <- newITemp; nϵR <- newITemp
    pure (Just a, plXs++plX++MT nϵR (EAt (ADim xsR 0 l)):MT nR (Tmp nϵR+1):Ma a t 1 (Tmp nR) 8:Wr (ADim t 0 (Just a)) (Tmp nR):wt (AElem t 1 (Tmp nR-1) (Just a) 8) xR:[CpyE (AElem t 1 0 (Just a) 8) (AElem xsR 1 0 l 8) (Tmp nϵR) 8])
aeval (EApp _ (EApp _ (Builtin _ Re) n) x) t | tX <- eAnn x, isIF tX = do
    a <- nextArr t
    xR <- rtemp tX; nR <- newITemp
    i <- newITemp
    putN <- eval n nR; putX <- eeval x xR
    let loop=For i 0 ILt (Tmp nR) [wt (AElem t 1 (Tmp i) (Just a) 8) xR]
    pure (Just a, putN++Ma a t 1 (Tmp nR) 8:Wr (ADim t 0 (Just a)) (Tmp nR):putX++[loop])
aeval (EApp oTy (Builtin _ Init) x) t | if1p oTy = do
    a <- nextArr t
    xR <- newITemp; nR <- newITemp
    (lX, plX) <- aeval x xR
    pure (Just a, plX++MT nR (EAt (ADim xR 0 lX)-1):Ma a t 1 (Tmp nR) 8:Wr (ADim t 0 (Just a)) (Tmp nR):[CpyE (AElem t 1 0 (Just a) 8) (AElem xR 1 0 lX 8) (Tmp nR) 8])
aeval (EApp oTy (Builtin _ Tail) x) t | if1p oTy = do
    a <- nextArr t
    xR <- newITemp; nR <- newITemp
    (lX, plX) <- aeval x xR
    pure (Just a, plX++MT nR (EAt (ADim xR 0 lX)-1):Ma a t 1 (Tmp nR) 8:Wr (ADim t 0 (Just a)) (Tmp nR):[CpyE (AElem t 1 0 (Just a) 8) (AElem xR 1 1 lX 8) (Tmp nR) 8])
aeval (EApp _ (EApp _ (EApp _ (Builtin _ Zip) op) xs) ys) t | (Arrow tX (Arrow tY tC)) <- eAnn op, isIF tX && isIF tY && isIF tC = do
    a <- nextArr t
    aPX <- newITemp; aPY <- newITemp
    (lX, plEX) <- aeval xs aPX; (lY, plEY) <- aeval ys aPY
    x <- rtemp tX; y <- rtemp tY; z <- rtemp tC
    ss <- writeRF op [x,y] z
    nR <- newITemp; i <- newITemp
    let loopBody=mt (AElem aPX 1 (Tmp i) lX 8) x:mt (AElem aPY 1 (Tmp i) lY 8) y:ss++[wt (AElem t 1 (Tmp i) (Just a) 8) z]
        loop=For i 0 ILt (Tmp nR) loopBody
    pure (Just a, plEX++plEY++MT nR (EAt (ADim aPX 0 lX)):Ma a t 1 (Tmp nR) 8:Wr (ADim t 0 (Just a)) (Tmp nR):[loop])
aeval (EApp _ (EApp _ (EApp _ (Builtin _ ScanS) op) seed) e) t | (Arrow tX (Arrow tY _)) <- eAnn op, isIF tX && isIF tY = do
    a <- nextArr t
    aP <- newITemp
    acc <- rtemp tX; x <- rtemp tY
    plS <- eeval seed acc
    (l, plE) <- aeval e aP
    ss <- writeRF op [acc, x] acc
    i <- newITemp; n <- newITemp
    let loopBody=wt (AElem t 1 (Tmp i) (Just a) 8) acc:mt (AElem aP 1 (Tmp i) l 8) x:ss
        loop=For i 0 ILt (Tmp n) loopBody
    pure (Just a, plE++plS++MT n (EAt (ADim aP 0 l)+1):Ma a t 1 (Tmp n) 8:Wr (ADim t 0 (Just a)) (Tmp n):[loop])
aeval (EApp _ (EApp _ (Builtin _ Scan) op) xs) t | (Arrow tAcc (Arrow tX _)) <- eAnn op, isIF tAcc && isIF tX = do
    a <- nextArr t
    aP <- newITemp
    acc <- rtemp tAcc; x <- rtemp tX
    (l, plE) <- aeval xs aP
    ss <- writeRF op [acc, x] acc
    i <- newITemp; n <- newITemp
    let loopBody=wt (AElem t 1 (Tmp i-1) (Just a) 8) acc:mt (AElem aP 1 (Tmp i) l 8) x:ss
        loop=For i 1 ILeq (Tmp n) loopBody
    pure (Just a, plE++MT n (EAt (ADim aP 0 l)):Ma a t 1 (Tmp n) 8:Wr (ADim t 0 (Just a)) (Tmp n):mt (AElem aP 1 0 l 8) acc:[loop])
aeval (EApp oTy (EApp _ (Builtin _ (DI n)) op) xs) t | Just{} <- if1 (eAnn xs), Just ot <- if1 oTy = do
    a <- nextArr t
    aP <- newITemp
    slopP <- newITemp
    szR <- newITemp; sz'R <- newITemp; i <- newITemp
    fR <- rtemp ot
    (_, ss) <- writeF op [AA slopP Nothing] fR
    let szSlop=fromIntegral$16+8*n
    (lX, plX) <- aeval xs aP
    let sz'=Tmp szR-fromIntegral(n-1)
    let loopBody=CpyE (AElem slopP 1 0 Nothing 8) (AElem aP 1 (Tmp i) lX 8) (fromIntegral n) 8:ss++[wt (AElem t 1 (Tmp i) (Just a) 8) fR]
        loop=For i 0 ILt (Tmp sz'R) loopBody
    pure (Just a, plX++MT szR (EAt (ADim aP 0 lX)):MT sz'R sz':Ma a t 1 (Tmp sz'R) 8:Wr (ADim t 0 (Just a)) (Tmp sz'R):Sa slopP szSlop:Wr (ARnk slopP Nothing) 1:Wr (ADim slopP 0 Nothing) (fromIntegral n):loop:[Pop szSlop])
aeval (EApp _ (EApp _ (Builtin _ Rot) n) xs) t | if1p (eAnn xs) = do
    a <- nextArr t
    xsR <- newITemp; nR <- newITemp; c <- newITemp; szR <- newITemp
    plN <- eval n nR
    (lX, plX) <- aeval xs xsR
    pure (Just a, plX++plN++MT szR (EAt (ADim xsR 0 lX)):Ma a t 1 (Tmp szR) 8:Wr (ADim t 0 (Just a)) (Tmp szR):Ifn't (IRel IGeq (Tmp nR) 0) [MT nR (Tmp szR+ Tmp nR)]:MT c (Tmp szR-Tmp nR):[CpyE (AElem t 1 0 (Just a) 8) (AElem xsR 1 (Tmp nR) lX 8) (Tmp c) 8, CpyE (AElem t 1 (Tmp c) (Just a) 8) (AElem xsR 1 0 lX 8) (Tmp nR) 8])
aeval (Id _ (AShLit ns es)) t | Just ws <- mIFs es = do
    let rnk=fromIntegral$length ns
    n <- nextAA
    modify (addAA n (rnk:fmap fromIntegral ns++ws))
    pure (Nothing, [MT t (LA n)])
aeval (EApp _ (Builtin _ T) x) t | Just (ty, rnk) <- tRnk (eAnn x) = do
    a <- nextArr t
    let sze=bT ty; dO=ConstI$8+8*rnk
    xR <- newITemp; xd <- newITemp; td <- newITemp
    (l, plX) <- aeval x xR
    (dts, plDs) <- plDim rnk (xR, l)
    (sts, plSs) <- offByDim (reverse dts)
    (std, plSd) <- offByDim dts
    let n:sstrides = sts; (_:dstrides) = std
    is <- traverse (\_ -> newITemp) [1..rnk]
    let loop=thread (zipWith (\i tt -> (:[]) . For i 0 ILt (Tmp tt)) is dts) [CpyE (At td (Tmp<$>dstrides) (Tmp<$>reverse is) (Just a) sze) (At xd (Tmp<$>sstrides) (Tmp<$>is) l sze) 1 sze]
    pure (Just a, plX++plDs++plSs++Ma a t (ConstI rnk) (Tmp n) sze:zipWith (\tϵ o -> Wr (ADim t (ConstI o) (Just a)) (Tmp tϵ)) (reverse dts) [0..]++init plSd++MT xd (Tmp xR+dO):MT td (Tmp t+dO):loop)
aeval (EApp _ (EApp _ (EApp _ (Builtin _ Outer) op) xs) ys) t | (Arrow tX (Arrow tY tC)) <- eAnn op, isIF tX && isIF tY && isIF tC = do
    a <- nextArr t
    x <- rtemp tX; y <- rtemp tY; z <- rtemp tC
    xR <- newITemp; yR <- newITemp; szX <- newITemp; szY <- newITemp; i <- newITemp; j <- newITemp; k <- newITemp
    (lX, plX) <- aeval xs xR
    (lY, plY) <- aeval ys yR
    ss <- writeRF op [x,y] z
    let loop=For i 0 ILt (Tmp szX) [For j 0 ILt (Tmp szY) (mt (AElem xR 1 (Tmp i) lX 8) x:mt (AElem yR 1 (Tmp j) lY 8) y:ss++[wt (AElem t 2 (Tmp k) (Just a) 8) z, tick k])]
    pure (Just a, plX++plY++MT szX (EAt (ADim xR 0 lX)):MT szY (EAt (ADim yR 0 lY)):Ma a t 2 (Tmp szX*Tmp szY) 8:Wr (ADim t 0 (Just a)) (Tmp szX):Wr (ADim t 1 (Just a)) (Tmp szY):MT k 0:[loop])
aeval (EApp _ (EApp _ (Builtin _ Succ) op) xs) t | Arrow tX (Arrow _ tD) <- eAnn op, isIF tX && isIF tD= do
    a <- nextArr t
    xR <- newITemp
    szR <- newITemp; sz'R <- newITemp
    x <- rtemp tX; y <- rtemp tX; z <- rtemp tD
    (lX, plX) <- aeval xs xR
    i <- newITemp
    ss <- writeRF op [x,y] z
    let loopBody = mt (AElem xR 1 (Tmp i+1) lX 8) x:mt (AElem xR 1 (Tmp i) lX 8) y:ss++[wt (AElem t 1 (Tmp i) (Just a) 8) z]
        loop=For i 0 ILt (Tmp sz'R) loopBody
    pure (Just a, plX++MT szR (EAt (ADim xR 0 lX)):MT sz'R (Tmp szR-1):Ma a t 1 (Tmp sz'R) 8:Wr (ADim t 0 (Just a)) (Tmp sz'R):[loop])
aeval (EApp oTy (Builtin _ RevE) e) t | Just ty <- if1 oTy = do
    a <- nextArr t
    eR <- newITemp; n <- newITemp; i <- newITemp; o <- rtemp ty
    (lE, plE) <- aeval e eR
    let loop=For i 0 ILt (Tmp n) [mt (AElem eR 1 (Tmp n-Tmp i-1) lE 8) o, wt (AElem t 1 (Tmp i) (Just a) 8) o]
    pure (Just a, plE++MT n (EAt (ADim eR 0 lE)):Ma a t 1 (Tmp n) 8:Wr (ADim t 0 (Just a)) (Tmp n):[loop])
aeval (EApp oTy (EApp _ (EApp _ (Builtin _ Gen) seed) op) n) t | Just ty <- if1 oTy = do
    nR <- newITemp; plN <- eval n nR; i <- newITemp
    acc <- rtemp ty
    plS <- eeval seed acc
    a <- nextArr t
    ss <- writeRF op [acc] acc
    let loop=For i 0 ILt (Tmp nR) (wt (AElem t 1 (Tmp i) (Just a) 8) acc:ss)
    pure (Just a, plS++plN++Ma a t 1 (Tmp nR) 8:Wr (ADim t 0 (Just a)) (Tmp nR):[loop])
aeval (EApp _ (EApp _ (EApp _ (Builtin _ Gen) seed) op) n) t | isΠIF (eAnn seed) = do
    nR <- newITemp; plN <- eval n nR; i <- newITemp
    acc <- newITemp
    (szs,mP,_,plS) <- πe seed acc
    let πsz=last szs
    a <- nextArr t
    (_, ss) <- writeF op [IPA acc] (Right acc)
    let loop=For i 0 ILt (Tmp nR) (CpyE (AElem t 1 (Tmp i) (Just a) πsz) (Raw acc 0 Nothing undefined) 1 πsz:ss)
    pure (Just a, Sa acc (ConstI πsz):plS++plN++Ma a t 1 (Tmp nR) πsz:Wr (ADim t 0 (Just a)) (Tmp nR):loop:m'pop mP)
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
    ss <- writeRF op [Right acc, x] (Right acc)
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
eval (EApp _ (EApp _ (Builtin (Arrow I _) op) e0) e1) t | Just iop <- rel op = do
    e0R <- newITemp; e1R <- newITemp
    plE0 <- eval e0 e0R; plE1 <- eval e1 e1R
    pure $ plE0 ++ plE1 ++ [Cset (IRel iop (Tmp e0R) (Tmp e1R)) t]
eval (EApp _ (EApp _ (Builtin (Arrow F _) op) e0) e1) t | Just fop' <- frel op = do
    e0R <- newFTemp; e1R <- newFTemp
    plE0 <- feval e0 e0R; plE1 <- feval e1 e1R
    pure $ plE0 ++ plE1 ++ [Cset (FRel fop' (FTmp e0R) (FTmp e1R)) t]
eval (EApp _ (EApp _ (Builtin _ A1) e) i) t = do
    eR <- newITemp; iR <- newITemp
    (lE, plE) <- aeval e eR; plI <- eval i iR
    pure $ plE ++ plI ++ [MT t (EAt (AElem eR 1 (Tmp iR) lE 8))]
eval (EApp _ (Builtin _ Head) xs) t = do
    a <- newITemp
    (l, plX) <- aeval xs a
    pure $ plX ++ [MT t (EAt (AElem a 1 0 l 8))]
eval (EApp _ (Builtin _ Last) xs) t = do
    a <- newITemp
    (l, plX) <- aeval xs a
    pure $ plX ++ [MT t (EAt (AElem a 1 (EAt (ADim a 0 l)-1) l 8))]
eval (EApp _ (Builtin _ Size) xs) t | Just (_, 1) <- tRnk (eAnn xs) = do
    xsR <- newITemp
    (l, plE) <- aeval xs xsR
    pure $ plE ++ [MT t (EAt (ADim xsR 0 l))]
eval (EApp _ (Builtin _ Size) xs) t = do
    xsR <- newITemp
    (l, plE) <- aeval xs xsR
    rnkR <- newITemp
    pure $ plE ++ [MT rnkR (EAt (ARnk xsR l)), SZ t xsR (Tmp rnkR) l]
eval (EApp _ (Builtin _ Floor) x) t = do
    xR <- newFTemp
    plX <- feval x xR
    pure $ plX ++ [MT t (CFloor (FTmp xR))]
eval (EApp _ (Builtin _ (TAt i)) e) t = do
    k <- newITemp
    (offs, a, _, plT) <- πe e k
    pure $ plT ++ MT t (EAt (Raw k (ConstI$offs!!(i-1)) Nothing 1)):m'pop a
eval e _          = error (show e)

frel :: Builtin -> Maybe FRel
frel Gte=Just FGeq; frel Lte=Just FLeq; frel Eq=Just FEq; frel Neq=Just FNeq; frel Lt=Just FLt; frel Gt=Just FGt; frel _=Nothing

mFop :: Builtin -> Maybe FBin
mFop Plus=Just FPlus; mFop Times=Just FTimes; mFop Minus=Just FMinus; mFop Div=Just FDiv; mFop Exp=Just FExp; mFop Max=Just FMax; mFop Min=Just FMin; mFop _=Nothing

mOp :: Builtin -> Maybe IBin
mOp Plus=Just IPlus;mOp Times=Just ITimes;mOp Minus=Just IMinus; mOp Mod=Just IRem;mOp _=Nothing

mFun :: Builtin -> Maybe FUn
mFun Sqrt=Just FSqrt; mFun Log=Just FLog; mFun Sin=Just FSin; mFun Cos=Just FCos; mFun Abs=Just FAbs; mFun _=Nothing

cond :: E (T ()) -> E (T ()) -> E (T ()) -> Either FTemp Temp -> CM (Maybe AL, [CS])
cond (EApp _ (EApp _ (Builtin (Arrow F _) o) c0) c1) e0 e1 t | Just f <- frel o, isIF (eAnn e0) = do
    c0R <- newFTemp; c1R <- newFTemp
    plC0 <- feval c0 c0R; plC1 <- feval c1 c1R
    plE0 <- eeval e0 t; plE1 <- eeval e1 t
    pure (Nothing, plC0 ++ plC1 ++ [If (FRel f (FTmp c0R) (FTmp c1R)) plE0 plE1])
cond (EApp _ (EApp _ (Builtin (Arrow I _) o) c0) c1) e0 e1 t | Just cmp <- rel o, isIF (eAnn e0) = do
    c0R <- newITemp; c1R <- newITemp
    plC0 <- eval c0 c0R; plC1 <- eval c1 c1R
    plE0 <- eeval e0 t; plE1 <- eeval e1 t
    pure (Nothing, plC0 ++ plC1 ++ [If (IRel cmp (Tmp c0R) (Tmp c1R)) plE0 plE1])
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
feval (EApp _ (EApp _ (Builtin _ op) (FLit _ d)) e1) t | Just fb <- mFop op = do
    t1 <- newFTemp
    pl1 <- feval e1 t1
    pure $ pl1 ++ [MX t (FBin fb (ConstF d) (FTmp t1))]
feval (EApp _ (EApp _ (Builtin _ op) e0) (Var _ x)) t | Just fb <- mFop op = do
    st <- gets dvars
    t0 <- newFTemp
    pl0 <- feval e0 t0
    pure $ pl0 ++ [MX t (FBin fb (FTmp t0) (FTmp (getT st x)))]
feval (EApp _ (EApp _ (Builtin _ Plus) e0) (EApp _ (EApp _ (Builtin _ Times) e1) e2)) t = do
    t0 <- newFTemp; t1 <- newFTemp; t2 <- newFTemp
    pl0 <- feval e0 t0; pl1 <- feval e1 t1; pl2 <- feval e2 t2
    pure $ pl0 ++ pl1 ++ pl2 ++ [MX t (FTmp t0+FTmp t1*FTmp t2)]
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
feval (EApp _ (EApp _ (Builtin _ A1) e) i) t = do
    eR <- newITemp; iR <- newITemp
    (lE, plE) <- aeval e eR; plI <- eval i iR
    pure $ plE ++ plI ++ [MX t (FAt (AElem eR 1 (Tmp iR) lE 8))]
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
    ss <- writeRF op [Left acc, x] (Left acc)
    let step = mt (AElem  pR 1 (Tmp i) lP 8) x:ss
        loop = For i 1 ILt (Tmp szR) step
    sseed <- writeRF zop [x] (Left acc)
    pure $ plP++MT szR (EAt (ADim pR 0 lP)):mt (AElem pR 1 0 lP 8) x:sseed++[loop]
feval (Id _ (FoldOfZip zop op [EApp _ (EApp _ (EApp _ (Builtin _ IRange) start) _) incr, ys])) acc | Just tQ <- if1 (eAnn ys) = do
    x <- newITemp; yR <- newITemp; y <- rtemp tQ
    incrR <- newITemp; szR <- newITemp; i <- newITemp
    plX <- eval start x; plY <- eeval (EApp tQ (Builtin undefined Head) ys) y; (lY, plYs) <- aeval ys yR; plI <- eval incr incrR
    seed <- writeRF zop [Right x, y] (Left acc)
    ss <- writeRF op [Left acc, Right x, y] (Left acc)
    pure $ plX ++ plY ++ plYs ++ seed ++ plI ++ MT szR (EAt (ADim yR 0 lY)):[For i 1 ILt (Tmp szR) (mt (AElem yR 1 (Tmp i) lY 8) y:MT x (Tmp x+Tmp incrR):ss)]
feval (Id _ (FoldOfZip zop op [p, q])) acc | Just tP <- if1 (eAnn p), Just tQ <- if1 (eAnn q) = do
    x <- rtemp tP; y <- rtemp tQ
    pR <- newITemp; qR <- newITemp
    szR <- newITemp
    i <- newITemp
    (lP, plP) <- aeval p pR; (lQ, plQ) <- aeval q qR
    ss <- writeRF op [Left acc, x, y] (Left acc)
    let step = mt (AElem pR 1 (Tmp i) lP 8) x:mt (AElem qR 1 (Tmp i) lQ 8) y:ss
        loop = For i 1 ILt (Tmp szR) step
    seed <- writeRF zop [x,y] (Left acc)
    pure $ plP++plQ++MT szR (EAt (ADim pR 0 lP)):mt (AElem pR 1 0 lP 8) x:mt (AElem qR 1 0 lQ 8) y:seed++[loop]
feval (EApp _ (EApp _ (Builtin _ Fold) op) e) acc | (Arrow tX _) <- eAnn op, isF tX = do
    x <- newFTemp
    aP <- newITemp
    szR <- newITemp
    i <- newITemp
    (l, plE) <- aeval e aP
    ss <- writeRF op [Left acc, Left x] (Left acc)
    let loopBody=MX x (FAt (AElem aP 1 (Tmp i) l 8)):ss
        loop=For i 1 ILt (Tmp szR) loopBody
    pure $ plE++MT szR (EAt (ADim aP 0 l)):MX acc (FAt (AElem aP 1 0 l 8)):[loop]
feval (EApp _ (EApp _ (EApp _ (Builtin _ Foldl) op) seed) e) acc | (Arrow _ (Arrow tX _)) <- eAnn op, isIF tX = do
    x <- rtemp tX
    eR <- newITemp
    i <- newITemp
    (l, plE) <- aeval e eR
    plAcc <- feval seed acc
    ss <- writeRF op [x, Left acc] (Left acc)
    let loopBody=mt (AElem eR 1 (Tmp i) l 8) x:ss++[MT i (Tmp i-1)]
        loop=While i IGeq 0 loopBody
    pure $ plE++plAcc++MT i (EAt (ADim eR 0 l)-1):[loop]
feval (EApp _ (EApp _ (EApp _ (Builtin _ FoldS) op) seed) (EApp _ (EApp _ (EApp _ (Builtin _ IRange) start) end) incr)) acc = do
    i <- newITemp
    endR <- newITemp; incrR <- newITemp
    plStart <- eval start i; plAcc <- feval seed acc; plEnd <- eval end endR; plIncr <- eval incr incrR
    ss <- writeRF op [Left acc, Right i] (Left acc)
    pure $ plStart ++ plAcc ++ plEnd ++ plIncr ++ [While i ILeq (Tmp endR) (ss++[MT i (Tmp i+Tmp incrR)])]
feval (EApp _ (EApp _ (EApp _ (Builtin _ FoldS) op) seed) (EApp _ (EApp _ (EApp _ (Builtin _ FRange) start) end) nSteps)) acc = do
    i <- newITemp; startR <- newFTemp; incrR <- newFTemp; xR <- newFTemp; endI <- newITemp
    plStart <- feval start startR
    plAcc <- feval seed acc
    plEnd <- eval nSteps endI
    plIncr <- feval ((end `eMinus` start) `eDiv` (EApp F (Builtin (Arrow I F) ItoF) nSteps `eMinus` FLit F 1)) incrR
    ss <- writeRF op [Left acc, Left xR] (Left acc)
    pure $ plStart ++ MX xR (FTmp startR):plEnd++plIncr++plAcc++[For i 0 ILt (Tmp endI) (ss++[MX xR (FTmp xR+FTmp incrR)])]
feval (EApp _ (EApp _ (EApp _ (Builtin _ FoldS) op) seed) e) acc | (Arrow _ (Arrow tX _)) <- eAnn op, isIF tX = do
    x <- rtemp tX
    eR <- newITemp
    szR <- newITemp
    i <- newITemp
    (l, plE) <- aeval e eR
    plAcc <- feval seed acc
    ss <- writeRF op [Left acc, x] (Left acc)
    let loopBody=mt (AElem eR 1 (Tmp i) l 8) x:ss
        loop=For i 0 ILt (Tmp szR) loopBody
    pure $ plE++plAcc++MT szR (EAt (ADim eR 0 l)):[loop]
feval (EApp _ (Builtin _ (TAt i)) e) t = do
    k <- newITemp
    (offs, a, _, plT) <- πe e k
    pure $ plT ++ MX t (FAt (Raw k (ConstI$offs!!(i-1)) Nothing 1)):m'pop a
feval e _ = error (show e)

m'pop :: Maybe CE -> [CS]
m'pop = maybe [] ((:[]).Pop)

πe :: E (T ()) -> Temp -> CM ([Int64], Maybe CE, [AL], [CS])
πe (EApp (P tys) (Builtin _ Head) xs) t | offs <- szT tys, sz <- last offs, szE <- ConstI sz = do
    xR <- newITemp
    (lX, plX) <- aeval xs xR
    pure (offs, Just szE, [], plX++[Sa t szE, CpyE (Raw t 0 Nothing undefined) (AElem xR 1 0 lX sz) 1 sz])
πe (EApp (P tys) (Builtin _ Last) xs) t | offs <- szT tys, sz <- last offs, szE <- ConstI sz = do
    xR <- newITemp
    (lX, plX) <- aeval xs xR
    pure (offs, Just szE, [], plX++[Sa t szE, CpyE (Raw t 0 Nothing undefined) (AElem xR 1 (EAt (ADim xR 0 lX)-1) lX sz) 1 sz])
πe (Tup (P tys) es) t | offs <- szT tys, sz <- ConstI$last offs = do
    (ls, ss) <- unzip <$> zipWithM (\e off -> case eAnn e of {F -> do {f <- newFTemp; plX <- feval e f; pure (Nothing, plX++[WrF (Raw t (ConstI off) Nothing 1) (FTmp f)])}; Arr{} -> do {r <- newITemp ; (l,pl) <- aeval e r; pure (l, pl++[Wr (Raw t (ConstI off) Nothing 1) (Tmp r)])}}) es offs
    pure (offs, Just sz, catMaybes ls, concat ss)
πe (EApp (P tys) (EApp _ (Builtin _ A1) e) i) t | offs <- szT tys, sz <- last offs, szE <- ConstI sz = do
    xR <- newITemp; iR <- newITemp
    (lX, plX) <- aeval e xR; plI <- eval i iR
    pure (offs, Just szE, mempty, plX ++ plI ++ [Sa t szE, CpyE (Raw t 0 Nothing undefined) (AElem xR 1 (Tmp iR) lX sz) 1 sz])
πe (Var (P tys) x) t = do
    st <- gets vars
    pure (szT tys, Nothing, undefined, [MT t (Tmp$getT st x)])
πe (LLet _ (n,e') e) t | isArr (eAnn e') = do
    t' <- newITemp
    (l, ss) <- aeval e' t'
    modify (addAVar n (l, t'))
    fourth (ss++) <$> πe e t
πe (LLet _ (n,e') e) t | isF (eAnn e') = do
    eR <- newFTemp
    plE <- feval e' eR
    modify (addD n eR)
    fourth (plE++) <$> πe e t
πe e _ = error (show e)

fourth f ~(x,y,z,w) = (x,y,z,f w)

qmap f g h k ~(x,y,z,w) = (f x, g y, h z, k w)
