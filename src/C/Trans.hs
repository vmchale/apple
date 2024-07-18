{-# LANGUAGE TupleSections #-}

module C.Trans ( writeC ) where

import           A
import           Bits
import           C
import           CF.AL                      (AL (..))
import qualified CF.AL                      as AL
import           Control.Composition        (thread)
import           Control.Monad              (zipWithM)
import           Control.Monad.State.Strict (State, gets, modify, runState, state)
import           Data.Bifunctor             (first, second)
import           Data.Functor               (($>))
import           Data.Int                   (Int64)
import qualified Data.IntMap                as IM
import qualified Data.IntSet                as IS
import           Data.List                  (scanl')
import           Data.Maybe                 (catMaybes, isJust)
import           Data.Word                  (Word64)
import           GHC.Float                  (castDoubleToWord64)
import           Nm
import           Nm.IntMap
import           Op

data CSt = CSt { tempU       :: !Int
               , arrU        :: !AL
               , assemblerSt :: !Int
               , label       :: !Label
               , vars        :: IM.IntMap Temp -- track vars so that (Var x) can be replaced at the site
               , dvars       :: IM.IntMap FTemp
               , avars       :: IM.IntMap (Maybe AL, Temp)
               , fvars       :: IM.IntMap (Label, [Arg], Either FTemp Temp)
               , _aa         :: AsmData
               , mts         :: IM.IntMap Temp
               }

nextI :: CM Int
nextI = state (\(CSt tϵ ar as l v d a f aas ts) -> (tϵ, CSt (tϵ+1) ar as l v d a f aas ts))

nextArr :: Temp -> CM AL
nextArr r = state (\(CSt t a@(AL i) as l v d aϵ f aas ts) -> (a, CSt t (AL$i+1) as l v d aϵ f aas (AL.insert a r ts)))

nextAA :: CM Int
nextAA = state (\(CSt t ar as l v d a f aas ts) -> (as, CSt t ar (as+1) l v d a f aas ts))

neL :: CM Label
neL = state (\(CSt t ar as l v d a f aas ts) -> (l, CSt t ar as (l+1) v d a f aas ts))

newITemp :: CM Temp
newITemp = ITemp <$> nextI

newFTemp :: CM FTemp
newFTemp = FTemp <$> nextI

addAA :: Int -> [Word64] -> CSt -> CSt
addAA i aa (CSt t ar as l v d a f aas ts) = CSt t ar as l v d a f (IM.insert i aa aas) ts

addVar :: Nm a -> Temp -> CSt -> CSt
addVar n r (CSt t ar as l v d a f aas ts) = CSt t ar as l (insert n r v) d a f aas ts

addD :: Nm a -> FTemp -> CSt -> CSt
addD n r (CSt t ar as l v d a f aas ts) = CSt t ar as l v (insert n r d) a f aas ts

addAVar :: Nm a -> (Maybe AL, Temp) -> CSt -> CSt
addAVar n r (CSt t ar as l v d a f aas ts) = CSt t ar as l v d (insert n r a) f aas ts

addF :: Nm a -> (Label, [Arg], Either FTemp Temp) -> CSt -> CSt
addF n f (CSt t ar as l v d a fs aas ts) = CSt t ar as l v d a (insert n f fs) aas ts

getT :: IM.IntMap b -> Nm a -> b
getT st n = findWithDefault (error ("Internal error: variable " ++ show n ++ " not assigned to a temp.")) n st

type CM = State CSt

infix 9 +=
(+=) t i = t := (Tmp t+i)

fop op e0 = EApp F (EApp (F ~> F) (Builtin (F ~> F ~> F) op) e0)
eMinus = fop Minus
eDiv = fop Div

isF, isI, isB, isIF :: T a -> Bool
isF F = True; isF _ = False
isI I = True; isI _ = False
isB B = True; isB _ = False
isArr Arr{}=True; isArr _=False
isIF I=True; isIF F=True; isIF _=False
nind I=True; nind F=True; nind P{}=True; nind _=False
isΠIF (P ts)=all isIF ts; isΠIF _=False
isΠ P{}=True; isΠ _=False

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
bT (P ts)=sum (bT<$>ts); bT F=8; bT I=8; bT Arr{}=8

szT = scanl' (\off ty -> off+bT ty::Int64) 0

staRnk :: Integral b => Sh a -> Maybe b
staRnk Nil           = Just 0
staRnk (_ `Cons` sh) = (1+) <$> staRnk sh
staRnk _             = Nothing

tRnk :: T a -> Maybe (T a, Int64)
tRnk (Arr sh t) = (t,) <$> staRnk sh
tRnk _          = Nothing

staIx :: Sh a -> Maybe [Int64]
staIx Nil=Just[]; staIx (Ix _ i `Cons` s) = (fromIntegral i:)<$>staIx s; staIx _=Nothing

tIx :: T a -> Maybe (T a, [Int64])
tIx (Arr sh t) = (t,)<$>staIx sh; tIx _=Nothing

nz, ni1 :: I a -> Bool
nz (Ix _ i) | i > 0 = True
nz (StaPlus _ i0 i1) = nz i0 || nz i1 -- no negative dims
nz (StaMul _ i0 i1) = nz i0 && nz i1
nz _ = False

ni1 (Ix _ i) | i > 1 = True
ni1 (StaPlus _ i0 i1) = ni1 i0 || ni1 i1
ni1 (StaMul _ i0 i1) = (nz i0&&ni1 i1) || (nz i1&&ni1 i0)
ni1 _ = False

ne, n1 :: T a -> Bool
ne (Arr (i `Cons` _) _) = nz i; ne _=False
n1 (Arr (i `Cons` _) _) = ni1 i; n1 _=False

for t = if ne t then For1 else For; for1 t = if n1 t then For1 else For

staR :: Sh a -> [Int64]
staR Nil = []; staR (Ix _ i `Cons` s) = fromIntegral i:staR s

tRnd :: T a -> (T a, [Int64])
tRnd (Arr sh t) = (t, staR sh)

mIFs :: [E a] -> Maybe [Word64]
mIFs = fmap concat.traverse mIFϵ where mIFϵ (FLit _ d)=Just [castDoubleToWord64 d]; mIFϵ (ILit _ n)=Just [fromIntegral n]; mIFϵ (Tup _ xs)=mIFs xs; mIFϵ _=Nothing

writeC :: E (T ()) -> ([CS], LSt, AsmData, IM.IntMap Temp)
writeC = π.flip runState (CSt 0 (AL 0) 0 0 IM.empty IM.empty IM.empty IM.empty IM.empty IM.empty) . writeCM . fmap rLi where π (s, CSt t _ _ l _ _ _ _ aa a) = (s, LSt l t, aa, a)

writeCM :: E (T ()) -> CM [CS]
writeCM eϵ = do
    cs <- traverse (\_ -> newITemp) [(0::Int)..5]; fs <- traverse (\_ -> newFTemp) [(0::Int)..5]
    (zipWith (\xr xr' -> MX xr' (FTmp xr)) [F0,F1,F2,F3,F4,F5] fs ++) . (zipWith (\r r' -> r' := Tmp r) [C0,C1,C2,C3,C4,C5] cs ++) <$> go eϵ fs cs where
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
             | isI (eAnn e) = do {t <- newITemp; (++[CRet := Tmp t]) <$> eval e t} -- avoid clash when calling functions
             | isB (eAnn e) = do {t <- newITemp; (++[CRet := Tmp t]) <$> eval e t}
             | isArr (eAnn e) = do {i <- newITemp; (l,r) <- aeval e i; pure$r++[CRet := Tmp i]++case l of {Just m -> [RA m]; Nothing -> []}}
             | P [F,F] <- eAnn e = do {t <- newITemp; (_,_,_,p) <- πe e t; pure$Sa t 16:p++[MX FRet0 (FAt (Raw t 0 Nothing 8)), MX FRet1 (FAt (Raw t 1 Nothing 8)), Pop 16]}
             | ty@P{} <- eAnn e, b64 <- bT ty, (n,0) <- b64 `quotRem` 8 = let b=ConstI b64 in do {t <- newITemp; a <- nextArr CRet; (_,_,ls,pl) <- πe e t; pure (Sa t b:pl++MaΠ a CRet b:CpyE (TupM CRet (Just a)) (TupM t Nothing) (ConstI n) 8:Pop b:RA a:(RA<$>ls))}

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
writeF e [] (Right r) | ty <- eAnn e, isI ty || isB ty = (Nothing,)<$>eval e r
writeF e [] (Right r) | isΠIF (eAnn e) = (\ ~(_,_,_,ss) -> (Nothing, ss))<$>πe e r
writeF e [] (Left r) = (Nothing,)<$>feval e r

m'p :: Maybe (CS, CS) -> [CS] -> [CS]
m'p Nothing        = id
m'p (Just (a,pop)) = (++[pop]).(a:)

sas :: [Maybe (CS, CS)] -> [CS] -> [CS]
sas = thread.fmap m'p

aS :: E (T ()) -> [(T (), Int64 -> ArrAcc)] -> T () -> (Int64 -> ArrAcc) -> CM ([CS], [Maybe (CS, CS)])
aS f as rT rAt = do
    (args, rArgs, pinchArgs) <- unzip3 <$> traverse (uncurry arg) (fmap (\(t,r) -> (t,r$bT t)) as)
    (r, wR, pinch) <- rW rT (rAt$bT rT)
    ss <- writeRF f args r
    pure (rArgs++ss++[wR], pinch:pinchArgs)

arg :: T () -> ArrAcc -> CM (Either FTemp Temp, CS, Maybe (CS, CS))
arg ty at | isIF ty = do
    t <- rtemp ty
    pure (t, mt at t, Nothing)
arg ty at | isΠ ty = do
    slop <- newITemp
    let sz=bT ty; slopE=ConstI sz
    pure (Right slop, CpyE (TupM slop Nothing) at 1 sz, Just (Sa slop slopE, Pop slopE))

rW :: T () -> ArrAcc -> CM (Either FTemp Temp, CS, Maybe (CS, CS))
rW ty at | isIF ty = do
    t <- rtemp ty
    pure (t, wt at t, Nothing)
rW ty at | isΠ ty = do
    slopO <- newITemp
    let sz=bT ty; slopE=ConstI sz
    pure (Right slopO, CpyE at (TupM slopO Nothing) 1 sz, Just (Sa slopO slopE, Pop slopE))

writeRF :: E (T ()) -> [Either FTemp Temp] -> Either FTemp Temp -> CM [CS]
writeRF e args = fmap snd.writeF e (ra<$>args)

data Arg = IPA !Temp | FA !FTemp | AA !Temp (Maybe AL)

mt :: ArrAcc -> Either FTemp Temp -> CS
mt p = either (`MX` FAt p) (:= EAt p)

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

diml :: (Temp, Maybe AL) -> [CE] -> [CS]
diml (t,l) ds = zipWith (\d i -> Wr (ADim t (ConstI i) l) d) ds [0..]

vSz :: Temp -> CE -> Int64 -> CM (AL, [CS])
vSz t n sz = do {a <- nextArr t; pure (a, [Ma a t 1 n sz, Wr (ADim t 0 (Just a)) n])}

v8 :: Temp -> CE -> CM (AL, [CS])
v8 t n = vSz t n 8

plDim :: Int64 -> (Temp, Maybe AL) -> CM ([Temp], [CS])
plDim rnk (a,l) =
    unzip <$> traverse (\at -> do {dt <- newITemp; pure (dt, dt := EAt at)}) [ ADim a (ConstI i) l | i <- [0..rnk-1] ]

offByDim :: [Temp] -> CM ([Temp], [CS])
offByDim dims = do
    sts <- traverse (\_ -> newITemp) (undefined:dims)
    let ss=zipWith3 (\s1 s0 d -> s1 := (Tmp s0*Tmp d)) (tail sts) sts dims
    pure (reverse sts, head sts := 1:ss)
    -- drop 1 for strides

data Cell a b = Fixed -- set by the larger procedure
              | Bound b -- to be iterated over

forAll is bs = thread (zipWith (\t b -> (:[]) . For t 0 ILt b) is bs)

-- the resulting expressions/statement contain free variables that will be iterated over in the main rank-ification loop, these free variables are returned alongside
extrCell :: [Cell () Temp] -> [Temp] -> (Temp, Maybe AL) -> Temp -> CM ([Temp], [CS])
extrCell fixBounds sstrides (srcP, srcL) dest = do
    (dims, ts, arrIxes, complts) <- switch fixBounds
    t <- newITemp; i <- newITemp
    pure (complts, (i := 0:) $ forAll ts (Tmp<$>dims)
        [t := EAt (At srcP (Tmp<$>sstrides) (Tmp<$>arrIxes) srcL 8), Wr (Raw dest (Tmp i) Nothing 8) (Tmp t), i+=1])
    where switch (Bound d:ds) = do {t <- newITemp; qmap (d:) (t:) (t:) id <$> switch ds}
          switch (Fixed:ds)   = do {f <- newITemp; qmap id id (f:) (f:) <$> switch ds}
          switch []           = pure ([], [], [], [])

llet :: (Nm (T ()), E (T ())) -> CM [CS]
llet (n,e') | isArr (eAnn e') = do
    eR <- newITemp
    (l, ss) <- aeval e' eR
    modify (addAVar n (l,eR)) $> ss
llet (n,e') | isI (eAnn e') = do
    eR <- newITemp
    ss <- eval e' eR
    modify (addVar n eR) $> ss
llet (n,e') | isF (eAnn e') = do
    eR <- newFTemp
    ss <- feval e' eR
    modify (addD n eR) $> ss
llet (n,e') | Arrow F F <- eAnn e' = do
    l <- neL
    x <- newFTemp; y <- newFTemp
    (_, ss) <- writeF e' [FA x] (Left y)
    modify (addF n (l, [FA x], (Left y)))
    pure [C.Def l ss]

aeval :: E (T ()) -> Temp -> CM (Maybe AL, [CS])
aeval (LLet _ b e) t = do
    ss <- llet b
    second (ss ++) <$> aeval e t
aeval (Var _ x) t = do
    st <- gets avars
    let (i, r) = {-# SCC "getA" #-} getT st x
    pure (i, [t := Tmp r])
aeval (EApp ty (EApp _ (Builtin _ A.R) e0) e1) t | (F, ixs) <- tRnd ty = do
    a <- nextArr t
    e0R <- newFTemp; e1R <- newFTemp; iR <- newITemp; xR <- newFTemp; k <- newITemp
    plE0 <- feval e0 e0R; plE1 <- feval e1 e1R
    let rnk=fromIntegral(length ixs); n=product ixs
        plRnd = [Rnd iR, MX xR (IE (Tmp iR)), MX xR ((FTmp e1R - FTmp e0R) * (FTmp xR / (2*9223372036854775807) + 0.5) + FTmp e0R), WrF (AElem t rnk (Tmp k) (Just a) 8) (FTmp xR)]
        loop=For k 0 ILt (ConstI n) plRnd
    pure (Just a, plE0++plE1++Ma a t rnk (ConstI n) 8:diml (t, Just a) (ConstI<$>ixs)++[loop])
aeval (EApp ty (EApp _ (Builtin _ A.R) e0) e1) t | (I, ixs) <- tRnd ty = do
    a <- nextArr t
    e0R <- newITemp; e1R <- newITemp; iR <- newITemp; k <- newITemp
    plE0 <- eval e0 e0R; plE1 <- eval e1 e1R
    let rnk=fromIntegral$length ixs; n=product ixs
        plRnd = [Rnd iR, iR := (Bin IRem (Tmp iR) (Tmp e1R - Tmp e0R + 1) + Tmp e0R), Wr (AElem t rnk (Tmp k) (Just a) 8) (Tmp iR)]
        loop=For k 0 ILt (ConstI n) plRnd
    pure (Just a, plE0++plE1++Ma a t rnk (ConstI n) 8:diml (t, Just a) (ConstI<$>ixs)++[loop])
aeval (EApp _ (Builtin _ AddDim) x) t | F <- eAnn x = do
    xR <- newFTemp
    plX <- feval x xR
    (a,aV) <- v8 t 1
    pure (Just a, plX++aV++[WrF (AElem t 1 0 (Just a) 8) (FTmp xR)])
aeval (EApp _ (Builtin _ AddDim) x) t | I <- eAnn x = do
    xR <- newITemp
    plX <- eval x xR
    (a,aV) <- v8 t 1
    pure (Just a, plX++aV++[Wr (AElem t 1 0 (Just a) 8) (Tmp xR)])
aeval (EApp _ (Builtin _ AddDim) x) t | P{} <- eAnn x = do
    xR <- newITemp
    (szs, mS, _, plX) <- πe x xR
    let sz=last szs
    (a,aV) <- vSz t 1 sz
    pure (Just a, m'sa xR mS++plX++aV++[CpyE (AElem t 1 0 (Just a) sz) (TupM xR Nothing) 1 sz]++m'pop mS)
aeval (EApp _ (Builtin _ AddDim) xs) t | (Arr _ ty) <- eAnn xs, nind ty = do
    xR <- newITemp
    (lX, plX) <- aeval xs xR
    let sz=bT ty
    xRnk <- newITemp; szR <- newITemp; rnk <- newITemp
    a <- nextArr t
    pure (Just a,
            plX++xRnk:=EAt (ARnk xR lX):SZ szR xR (Tmp xRnk) lX:rnk := (Tmp xRnk+1):Ma a t (Tmp rnk) (Tmp szR) sz:
           [Wr (ADim t 0 (Just a)) 1, CpyD (ADim t 1 (Just a)) (ADim xR 0 lX) (Tmp xRnk), CpyE (AElem t (Tmp rnk) 0 (Just a) sz) (AElem xR (Tmp xRnk) 0 lX sz) (Tmp szR) sz])
aeval (EApp _ (Builtin _ Flat) xs) t | (Arr _ ty) <- eAnn xs, nind ty = do
    xR <- newITemp
    (lX, plX) <- aeval xs xR
    let sz=bT ty
    xRnk <- newITemp; szR <- newITemp
    (a,aV) <- vSz t (Tmp szR) sz
    pure (Just a, plX++xRnk:=EAt (ARnk xR lX):SZ szR xR (Tmp xRnk) lX:aV++[CpyE (AElem t 1 0 (Just a) sz) (AElem xR (Tmp xRnk) 0 lX sz) (Tmp szR) sz])
aeval (EApp _ (EApp _ (Builtin _ Map) op) e) t | (Arrow tD tC) <- eAnn op, nind tD && nind tC = do
    xR <- newITemp
    (l, plE) <- aeval e xR
    iR <- newITemp; szR <- newITemp
    let sz=bT tC
    (a,aV) <- vSz t (Tmp szR) sz
    (step, pinches) <- aS op [(tD, AElem xR 1 (Tmp iR) l)] tC (AElem t 1 (Tmp iR) (Just a))
    let loop=for (eAnn e) iR 0 ILt (Tmp szR) step
    pure (Just a,
        plE
        ++szR:=EAt (ADim xR 0 l):aV
        ++sas pinches [loop])
aeval (EApp _ (EApp _ (Builtin _ Map) f) xs) t | (Arrow tD tC) <- eAnn f, Just (_, xRnk) <- tRnk (eAnn xs), Just (ta, rnk) <- tRnk tD, nind tC && isIF ta = do
    a <- nextArr t
    xR <- newITemp; slopP <- newITemp; szR <- newITemp; slopSz <- newITemp
    xd <- newITemp; i <- newITemp; k <- newITemp
    (lX, plX) <- aeval xs xR
    let sz=bT tC
    (y, wRet, pinch) <- rW tC (AElem t 1 (Tmp k) (Just a) sz)
    (_, ss) <- writeF f [AA slopP Nothing] y
    let slopDims=[EAt (ADim xR (ConstI l) lX) | l <- [rnk..(xRnk-1)]]
        xDims=[EAt (ADim xR (ConstI l) lX) | l <- [0..(rnk-1)]]
        slopE=Bin IAsl (Tmp slopSz) 3+fromIntegral (8+8*rnk)
        dimsFromIn=ConstI$xRnk-rnk
        oRnk=xRnk-rnk
        step=CpyE (AElem slopP (ConstI rnk) 0 Nothing 8) (Raw xd (Tmp i) lX 8) (Tmp slopSz) 8:ss++[wRet, i+=Tmp slopSz]
    pure (Just a,
        plX
        ++PlProd slopSz slopDims:Sa slopP slopE:diml (slopP, Nothing) slopDims
        ++PlProd szR xDims
        :Ma a t (ConstI oRnk) (Tmp szR) 8
            :CpyD (ADim t 0 (Just a)) (ADim xR 0 lX) dimsFromIn
        :xd:=DP xR (ConstI xRnk):i:=0
        :m'p pinch
            (For k 0 ILt (Tmp szR) step:[Pop slopE]))
aeval (EApp _ (EApp _ (Builtin _ Map) f) xs) t | (Arrow tD tC) <- eAnn f, Just (_, xRnk) <- tRnk (eAnn xs), Just (ta, rnk) <- tRnk tC, isIF tD && isIF ta = do
    a <- nextArr t
    x <- rtemp tD; y <- newITemp; szX <- newITemp; szY <- newITemp
    xR <- newITemp; j <- newITemp; k <- newITemp; td <- newITemp; yd <- newITemp
    (lX, plX) <- aeval xs xR
    (lY, ss) <- writeF f [ra x] (Right y)
    let xDims=[EAt (ADim xR (ConstI l) lX) | l <- [0..(xRnk-1)]]
        yDims=[EAt (ADim y (ConstI l) lY) | l <- [0..(rnk-1)]]
        oRnk=xRnk+rnk
        step=mt (AElem xR (ConstI xRnk) (Tmp k) (Just a) 8) x:ss++[yd:=DP y (ConstI rnk), CpyE (Raw td (Tmp j) (Just a) 8) (Raw yd 0 lY undefined) (Tmp szY) 8, j+=Tmp szY]
    pure (Just a,
        plX
        ++mt (AElem xR (ConstI xRnk) 0 (Just a) 8) x
        :ss
        ++PlProd szY yDims
        :PlProd szX xDims
        :Ma a t (ConstI oRnk) (Tmp szX*Tmp szY) 8
            :CpyD (ADim t 0 (Just a)) (ADim xR 0 lX) (ConstI xRnk)
            :CpyD (ADim t (ConstI xRnk) (Just a)) (ADim y 0 lY) (ConstI rnk)
        :td:=DP t (ConstI$xRnk+rnk)
        :j:=0
          :[For k 0 ILt (Tmp szX) step])
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
        step=CpyE (AElem slopP (ConstI rnk0) 0 Nothing 8) (Raw xd (Tmp i) lX 8) (Tmp slopSz) 8:ss++[CpyE (Raw td (Tmp j) (Just a) 8) (AElem y (ConstI rnk1) 0 lY 8) (Tmp szY) 8, i+=Tmp slopSz, j+=Tmp szY]
    pure (Just a,
        plX
        ++PlProd slopSz slopDims:Sa slopP slopE:diml (slopP, Nothing) slopDims
        ++xd:=DP xR (ConstI xRnk)
        :CpyE (AElem slopP (ConstI rnk0) 0 Nothing 8) (Raw xd 0 lX 8) (Tmp slopSz) 8
        :ss
        ++PlProd szR (xDims++yDims)
        :Ma a t (ConstI oRnk) (Tmp szR) 8
            :CpyD (ADim t 0 (Just a)) (ADim xR 0 lX) dimsFromIn
            :CpyD (ADim t dimsFromIn (Just a)) (ADim y 0 lY) (ConstI rnk1)
        :td:=DP t (ConstI oRnk)
        :PlProd szY yDims
        :PlProd kL xDims:i := 0:j := 0
            :For k 0 ILt (Tmp kL) step
        :[Pop slopE])
aeval (EApp _ (EApp _ (Builtin _ (Rank [(0, _)])) f) xs) t | (Arrow tX tY) <- eAnn f, nind tX && nind tY = do
    a <- nextArr t
    xR <- newITemp; rnkR <- newITemp; szR <- newITemp
    i <- newITemp; xRd <- newITemp; tD <- newITemp
    let szY=bT tY
    (lX, plX) <- aeval xs xR
    (step, pinches) <- aS f [(tX, Raw xRd (Tmp i) lX)] tY (Raw tD (Tmp i) (Just a))
    let loop=For i 0 ILt (Tmp szR) step
    pure (Just a, plX++rnkR := EAt (ARnk xR lX):SZ szR xR (Tmp rnkR) lX:Ma a t (Tmp rnkR) (Tmp szR) szY:CpyD (ADim t 0 (Just a)) (ADim xR 0 lX) (Tmp rnkR):xRd := DP xR (Tmp rnkR):tD := DP t (Tmp rnkR):sas pinches [loop])
aeval (EApp _ (EApp _ (EApp _ (Builtin _ (Rank [(0, _), (0, _)])) op) xs) ys) t | Arrow tX (Arrow tY tC) <- eAnn op, nind tX && nind tY && nind tC = do
    a <- nextArr t
    xR <- newITemp; yR <- newITemp; rnkR <- newITemp; szR <- newITemp
    xRd <- newITemp; yRd <- newITemp; tD <- newITemp
    (lX, plX) <- aeval xs xR
    (lY, plY) <- aeval ys yR
    let szC=bT tC
    i <- newITemp
    (step, pinches) <- aS op [(tX, Raw xRd (Tmp i) lX), (tY, Raw yRd (Tmp i) lY)] tC (Raw tD (Tmp i) (Just a))
    let loop=For i 0 ILt (Tmp szR) step
    pure (Just a, plX ++ plY ++ rnkR := EAt (ARnk xR lX):SZ szR xR (Tmp rnkR) lX:Ma a t (Tmp rnkR) (Tmp szR) szC:CpyD (ADim t 0 (Just a)) (ADim xR 0 lX) (Tmp rnkR):xRd := DP xR (Tmp rnkR):yRd := DP yR (Tmp rnkR):tD := DP t (Tmp rnkR):sas pinches [loop])
aeval (EApp _ (EApp _ (EApp _ (Builtin _ (Rank [(0, _), (cr, Just ixs)])) op) xs) ys) t | Just (_, yRnk) <- tRnk (eAnn ys)
                                                                                        , Just (_, xRnk) <- tRnk (eAnn xs)
                                                                                        , (Arrow tX (Arrow _ tCod)) <- eAnn op
                                                                                        , Just (tC, opRnk) <- tRnk tCod
                                                                                        , nind tX && isIF tC = do
    a <- nextArr t
    xR <- newITemp; yR <- newITemp; zR <- newITemp
    (lX, plX) <- aeval xs xR; (lY, plY) <- aeval ys yR
    slopP <- newITemp
    let ixsIs = IS.fromList ixs; allIx = [ if ix `IS.member` ixsIs then Index() else Cell() | ix <- [1..fromIntegral yRnk] ]
    oSz <- newITemp; slopSz <- newITemp; zSz <- newITemp
    ix <- newITemp; it <- newITemp
    slopE <- newITemp
    (dts, dss) <- plDim yRnk (yR, lY)
    (sts, sssϵ) <- offByDim (reverse dts)
    let _:sstrides = sts; sss=init sssϵ
        allDims = zipWith (\ixϵ dt -> case ixϵ of {Cell{} -> Cell dt; Index{} -> Index dt}) allIx dts
        ~(oDims, complDims) = part allDims
        slopRnk=fromIntegral cr::Int64; oRnk=yRnk+opRnk-slopRnk
        xSz=bT tX
    (x, pAX, pinch) <- arg tX (AElem xR (ConstI xRnk) (Tmp ix) lX xSz)
    (lZ, ss) <- writeF op [ra x, AA slopP Nothing] (Right zR)
    let ecArg = zipWith (\d tt -> case (d,tt) of (dϵ,Index{}) -> Bound dϵ; (_,Cell{}) -> Fixed) dts allIx
    yRd <- newITemp; slopPd <- newITemp
    (complts, place) <- extrCell ecArg sstrides (yRd, lY) slopPd
    let loop=forAll complts (Tmp<$>oDims) $ pAX:place ++ ss ++ [CpyE (AElem t (ConstI oRnk) (Tmp it) (Just a) 8) (AElem zR (ConstI opRnk) 0 lZ undefined) (Tmp zSz) 8, ix+=1, it+=Tmp zSz]
    (dots, doss) <- plDim opRnk (zR, lZ)
    pure (Just a,
        plX
        ++plY
        ++dss
        ++PlProd slopSz (Tmp<$>complDims)
            :slopE := Bin IAsl (Tmp slopSz+ConstI (slopRnk+1)) 3
            :Sa slopP (Tmp slopE):Wr (ARnk slopP Nothing) (ConstI slopRnk)
            :diml (slopP, Nothing) (Tmp<$>complDims)
        ++[tϵ:=0 | tϵ <- complts]
        ++mt (AElem xR (ConstI xRnk) 0 lX undefined) x
        :sss
        ++yRd := DP yR (ConstI yRnk):slopPd := DP slopP (ConstI slopRnk)
        :place
        ++ss
        ++doss
        ++PlProd zSz (Tmp<$>dots)
        :PlProd oSz (Tmp<$>(zSz:oDims))
            :Ma a t (ConstI oRnk) (Tmp oSz) 8
            :diml (t, Just a) (Tmp<$>(oDims++dots))
        ++ix:=0:it:=0:m'p pinch loop
        ++[Pop (Tmp slopE)])
aeval (EApp _ (EApp _ (Builtin _ (Rank [(cr, Just ixs)])) f) xs) t | Just (tA, rnk) <- tRnk (eAnn xs)
                                                                    , (Arrow _ tC) <- eAnn f
                                                                    , nind tC && isIF tA = do
    a <- nextArr t
    xR <- newITemp
    (lX, plX) <- aeval xs xR
    slopP <- newITemp
    let ixsIs = IS.fromList ixs; allIx = [ if ix `IS.member` ixsIs then Index() else Cell() | ix <- [1..fromIntegral rnk] ]
    oSz <- newITemp; slopSz <- newITemp; slopE <- newITemp
    di <- newITemp
    (dts, dss) <- plDim rnk (xR, lX)
    (sts, sssϵ) <- offByDim (reverse dts)
    let _:sstrides = sts; sss=init sssϵ
        allDims = zipWith (\ix dt -> case ix of {Cell{} -> Cell dt; Index{} -> Index dt}) allIx dts
        ~(oDims, complDims) = part allDims
        oRnk=rnk-fromIntegral cr; slopRnk=fromIntegral cr::Int64
        ySz=bT tC
    (y, wY, pinch) <- rW tC (AElem t (ConstI oRnk) (Tmp di) Nothing ySz)
    (_, ss) <- writeF f [AA slopP Nothing] y
    let ecArg = zipWith (\d tt -> case (d,tt) of (dϵ,Index{}) -> Bound dϵ; (_,Cell{}) -> Fixed) dts allIx
    xRd <- newITemp; slopPd <- newITemp
    (complts, place) <- extrCell ecArg sstrides (xRd, lX) slopPd
    let loop=forAll complts (Tmp<$>oDims) $ place ++ ss ++ [wY, di+=1]
    pure (Just a,
        plX++dss
        ++PlProd slopSz (Tmp<$>complDims)
            :slopE := Bin IAsl (Tmp slopSz+ConstI (slopRnk+1)) 3
            :Sa slopP (Tmp slopE):Wr (ARnk slopP Nothing) (ConstI slopRnk)
            :diml (slopP, Nothing) (Tmp<$>complDims)
        ++PlProd oSz (Tmp<$>oDims)
            :Ma a t (ConstI oRnk) (Tmp oSz) ySz
            :diml (t, Just a) (Tmp<$>oDims)
        ++sss
        ++xRd := DP xR (ConstI rnk):slopPd := DP slopP (ConstI slopRnk):di := 0:m'p pinch loop
        ++[Pop (Tmp slopE)])
aeval (EApp tO (EApp _ (Builtin _ (Rank [(cr, Just ixs)])) f) xs) t | Just (tA, xRnk) <- tRnk (eAnn xs)
                                                                    , Just {} <- mIF tO
                                                                    , (Arrow _ tCod) <- eAnn f
                                                                    , Just (_, opRnk) <- tRnk tCod
                                                                    , isIF tA = do
    a <- nextArr t
    xR <- newITemp
    (lX, plX) <- aeval xs xR
    slopP <- newITemp
    let ixIs = IS.fromList ixs; allIx = [ if ix `IS.member` ixIs then Index() else Cell() | ix <- [1..fromIntegral xRnk] ]
    yR <- newITemp; ySz <- newITemp
    (dts,dss) <- plDim xRnk (xR,lX)
    (sts, sssϵ) <- offByDim (reverse dts)
    let _:sstrides = sts; sss=init sssϵ
        allDims = zipWith (\ix dt -> case ix of {Cell{} -> Cell dt; Index{} -> Index dt}) allIx dts
        ~(oDims, complDims) = part allDims
        slopRnk=fromIntegral cr::Int64; oRnk=xRnk+opRnk-slopRnk
    (lY, ss) <- writeF f [AA slopP Nothing] (Right yR)
    let ecArg = zipWith (\d tt -> case (d,tt) of (dϵ,Index{}) -> Bound dϵ; (_,Cell{}) -> Fixed) dts allIx
    xRd <- newITemp; slopPd <- newITemp; slopSz <- newITemp
    slopE <- newITemp; oSz <- newITemp
    (complts, place) <- extrCell ecArg sstrides (xRd, lX) slopPd
    it <- newITemp
    let loop=forAll complts (Tmp<$>oDims)
                $ place ++ ss ++ [CpyE (AElem t (ConstI oRnk) (Tmp it) (Just a) 8) (AElem yR (ConstI opRnk) 0 lY undefined) (Tmp ySz) 8, it+=Tmp ySz]
    (dots, doss) <- plDim opRnk (yR, lY)
    pure (Just a,
        plX
        ++dss
        ++PlProd slopSz (Tmp<$>complDims)
            :slopE := Bin IAsl (Tmp slopSz+ConstI (slopRnk+1)) 3
            :Sa slopP (Tmp slopE):Wr (ARnk slopP Nothing) (ConstI slopRnk)
            :diml (slopP, Nothing) (Tmp<$>complDims)
        ++[tϵ:=0 | tϵ <- complts]
        ++sss
        ++xRd:=DP xR (ConstI xRnk):slopPd:=DP slopP (ConstI slopRnk)
        :place
        ++ss
        ++doss
        ++PlProd ySz (Tmp<$>dots)
        :PlProd oSz (Tmp<$>(ySz:oDims))
            :Ma a t (ConstI oRnk) (Tmp oSz) 8
            :diml (t, Just a) (Tmp<$>(oDims++dots))
        ++it:=0:loop
        ++[Pop (Tmp slopE)]
        )
aeval (EApp _ (EApp _ (Builtin _ CatE) x) y) t | Just (ty, 1) <- tRnk (eAnn x) = do
    xR <- newITemp; yR <- newITemp
    xnR <- newITemp; ynR <- newITemp; tn <- newITemp
    (a,aV) <- v8 t (Tmp tn)
    let tyN=bT ty
    (lX, plX) <- aeval x xR; (lY, plY) <- aeval y yR
    pure (Just a, plX ++ plY ++ xnR := EAt (ADim xR 0 lX):ynR := EAt (ADim yR 0 lY):tn := (Tmp xnR+Tmp ynR):aV++CpyE (AElem t 1 0 (Just a) tyN) (AElem xR 1 0 lX tyN) (Tmp xnR) tyN:[CpyE (AElem t 1 (Tmp xnR) (Just a) tyN) (AElem yR 1 0 lY tyN) (Tmp ynR) tyN])
aeval (EApp _ (EApp _ (EApp _ (Builtin _ IRange) start) end) (ILit _ 1)) t = do
    n <- newITemp; startR <- newITemp; endR <- newITemp
    (a,aV) <- v8 t (Tmp n)
    i <- newITemp
    pStart <- eval start startR; pEnd <- eval end endR
    let pN=n := ((Tmp endR - Tmp startR)+1)
        loop=For i 0 ILt (Tmp n) [Wr (AElem t 1 (Tmp i) (Just a) 8) (Tmp startR), startR+=1]
    pure (Just a, pStart++pEnd++pN:aV++[loop])
aeval (EApp _ (EApp _ (EApp _ (Builtin _ IRange) start) end) incr) t = do
    n <- newITemp; startR <- newITemp; endR <- newITemp; incrR <- newITemp
    (a,aV) <- v8 t (Tmp n)
    i <- newITemp
    pStart <- eval start startR; pEnd <- eval end endR; pIncr <- eval incr incrR
    let pN=n := (Bin Op.IDiv (Tmp endR - Tmp startR) (Tmp incrR)+1)
        loop=For i 0 ILt (Tmp n) [Wr (AElem t 1 (Tmp i) (Just a) 8) (Tmp startR), startR+=Tmp incrR]
    pure (Just a, pStart++pEnd++pIncr++pN:aV++[loop])
aeval (EApp _ (EApp _ (EApp _ (Builtin _ FRange) start) end) steps) t = do
    i <- newITemp
    startR <- newFTemp; incrR <- newFTemp; n <- newITemp
    (a,aV) <- v8 t (Tmp n)
    putStart <- feval start startR; putN <- eval steps n
    putIncr <- feval ((end `eMinus` start) `eDiv` (EApp F (Builtin (Arrow I F) ItoF) steps `eMinus` FLit F 1)) incrR
    let loop=For i 0 ILt (Tmp n) [WrF (AElem t 1 (Tmp i) (Just a) 8) (FTmp startR), MX startR (FTmp startR+FTmp incrR)]
    pure (Just a, putStart++putIncr++putN++aV++[loop])
aeval (EApp res (EApp _ (Builtin _ Cyc) xs) n) t | if1p res = do
    xR <- newITemp; i <- newITemp; nR <- newITemp; nO <- newITemp; szR <- newITemp
    (a,aV) <- v8 t (Tmp nO)
    (lX, plX) <- aeval xs xR
    plN <- eval n nR
    ix <- newITemp
    let body=For i 0 ILt (Tmp nR) [CpyE (AElem t 1 (Tmp ix) (Just a) 8) (AElem xR 1 0 lX 8) (Tmp szR) 8, ix+=Tmp szR]
    pure (Just a, plX ++ plN ++ szR := EAt (ADim xR 0 lX):nO := (Tmp szR*Tmp nR):aV++ix := 0:[body])
aeval (EApp _ (EApp _ (Builtin _ VMul) a) x) t | Just (F, [m,n]) <- tIx$eAnn a, Just s <- cLog n = do
    xR <- newITemp; aR <- newITemp; i <- newITemp; j <- newITemp; mR <- newITemp; nR <- newITemp; z <- newFTemp
    (aL,aV) <- v8 t (Tmp mR)
    (lA, plA) <- aeval a aR; (lX, plX) <- aeval x xR
    let loop = For i 0 ILt (Tmp mR)
                  [ MX z 0,
                    For j 0 ILt (Tmp nR)
                        [ MX z (FTmp z+FAt (AElem aR 2 ((Bin IAsl (Tmp i) (ConstI s))+Tmp j) lA 8)*FAt (AElem xR 1 (Tmp j) lX 8)) ]
                  , WrF (AElem t 1 (Tmp i) (Just aL) 8) (FTmp z)
                  ]
    pure (Just aL,
        plA
        ++plX
        ++mR:=ConstI m
        :aV
        ++nR:=ConstI n
        :[loop])
aeval (EApp _ (EApp _ (Builtin _ VMul) a) x) t | f1 (eAnn x) = do
    xR <- newITemp; aR <- newITemp; i <- newITemp; j <- newITemp; m <- newITemp; n <- newITemp; z <- newFTemp
    (aL,aV) <- v8 t (Tmp m)
    (lA, plA) <- aeval a aR; (lX, plX) <- aeval x xR
    let loop = For i 0 ILt (Tmp m)
                  [ MX z 0,
                    For j 0 ILt (Tmp n)
                        [ MX z (FTmp z+FAt (AElem aR 2 (Tmp n*Tmp i+Tmp j) lA 8)*FAt (AElem xR 1 (Tmp j) lX 8)) ]
                  , WrF (AElem t 1 (Tmp i) (Just aL) 8) (FTmp z)
                  ]
    pure (Just aL,
        plA
        ++plX
        ++m:=EAt (ADim aR 0 lA)
        :aV
        ++n:=EAt (ADim xR 0 lX)
        :[loop])
aeval (EApp _ (EApp _ (Builtin _ Mul) a) b) t | Just (F, _) <- tRnk (eAnn a) = do
    aL <- nextArr t
    aR <- newITemp; bR <- newITemp; i <- newITemp; j <- newITemp; k <- newITemp; m <- newITemp; n <- newITemp; o <- newITemp; z <- newFTemp
    (lA, plA) <- aeval a aR
    (lB, plB) <- aeval b bR
    let loop=For i 0 ILt (Tmp m)
                [For j 0 ILt (Tmp o)
                    [ MX z 0, For k 0 ILt (Tmp n)
                              [MX z (FTmp z+FAt (AElem aR 2 (Tmp n*Tmp i+Tmp k) lA 8)*FAt (AElem bR 2 (Tmp k*Tmp o+Tmp j) lB 8))]
                    , WrF (AElem t 2 (Tmp i*Tmp o+Tmp j) (Just aL) 8) (FTmp z)]
                    ]
    pure (Just aL,
        plA
        ++plB
        ++m:=EAt (ADim aR 0 lA):o:=EAt (ADim bR 1 lB)
        :Ma aL t 2 (Tmp m*Tmp o) 8:diml (t, Just aL) [Tmp m, Tmp o]
        ++n:=EAt (ADim bR 0 lB)
        :[loop])
aeval (EApp _ (EApp _ (Builtin _ ConsE) x) xs) t | tX <- eAnn x, isIF tX = do
    xR <- rtemp tX; xsR <- newITemp
    nR <- newITemp; nϵR <- newITemp
    (a,aV) <- v8 t (Tmp nR)
    plX <- eeval x xR
    (l, plXs) <- aeval xs xsR
    pure (Just a, plXs++plX++nϵR := EAt (ADim xsR 0 l):nR := (Tmp nϵR+1):aV++wt (AElem t 1 0 (Just a) 8) xR:[CpyE (AElem t 1 1 (Just a) 8) (AElem xsR 1 0 l 8) (Tmp nϵR) 8])
aeval (EApp _ (EApp _ (Builtin _ ConsE) x) xs) t | tX <- eAnn x, isΠ tX, sz <- bT tX = do
    xR <- newITemp; xsR <- newITemp
    nR <- newITemp; nϵR <- newITemp
    (_, mSz, _, plX) <- πe x xR
    (lX, plXs) <- aeval xs xsR
    (a,aV) <- vSz t (Tmp nR) sz
    pure (Just a, plXs++m'sa xR mSz++plX++nϵR := EAt (ADim xsR 0 lX):nR := (Tmp nϵR+1):aV++[CpyE (AElem t 1 0 (Just a) sz) (TupM xR Nothing) 1 sz, CpyE (AElem t 1 1 (Just a) sz) (AElem xsR 1 0 lX sz) (Tmp nϵR) sz]++m'pop mSz)
aeval (EApp _ (EApp _ (Builtin _ Snoc) x) xs) t | tX <- eAnn x, isIF tX = do
    xR <- rtemp tX; xsR <- newITemp
    nR <- newITemp; nϵR <- newITemp
    (a,aV) <- v8 t (Tmp nR)
    plX <- eeval x xR
    (l, plXs) <- aeval xs xsR
    pure (Just a, plXs++plX++nϵR := EAt (ADim xsR 0 l):nR := (Tmp nϵR+1):aV++wt (AElem t 1 (Tmp nϵR) (Just a) 8) xR:[CpyE (AElem t 1 0 (Just a) 8) (AElem xsR 1 0 l 8) (Tmp nϵR) 8])
aeval (EApp _ (EApp _ (Builtin _ Snoc) x) xs) t | tX <- eAnn x, isΠ tX, sz <- bT tX = do
    xR <- newITemp; xsR <- newITemp
    nR <- newITemp; nϵR <- newITemp
    (_, mSz, _, plX) <- πe x xR
    (lX, plXs) <- aeval xs xsR
    (a,aV) <- vSz t (Tmp nR) sz
    pure (Just a, plXs++m'sa xR mSz++plX++nϵR := EAt (ADim xsR 0 lX):nR := (Tmp nϵR+1):aV++[CpyE (AElem t 1 (Tmp nϵR) (Just a) sz) (TupM xR Nothing) 1 sz, CpyE (AElem t 1 0 (Just a) sz) (AElem xsR 1 0 lX sz) (Tmp nϵR) sz]++m'pop mSz)
aeval (EApp _ (EApp _ (Builtin _ Re) n) x) t | tX <- eAnn x, isIF tX = do
    xR <- rtemp tX; nR <- newITemp
    (a,aV) <- v8 t (Tmp nR)
    i <- newITemp
    putN <- eval n nR; putX <- eeval x xR
    let loop=For i 0 ILt (Tmp nR) [wt (AElem t 1 (Tmp i) (Just a) 8) xR]
    pure (Just a, putN++aV++putX++[loop])
aeval (EApp _ (EApp _ (Builtin _ Re) n) x) t | tX <- eAnn x, isΠ tX, sz <- bT tX = do
    xR <- newITemp; nR <- newITemp; k <- newITemp
    plN <- eval n nR
    (a,aV) <- vSz t (Tmp nR) sz
    (_, mSz, _, plX) <- πe x xR
    let loop = For k 0 ILt (Tmp nR) [CpyE (AElem t 1 (Tmp k) (Just a) sz) (TupM xR Nothing) 1 sz]
    pure (Just a, m'sa xR mSz++plX++plN++aV++loop:m'pop mSz)
aeval (EApp _ (EApp _ (Builtin _ Re) n) x) t | Arr _ tO <- eAnn x, sz <- bT tO = do
    a <- nextArr t
    xR <- newITemp; nR <- newITemp; k <- newITemp
    (lX, plX) <- aeval x xR; plN <- eval n nR
    xRnk <- newITemp; oRnk <- newITemp
    szX <- newITemp
    let loop = For k 0 ILt (Tmp nR) [CpyE (AElem t (Tmp oRnk) (Tmp k*Tmp szX) (Just a) sz) (AElem xR (Tmp xRnk) 0 lX sz) (Tmp szX) sz]
    pure (Just a,
        plX
        ++xRnk:=EAt (ARnk xR lX):oRnk:=(Tmp xRnk+1):SZ szX xR (Tmp xRnk) lX
        :plN
        ++Ma a t (Tmp oRnk) (Tmp szX*Tmp nR) sz:Wr (ADim t 0 (Just a)) (Tmp nR):CpyD (ADim t 1 (Just a)) (ADim xR 0 lX) (Tmp xRnk)
        :[loop])
aeval (EApp oTy (Builtin _ Init) x) t | if1p oTy = do
    xR <- newITemp; nR <- newITemp
    (a,aV) <- v8 t (Tmp nR)
    (lX, plX) <- aeval x xR
    pure (Just a, plX++nR := (EAt (ADim xR 0 lX)-1):aV++[CpyE (AElem t 1 0 (Just a) 8) (AElem xR 1 0 lX 8) (Tmp nR) 8])
aeval (EApp oTy (Builtin _ Tail) x) t | if1p oTy = do
    xR <- newITemp; nR <- newITemp
    (a,aV) <- v8 t (Tmp nR)
    (lX, plX) <- aeval x xR
    pure (Just a, plX++nR := (EAt (ADim xR 0 lX)-1):aV++[CpyE (AElem t 1 0 (Just a) 8) (AElem xR 1 1 lX 8) (Tmp nR) 8])
aeval (EApp _ (EApp _ (EApp _ (Builtin _ Zip) op) xs) ys) t | (Arrow tX (Arrow tY tC)) <- eAnn op, nind tX && nind tY && nind tC = do
    aPX <- newITemp; aPY <- newITemp
    nR <- newITemp; i <- newITemp
    let zSz=bT tC
    (a,aV) <- vSz t (Tmp nR) zSz
    (lX, plEX) <- aeval xs aPX; (lY, plEY) <- aeval ys aPY
    (step, pinches) <- aS op [(tX, AElem aPX 1 (Tmp i) lX), (tY, AElem aPY 1 (Tmp i) lY)] tC (AElem t 1 (Tmp i) (Just a))
    let loop=For i 0 ILt (Tmp nR) step
    pure (Just a, plEX++plEY++nR := EAt (ADim aPX 0 lX):aV++sas pinches [loop])
aeval (EApp _ (EApp _ (EApp _ (Builtin _ ScanS) op) seed) e) t | (Arrow tX (Arrow tY _)) <- eAnn op, isIF tX && isIF tY = do
    aP <- newITemp
    acc <- rtemp tX; x <- rtemp tY
    i <- newITemp; n <- newITemp
    plS <- eeval seed acc
    (a,aV) <- v8 t (Tmp n)
    (l, plE) <- aeval e aP
    ss <- writeRF op [acc, x] acc
    let loopBody=wt (AElem t 1 (Tmp i) (Just a) 8) acc:mt (AElem aP 1 (Tmp i) l 8) x:ss
        loop=for (eAnn e) i 0 ILt (Tmp n) loopBody
    pure (Just a, plE++plS++n := (EAt (ADim aP 0 l)+1):aV++[loop])
aeval (EApp _ (EApp _ (Builtin _ Scan) op) xs) t | (Arrow tAcc (Arrow tX _)) <- eAnn op, isIF tAcc && isIF tX = do
    aP <- newITemp
    acc <- rtemp tAcc; x <- rtemp tX
    i <- newITemp; n <- newITemp
    (a,aV) <- v8 t (Tmp n)
    (l, plE) <- aeval xs aP
    ss <- writeRF op [acc, x] acc
    let loopBody=wt (AElem t 1 (Tmp i-1) (Just a) 8) acc:mt (AElem aP 1 (Tmp i) l 8) x:ss
        loop=for1 (eAnn xs) i 1 ILeq (Tmp n) loopBody
    pure (Just a, plE++n := EAt (ADim aP 0 l):aV++mt (AElem aP 1 0 l 8) acc:[loop])
aeval (EApp oTy (EApp _ (Builtin _ (DI n)) op) xs) t | Just ot <- if1 oTy, if1p (eAnn xs) = do
    aP <- newITemp
    slopP <- newITemp
    szR <- newITemp; sz'R <- newITemp; i <- newITemp
    fR <- rtemp ot
    (a,aV) <- v8 t (Tmp sz'R)
    (_, ss) <- writeF op [AA slopP Nothing] fR
    let szSlop=fromIntegral$16+8*n
    (lX, plX) <- aeval xs aP
    let sz'=Tmp szR-fromIntegral(n-1)
    let loopBody=CpyE (AElem slopP 1 0 Nothing 8) (AElem aP 1 (Tmp i) lX 8) (fromIntegral n) 8:ss++[wt (AElem t 1 (Tmp i) (Just a) 8) fR]
        loop=For i 0 ILt (Tmp sz'R) loopBody
    pure (Just a, plX++szR := EAt (ADim aP 0 lX):sz'R := sz':aV++Sa slopP szSlop:Wr (ARnk slopP Nothing) 1:Wr (ADim slopP 0 Nothing) (fromIntegral n):loop:[Pop szSlop])
aeval (EApp _ (EApp _ (Builtin _ Rot) n) xs) t | if1p (eAnn xs) = do
    xsR <- newITemp; nR <- newITemp; c <- newITemp; szR <- newITemp
    plN <- eval n nR
    (lX, plX) <- aeval xs xsR
    (a, aV) <- v8 t (Tmp szR)
    pure (Just a, plX++plN++szR := EAt (ADim xsR 0 lX):aV++Ifn't (IRel IGeq (Tmp nR) 0) [nR+=Tmp szR]:c := (Tmp szR-Tmp nR):[CpyE (AElem t 1 0 (Just a) 8) (AElem xsR 1 (Tmp nR) lX 8) (Tmp c) 8, CpyE (AElem t 1 (Tmp c) (Just a) 8) (AElem xsR 1 0 lX 8) (Tmp nR) 8])
aeval (Id _ (AShLit ns es)) t | Just ws <- mIFs es = do
    let rnk=fromIntegral$length ns
    n <- nextAA
    modify (addAA n (rnk:fmap fromIntegral ns++ws))
    pure (Nothing, [t := LA n])
aeval (EApp _ (Builtin _ T) x) t | Just (ty, ixes) <- tIx (eAnn x), rnk <- fromIntegral$length ixes, any isJust (cLog<$>ixes) = do
    a <- nextArr t
    let sze=bT ty; rnkE=ConstI rnk
    xR <- newITemp; xd <- newITemp; td <- newITemp
    (lX, plX) <- aeval x xR
    (dts, plDs) <- plDim rnk (xR, lX)
    let n:sstrides = reverse $ scanl' (*) 1 (reverse ixes); _:dstrides=reverse $ scanl' (*) 1 ixes
    is <- traverse (\_ -> newITemp) [1..rnk]
    let loop=thread (zipWith (\i tt -> (:[]) . For i 0 ILt (Tmp tt)) is dts) [CpyE (At td (ConstI<$>dstrides) (Tmp<$>reverse is) (Just a) sze) (At xd (ConstI<$>sstrides) (Tmp<$>is) lX sze) 1 sze]
    pure (Just a, plX++plDs++Ma a t (ConstI rnk) (ConstI n) sze:diml (t, Just a) (Tmp<$>reverse dts)++xd:=DP xR rnkE:td:=DP t rnkE:loop)
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
    pure (Just a, plX++plDs++plSs++Ma a t (ConstI rnk) (Tmp n) sze:diml (t, Just a) (Tmp<$>reverse dts)++init plSd++xd := (Tmp xR+dO):td := (Tmp t+dO):loop)
aeval (EApp _ (EApp _ (EApp _ (Builtin _ Outer) op) xs) ys) t | (Arrow tX (Arrow tY tC)) <- eAnn op, nind tX && nind tY && nind tC = do
    a <- nextArr t
    xR <- newITemp; yR <- newITemp; szX <- newITemp; szY <- newITemp; i <- newITemp; j <- newITemp; k <- newITemp
    let zSz=bT tC
    (lX, plX) <- aeval xs xR
    (lY, plY) <- aeval ys yR
    (step, pinches) <- aS op [(tX ,AElem xR 1 (Tmp i) lX), (tY, AElem yR 1 (Tmp j) lY)] tC (AElem t 2 (Tmp k) (Just a))
    let loop=For i 0 ILt (Tmp szX) [For j 0 ILt (Tmp szY) (step++[k+=1])]
    pure (Just a, plX++plY++szX := EAt (ADim xR 0 lX):szY := EAt (ADim yR 0 lY):Ma a t 2 (Tmp szX*Tmp szY) zSz:diml (t, Just a) [Tmp szX, Tmp szY]++k:=0:sas pinches [loop])
aeval (EApp _ (EApp _ (EApp _ (Builtin _ Outer) op) xs) ys) t | (Arrow tX (Arrow tY tC)) <- eAnn op, Arr _ tEC <- tC, nind tX && nind tY && nind tEC = do
    a <- nextArr t
    xR <- newITemp; yR <- newITemp; szX <- newITemp; szY <- newITemp; szZ <- newITemp; i <- newITemp; j <- newITemp; k <- newITemp; l <- newITemp
    rnkZ <- newITemp; rnkO <- newITemp
    let szXT=bT tX; szYT=bT tY; szZT=bT tEC
    z <- newITemp
    (lX, plX) <- aeval xs xR
    (lY, plY) <- aeval ys yR
    (x, wX, pinchX) <- arg tX (AElem xR 1 (Tmp i) lX szXT)
    (y, wY, pinchY) <- arg tY (AElem yR 1 (Tmp j) lY szYT)
    (lZ0, ss0) <- writeF op [ra x, ra y] (Right z)
    (lZ, ss) <- writeF op [ra x, ra y] (Right z)
    let step=[wX, wY]++ss++[CpyE (AElem t (Tmp rnkO) (Tmp k*Tmp szZ) (Just a) szZT) (AElem z (Tmp rnkZ) 0 lZ szZT) (Tmp szZ) szZT, k+=1]
        loop=For i 0 ILt (Tmp szX) [For j 0 ILt (Tmp szY) step]
    pure (Just a,
        plX
        ++plY
        ++i:=0:j:=0:
        sas [pinchX, pinchY] (
        wX:wY:ss0
        ++rnkZ:=EAt (ARnk z lZ0)
        :rnkO:=(Tmp rnkZ+2)
        :SZ szZ z (Tmp rnkZ) lZ0
        :szX:=EAt (ADim xR 0 lX)
        :szY:=EAt (ADim yR 0 lY)
        :Ma a t (Tmp rnkO) (Tmp szX*Tmp szY*Tmp szZ) szZT
        :diml (t, Just a) [Tmp szX, Tmp szY]
        ++[CpyD (ADim t 2 (Just a)) (ADim z 0 lZ0) (Tmp rnkZ), loop]
        ))
aeval (EApp _ (EApp _ (Builtin _ Succ) op) xs) t | Arrow tX (Arrow _ tZ) <- eAnn op, nind tX && nind tZ = do
    xR <- newITemp
    szR <- newITemp; sz'R <- newITemp
    let zSz=bT tZ
    (a,aV) <- vSz t (Tmp sz'R) zSz
    (lX, plX) <- aeval xs xR
    i <- newITemp
    (step, pinches) <- aS op [(tX, AElem xR 1 (Tmp i+1) lX), (tX, AElem xR 1 (Tmp i) lX)] tZ (AElem t 1 (Tmp i) (Just a))
    let loop=For i 0 ILt (Tmp sz'R) step
    pure (Just a, plX++szR := EAt (ADim xR 0 lX):sz'R := (Tmp szR-1):aV++sas pinches [loop])
aeval (EApp oTy (Builtin _ RevE) e) t | Just ty <- if1 oTy = do
    eR <- newITemp; n <- newITemp; i <- newITemp; o <- rtemp ty
    (a,aV) <- v8 t (Tmp n)
    (lE, plE) <- aeval e eR
    let loop=For i 0 ILt (Tmp n) [mt (AElem eR 1 (Tmp n-Tmp i-1) lE 8) o, wt (AElem t 1 (Tmp i) (Just a) 8) o]
    pure (Just a, plE++n := EAt (ADim eR 0 lE):aV++[loop])
aeval (EApp oTy (EApp _ (EApp _ (Builtin _ Gen) seed) op) n) t | Just ty <- if1 oTy = do
    nR <- newITemp; plN <- eval n nR; i <- newITemp
    acc <- rtemp ty
    plS <- eeval seed acc
    (a,aV) <- v8 t (Tmp nR)
    ss <- writeRF op [acc] acc
    let loop=For i 0 ILt (Tmp nR) (wt (AElem t 1 (Tmp i) (Just a) 8) acc:ss)
    pure (Just a, plS++plN++aV++[loop])
aeval (EApp _ (EApp _ (EApp _ (Builtin _ Gen) seed) op) n) t | isΠIF (eAnn seed) = do
    nR <- newITemp; plN <- eval n nR; i <- newITemp
    acc <- newITemp
    (szs,mP,_,plS) <- πe seed acc
    let πsz=last szs
    (a,aV) <- vSz t (Tmp nR) πsz
    (_, ss) <- writeF op [IPA acc] (Right acc)
    let loop=For i 0 ILt (Tmp nR) (CpyE (AElem t 1 (Tmp i) (Just a) πsz) (TupM acc Nothing) 1 πsz:ss)
    pure (Just a, m'sa acc mP++plS++plN++aV++loop:m'pop mP)
aeval (EApp oTy (EApp _ (Builtin _ (Conv is)) f) x) t
    | (Arrow _ tC) <- eAnn f
    , Just (tX, xRnk) <- tRnk (eAnn x)
    , Just (_, oRnk) <- tRnk oTy
    , isIF tC && isIF tX && oRnk==xRnk = do
    a <- nextArr t
    xR <- newITemp; xRd <- newITemp; szR <- newITemp; slopP <- newITemp
    (lX, plX) <- aeval x xR
    (dts, plDs) <- plDim xRnk (xR, lX)
    (tdims, dims) <- unzip <$> zipWithM (\dt i -> do {odim <- newITemp; pure (odim, odim := (Tmp dt-fromIntegral (i-1)))}) dts is
    io <- traverse (\_ -> newITemp) tdims
    iw <- traverse (\_ -> newITemp) is; j <- newITemp
    let slopSz=product is; slopRnk=length is; slopE=fromIntegral ((slopSz+slopRnk+1)*8); slopDims=fromIntegral<$>is
        rnk=ConstI oRnk
    z <- rtemp tC; k <- newITemp; o <- rtemp tX
    (_, ss) <- writeF f [AA slopP Nothing] z
    (sts, plS) <- offByDim (reverse dts)
    let _:strides = sts; sss=init plS
        extrWindow = j:=0:forAll iw (ConstI . fromIntegral<$>is)
                            [mt (At xRd (Tmp<$>strides) (zipWith (\jϵ iϵ -> Tmp jϵ+Tmp iϵ) iw io) lX 8) o, wt (AElem slopP (ConstI$fromIntegral slopRnk) (Tmp j) Nothing 8) o, j+=1]
        step = extrWindow++ss++[wt (AElem t rnk (Tmp k) (Just a) 8) z, k+=1]
        loop=forAll io (Tmp<$>tdims) step
    pure (Just a,
        plX
        ++plDs
        ++dims
        ++sss
        ++PlProd szR (Tmp<$>tdims):Ma a t rnk (Tmp szR) 8:diml (t, Just a) (Tmp<$>tdims)
        ++Sa slopP slopE:Wr (ARnk slopP Nothing) (ConstI$fromIntegral slopRnk):diml (slopP, Nothing) slopDims
        ++xRd:=DP xR (ConstI xRnk):k:=0:loop
        ++[Pop slopE])
aeval e _ = error (show e)

plC :: E (T ()) -> CM ([CS] -> [CS], CE)
plC (ILit _ i) = pure (id, ConstI$fromIntegral i)
plC e          = do {t <- newITemp; pl <- eval e t; pure ((pl++), Tmp t)}

plEV :: E (T ()) -> CM ([CS] -> [CS], Temp)
plEV (Var I x) = do
    st <- gets vars
    pure (id, getT st x)
plEV e = do
    t <- newITemp
    pl <- eval e t
    pure ((pl++), t)

eval :: E (T ()) -> Temp -> CM [CS]
eval (LLet _ b e) t = do
    ss <- llet b
    (ss++) <$> eval e t
eval (ILit _ n) t = pure [t := fromInteger n]
eval (Var _ x) t = do
    st <- gets vars
    pure [t := Tmp (getT st x)]
eval (EApp _ (EApp _ (Builtin _ A.R) e0) e1) t = do
    e0R <- newITemp; e1R <- newITemp
    plE0 <- eval e0 e0R; plE1 <- eval e1 e1R
    pure $ plE0 ++ plE1 ++ [Rnd t, t := (Bin IRem (Tmp t) (Tmp e1R - Tmp e0R + 1) + Tmp e0R)]
eval (EApp _ (EApp _ (EApp _ (Builtin _ FoldS) op) seed) e) acc | (Arrow _ (Arrow tX _)) <- eAnn op, isIF tX = do
    x <- rtemp tX
    eR <- newITemp
    szR <- newITemp
    i <- newITemp
    (l, plE) <- aeval e eR
    plAcc <- eval seed acc
    ss <- writeRF op [Right acc, x] (Right acc)
    let loopBody=mt (AElem eR 1 (Tmp i) l 8) x:ss
        loop=for (eAnn e) i 0 ILt (Tmp szR) loopBody
    pure $ plE++plAcc++szR := EAt (ADim eR 0 l):[loop]
eval (EApp _ (EApp _ (Builtin _ op) (ILit _ n)) e1) t | Just cop <- mOp op = do
    (pl1,t1) <- plEV e1
    pure $ pl1 $ [t:=Bin cop (ConstI$fromIntegral n) (Tmp t1)]
eval (EApp _ (EApp _ (Builtin _ op) e0) (ILit _ n)) t | Just cop <- mOp op = do
    (pl0,t0) <- plEV e0
    pure $ pl0 $ [t:=Bin cop (Tmp t0) (ConstI$fromIntegral n)]
eval (EApp _ (EApp _ (Builtin _ op) e0) e1) t | Just cop <- mOp op = do
    (pl0,t0) <- plEV e0; (pl1,t1) <- plEV e1
    pure $ pl0 $ pl1 [t := Bin cop (Tmp t0) (Tmp t1)]
eval (EApp _ (EApp _ (Builtin _ Max) e0) e1) t = do
    (pl0,t0) <- plEV e0
    -- in case t==t1
    t1 <- newITemp
    pl1 <- eval e1 t1
    pure $ pl0 $ pl1 ++ [t := Tmp t0, Cmov (IRel IGt (Tmp t1) (Tmp t0)) t (Tmp t1)]
eval (EApp _ (EApp _ (Builtin _ Min) e0) e1) t = do
    (pl0,t0) <- plEV e0
    -- in case t==t1
    t1 <- newITemp
    pl1 <- eval e1 t1
    pure $ pl0 $ pl1 ++ [t := Tmp t0, Cmov (IRel ILt (Tmp t1) (Tmp t0)) t (Tmp t1)]
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
    pure $ plE ++ plI ++ [t := EAt (AElem eR 1 (Tmp iR) lE 8)]
eval (EApp _ (Builtin _ Head) xs) t = do
    a <- newITemp
    (l, plX) <- aeval xs a
    pure $ plX ++ [t := EAt (AElem a 1 0 l 8)]
eval (EApp _ (Builtin _ Last) xs) t = do
    a <- newITemp
    (l, plX) <- aeval xs a
    pure $ plX ++ [t := EAt (AElem a 1 (EAt (ADim a 0 l)-1) l 8)]
eval (EApp _ (Builtin _ Size) xs) t | Just (_, 1) <- tRnk (eAnn xs) = do
    xsR <- newITemp
    (l, plE) <- aeval xs xsR
    pure $ plE ++ [t := EAt (ADim xsR 0 l)]
eval (EApp _ (Builtin _ Size) xs) t = do
    xsR <- newITemp
    (l, plE) <- aeval xs xsR
    rnkR <- newITemp
    pure $ plE ++ [rnkR := EAt (ARnk xsR l), SZ t xsR (Tmp rnkR) l]
eval (EApp _ (Builtin _ Floor) x) t = do
    xR <- newFTemp
    plX <- feval x xR
    pure $ plX ++ [t := CFloor (FTmp xR)]
eval (EApp _ (Builtin _ (TAt i)) e) t = do
    k <- newITemp
    (offs, a, _, plT) <- πe e k
    pure $ m'sa t a++plT ++ t := EAt (Raw k (ConstI$offs!!(i-1)) Nothing 1):m'pop a
eval (EApp _ (EApp _ (Builtin _ IOf) p) xs) t | (Arrow tD _) <- eAnn p, nind tD = do
    xsR <- newITemp; pR <- newITemp
    szR <- newITemp; i <- newITemp; done <- newITemp
    (lX, plX) <- aeval xs xsR
    let szX=bT tD
    (x, wX, pinch) <- arg tD (AElem xsR 1 (Tmp i) lX szX)
    ss <- writeRF p [x] (Right pR)
    let loop=While done INeq 1 (wX:ss++[If (Is pR) [t:=Tmp i, done:=1] [], i+=1, Cmov (IRel IGeq (Tmp i) (Tmp szR)) done 1])
    pure $ plX ++ szR:=EAt (ADim xsR 0 lX):t:=(-1):done:=0:i:=0:m'p pinch [loop]
eval (Cond _ p e0 e1) t = snd <$> cond p e0 e1 (Right t)
eval (Id _ (FoldOfZip zop op [p])) acc | Just tP <- if1 (eAnn p) = do
    x <- rtemp tP
    pR <- newITemp
    szR <- newITemp
    i <- newITemp
    (lP, plP) <- aeval p pR
    ss <- writeRF op [Right acc, x] (Right acc)
    let step = mt (AElem  pR 1 (Tmp i) lP 8) x:ss
        loop = for1 (eAnn p) i 1 ILt (Tmp szR) step
    sseed <- writeRF zop [x] (Right acc)
    pure $ plP++szR := EAt (ADim pR 0 lP):mt (AElem pR 1 0 lP 8) x:sseed++[loop]
eval (Id _ (FoldOfZip zop op [p, q])) acc | Just tP <- if1 (eAnn p), Just tQ <- if1 (eAnn q) = do
    x <- rtemp tP; y <- rtemp tQ
    pR <- newITemp; qR <- newITemp
    szR <- newITemp
    i <- newITemp
    (lP, plP) <- aeval p pR; (lQ, plQ) <- aeval q qR
    ss <- writeRF op [Right acc, x, y] (Right acc)
    let step = mt (AElem pR 1 (Tmp i) lP 8) x:mt (AElem qR 1 (Tmp i) lQ 8) y:ss
        loop = for1 (eAnn p) i 1 ILt (Tmp szR) step
    seed <- writeRF zop [x,y] (Right acc)
    pure $ plP++plQ++szR := EAt (ADim pR 0 lP):mt (AElem pR 1 0 lP 8) x:mt (AElem qR 1 0 lQ 8) y:seed++[loop]
eval e _          = error (show e)

frel :: Builtin -> Maybe FRel
frel Gte=Just FGeq; frel Lte=Just FLeq; frel Eq=Just FEq; frel Neq=Just FNeq; frel Lt=Just FLt; frel Gt=Just FGt; frel _=Nothing

mFop :: Builtin -> Maybe FBin
mFop Plus=Just FPlus; mFop Times=Just FTimes; mFop Minus=Just FMinus; mFop Div=Just FDiv; mFop Exp=Just FExp; mFop Max=Just FMax; mFop Min=Just FMin; mFop _=Nothing

mOp :: Builtin -> Maybe IBin
mOp Plus=Just IPlus;mOp Times=Just ITimes;mOp Minus=Just IMinus; mOp Mod=Just IRem;mOp _=Nothing

mFun :: Builtin -> Maybe FUn
mFun Sqrt=Just FSqrt; mFun Log=Just FLog; mFun Sin=Just FSin; mFun Cos=Just FCos; mFun Abs=Just FAbs; mFun _=Nothing

mFEval :: E (T ()) -> Maybe (CM CFE)
mFEval (FLit _ d) = Just (pure $ ConstF d)
mFEval (Var _ x) = Just $ do
    st <- gets dvars
    pure (FTmp (getT st x))
mFEval _ = Nothing

cond :: E (T ()) -> E (T ()) -> E (T ()) -> Either FTemp Temp -> CM (Maybe AL, [CS])
cond (EApp _ (EApp _ (Builtin (Arrow F _) op) c0) c1) e e1 (Left t) | Just cmp <- frel op, Just cfe <- mFEval e1 = do
    c0R <- newFTemp; c1R <- newFTemp
    plC0 <- feval c0 c0R; plC1 <- feval c1 c1R
    eR <- newFTemp; fe <- cfe
    plE <- feval e eR
    pure (Nothing, plC0 ++ plC1 ++ [MX t fe] ++ plE ++ [Fcmov (FRel cmp (FTmp c0R) (FTmp c1R)) t (FTmp eR)])
cond (EApp _ (EApp _ (Builtin (Arrow F _) o) c0) c1) e0 e1 t | Just f <- frel o, isIF (eAnn e0) = do
    c0R <- newFTemp; c1R <- newFTemp
    plC0 <- feval c0 c0R; plC1 <- feval c1 c1R
    plE0 <- eeval e0 t; plE1 <- eeval e1 t
    pure (Nothing, plC0 ++ plC1 ++ [If (FRel f (FTmp c0R) (FTmp c1R)) plE0 plE1])
cond (EApp _ (EApp _ (Builtin (Arrow I _) op) c) (ILit _ i)) e e1 (Left t) | Just cmp <- rel op, Just cfe <- mFEval e1 = do
    cR <- newITemp
    plC <- eval c cR
    eR <- newFTemp; fe <- cfe
    plE <- feval e eR
    pure (Nothing, plC ++ [MX t fe] ++ plE ++ [Fcmov (IRel cmp (Tmp cR) (fromIntegral i)) t (FTmp eR)])
cond (EApp _ (EApp _ (Builtin (Arrow I _) op) c0) c1) e e1 (Left t) | Just cmp <- rel op, Just cfe <- mFEval e1 = do
    c0R <- newITemp; c1R <- newITemp
    plC0 <- eval c0 c0R; plC1 <- eval c1 c1R
    eR <- newFTemp; fe <- cfe
    plE <- feval e eR
    pure (Nothing, plC0 ++ plC1 ++ [MX t fe] ++ plE ++ [Fcmov (IRel cmp (Tmp c0R) (Tmp c1R)) t (FTmp eR)])
cond (EApp _ (EApp _ (Builtin (Arrow I _) op) c0) c1) e0 e1 t | Just cmp <- rel op, isIF (eAnn e0) = do
    c0R <- newITemp; c1R <- newITemp
    plC0 <- eval c0 c0R; plC1 <- eval c1 c1R
    plE0 <- eeval e0 t; plE1 <- eeval e1 t
    pure (Nothing, plC0 ++ plC1 ++ [If (IRel cmp (Tmp c0R) (Tmp c1R)) plE0 plE1])
cond p e0 e1 t | isIF (eAnn e0) = do
    pR <- newITemp
    plP <- eval p pR; plE0 <- eeval e0 t; plE1 <- eeval e1 t
    pure (Nothing, plP ++ [If (Is pR) plE0 plE1])

feval :: E (T ()) -> FTemp -> CM [CS]
feval (LLet _ b e) t = do
    ss <- llet b
    (ss++) <$> feval e t
feval (ILit _ x) t = pure [MX t (ConstF $ fromIntegral x)] -- if it overflows you deserve it
feval (FLit _ x) t = pure [MX t (ConstF x)]
feval (Var _ x) t = do
    st <- gets dvars
    pure [MX t (FTmp $ getT st x)]
feval (EApp _ (EApp _ (Builtin _ A.R) e0) e1) t = do
    e0R <- newFTemp; e1R <- newFTemp; iR <- newITemp
    plE0 <- feval e0 e0R; plE1 <- feval e1 e1R
    pure $ plE0 ++ plE1 ++ [Rnd iR, MX t (IE (Tmp iR)), MX t ((FTmp e1R - FTmp e0R) * (FTmp t / (2*9223372036854775807) + 0.5) + FTmp e0R)]
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
    pure $ plX ++ plN ++ [MX t 1, While nR IGt 0 [Ifn't (IUn IEven (Tmp nR)) [MX t (FTmp t*FTmp xR)], nR := Bin IAsr (Tmp nR) 1, MX xR (FTmp xR*FTmp xR)]]
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
        loop = for1 (eAnn p) i 1 ILt (Tmp szR) step
    sseed <- writeRF zop [x] (Left acc)
    pure $ plP++szR := EAt (ADim pR 0 lP):mt (AElem pR 1 0 lP 8) x:sseed++[loop]
feval (Id _ (FoldOfZip zop op [EApp _ (EApp _ (EApp _ (Builtin _ FRange) (FLit _ start)) (FLit _ end)) (ILit _ steps), ys])) acc | Just tQ <- if1 (eAnn ys) = do
    x <- newFTemp; yR <- newITemp; y <- rtemp tQ
    incrR <- newFTemp; i <- newITemp
    plY <- eeval (EApp tQ (Builtin undefined Head) ys) y
    (lY, plYs) <- aeval ys yR
    plIncr <- feval (FLit F$(end-start)/realToFrac (steps-1)) incrR
    seed <- writeRF zop [Left x, y] (Left acc)
    ss <- writeRF op [Left acc, Left x, y] (Left acc)
    pure $ plYs ++ plY ++ MX x (ConstF start):seed ++ plIncr ++ [For i 1 ILt (ConstI$fromIntegral steps) (mt (AElem yR 1 (Tmp i) lY 8) y:MX x (FTmp x+FTmp incrR):ss)]
feval (Id _ (FoldOfZip zop op [EApp _ (EApp _ (EApp _ (Builtin _ FRange) start) end) steps, ys])) acc | Just tQ <- if1 (eAnn ys) = do
    x <- newFTemp; yR <- newITemp; y <- rtemp tQ
    incrR <- newFTemp; n <- newITemp; i <- newITemp
    plX <- feval start x; plY <- eeval (EApp tQ (Builtin undefined Head) ys) y
    (lY, plYs) <- aeval ys yR
    plN <- eval steps n
    plIncr <- feval ((end `eMinus` start) `eDiv` (EApp F (Builtin (Arrow I F) ItoF) steps `eMinus` FLit F 1)) incrR
    seed <- writeRF zop [Left x, y] (Left acc)
    ss <- writeRF op [Left acc, Left x, y] (Left acc)
    pure $ plYs ++ plY ++ plX ++ seed ++ plIncr ++ plN ++ [For i 1 ILt (Tmp n) (mt (AElem yR 1 (Tmp i) lY 8) y:MX x (FTmp x+FTmp incrR):ss)]
feval (Id _ (FoldOfZip zop op [EApp _ (EApp _ (EApp _ (Builtin _ IRange) start) _) incr, ys])) acc | Just tQ <- if1 (eAnn ys) = do
    x <- newITemp; yR <- newITemp; y <- rtemp tQ
    incrR <- newITemp; szR <- newITemp; i <- newITemp
    plX <- eval start x; plY <- eeval (EApp tQ (Builtin undefined Head) ys) y; (lY, plYs) <- aeval ys yR; plI <- eval incr incrR
    seed <- writeRF zop [Right x, y] (Left acc)
    ss <- writeRF op [Left acc, Right x, y] (Left acc)
    pure $ plYs ++ plY ++ plX ++ seed ++ plI ++ szR := EAt (ADim yR 0 lY):[For i 1 ILt (Tmp szR) (mt (AElem yR 1 (Tmp i) lY 8) y:x+=Tmp incrR:ss)]
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
    pure $ plP++plQ++szR := EAt (ADim pR 0 lP):mt (AElem pR 1 0 lP 8) x:mt (AElem qR 1 0 lQ 8) y:seed++[loop]
feval (EApp _ (EApp _ (Builtin _ Fold) op) e) acc | (Arrow tX _) <- eAnn op, isF tX = do
    x <- newFTemp
    aP <- newITemp
    szR <- newITemp
    i <- newITemp
    (l, plE) <- aeval e aP
    ss <- writeRF op [Left acc, Left x] (Left acc)
    let loopBody=MX x (FAt (AElem aP 1 (Tmp i) l 8)):ss
        loop=for1 (eAnn e) i 1 ILt (Tmp szR) loopBody
    pure $ plE++szR := EAt (ADim aP 0 l):MX acc (FAt (AElem aP 1 0 l 8)):[loop]
feval (EApp _ (EApp _ (EApp _ (Builtin _ Foldl) op) seed) e) acc | (Arrow _ (Arrow tX _)) <- eAnn op, isIF tX = do
    x <- rtemp tX
    eR <- newITemp
    i <- newITemp
    (l, plE) <- aeval e eR
    plAcc <- feval seed acc
    ss <- writeRF op [x, Left acc] (Left acc)
    let loopBody=mt (AElem eR 1 (Tmp i) l 8) x:ss++[i := (Tmp i-1)]
        loop=While i IGeq 0 loopBody
    pure $ plE++plAcc++i := (EAt (ADim eR 0 l)-1):[loop]
feval (EApp _ (EApp _ (EApp _ (Builtin _ FoldA) op) seed) xs) acc | (Arrow _ (Arrow tX _)) <- eAnn op, isIF tX = do
    x <- rtemp tX
    xsR <- newITemp; rnkR <- newITemp; szR <- newITemp; i <- newITemp; k <- newITemp
    (lX, plE) <- aeval xs xsR
    plAcc <- feval seed acc
    ss <- writeRF op [x, Left acc] (Left acc)
    let step=mt (AElem xsR (Tmp rnkR) (Tmp k) lX 8) x:ss
        loop=for (eAnn xs) k 0 ILt (Tmp szR) step
        plSz = case tIx (eAnn xs) of {Just (_, is) -> szR:=ConstI (product is); Nothing -> SZ szR xsR (Tmp rnkR) lX}
    pure $ plE ++ plAcc ++ [rnkR := EAt (ARnk xsR lX), plSz, loop]
feval (EApp _ (EApp _ (EApp _ (Builtin _ FoldS) op) seed) (EApp _ (EApp _ (EApp _ (Builtin _ IRange) start) end) incr)) acc = do
    i <- newITemp
    endR <- newITemp
    (plI,iE) <- plC incr
    plStart <- eval start i; plAcc <- feval seed acc; plEnd <- eval end endR
    ss <- writeRF op [Left acc, Right i] (Left acc)
    pure $ plStart ++ plAcc ++ plEnd ++ plI [While i ILeq (Tmp endR) (ss++[i+=iE])]
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
        loop=for (eAnn e) i 0 ILt (Tmp szR) loopBody
    pure $ plE++plAcc++szR := EAt (ADim eR 0 l):[loop]
feval (EApp _ (Builtin _ (TAt i)) e) t = do
    k <- newITemp
    (offs, a, _, plT) <- πe e k
    pure $ m'sa k a++plT ++ MX t (FAt (Raw k (ConstI$offs!!(i-1)) Nothing 1)):m'pop a
feval (EApp _ (Var _ f) x) t | isF (eAnn x) = do
    st <- gets fvars
    let (l, [FA a], Left r) = getT st f
    plX <- feval x a
    pure $ plX ++ [G l, MX t (FTmp r)]
feval (Id _ (FoldGen seed g f n)) t = do
    seedR <- newFTemp; x <- newFTemp; acc <- newFTemp
    nR <- newITemp; k <- newITemp
    plSeed <- feval seed seedR; plN <- eval n nR
    uss <- writeRF g [Left x] (Left x)
    fss <- writeRF f [Left acc, Left x] (Left acc)
    pure $ plSeed++plN++[MX acc (FTmp seedR), MX x (FTmp seedR), For k 0 ILt (Tmp nR) (fss++uss), MX t (FTmp acc)]
feval e _ = error (show e)

m'pop :: Maybe CE -> [CS]
m'pop = maybe [] ((:[]).Pop)

m'sa :: Temp -> Maybe CE -> [CS]
m'sa t = maybe []  ((:[]).Sa t)

πe :: E (T ()) -> Temp -> CM ([Int64], Maybe CE, [AL], [CS]) -- element offsets, size to be popped off the stack, array labels kept live
πe (EApp (P tys) (Builtin _ Head) xs) t | offs <- szT tys, sz <- last offs, szE <- ConstI sz = do
    xR <- newITemp
    (lX, plX) <- aeval xs xR
    pure (offs, Just szE, [], plX++[CpyE (TupM t Nothing) (AElem xR 1 0 lX sz) 1 sz])
πe (EApp (P tys) (Builtin _ Last) xs) t | offs <- szT tys, sz <- last offs, szE <- ConstI sz = do
    xR <- newITemp
    (lX, plX) <- aeval xs xR
    pure (offs, Just szE, [], plX++[CpyE (TupM t Nothing) (AElem xR 1 (EAt (ADim xR 0 lX)-1) lX sz) 1 sz])
πe (Tup (P tys) es) t | offs <- szT tys, sz <- last offs, szE <- ConstI sz = do
    (ls, ss) <- unzip <$>
        zipWithM (\e off ->
            case eAnn e of
                F     -> do {f <- newFTemp; plX <- feval e f; pure (Nothing, plX++[WrF (Raw t (ConstI off) Nothing 1) (FTmp f)])}
                I     -> do {i <- newITemp; plX <- eval e i; pure (Nothing, plX++[Wr (Raw t (ConstI off) Nothing 1) (Tmp i)])}
                Arr{} -> do {r <- newITemp ; (l,pl) <- aeval e r; pure (l, pl++[Wr (Raw t (ConstI off) Nothing 1) (Tmp r)])}) es offs
    pure (offs, Just szE, catMaybes ls, concat ss)
πe (EApp (P tys) (EApp _ (Builtin _ A1) e) i) t | offs <- szT tys, sz <- last offs, szE <- ConstI sz = do
    xR <- newITemp; iR <- newITemp
    (lX, plX) <- aeval e xR; plI <- eval i iR
    pure (offs, Just szE, mempty, plX ++ plI ++ [CpyE (TupM t Nothing) (AElem xR 1 (Tmp iR) lX sz) 1 sz])
πe (Var (P tys) x) t = do
    st <- gets vars
    pure (szT tys, Nothing, undefined, [t := Tmp (getT st x)])
πe (LLet _ b e) t = do
    ss <- llet b
    fourth (ss++) <$> πe e t
πe e _ = error (show e)

fourth f ~(x,y,z,w) = (x,y,z,f w)

qmap f g h k ~(x,y,z,w) = (f x, g y, h z, k w)
