module C.Trans ( writeC ) where

import           A
import           C
import           CF.AL                            (AL (..))
import qualified CF.AL                            as AL
import           Control.Composition              (thread)
import           Control.Monad                    (zipWithM)
import           Control.Monad.Trans.State.Strict (State, gets, modify, runState, state)
import           Data.Bifunctor                   (first, second)
import           Data.Functor                     (($>))
import           Data.Int                         (Int64)
import qualified Data.IntMap                      as IM
import qualified Data.IntSet                      as IS
import           Data.List                        (scanl')
import           Data.Maybe                       (catMaybes)
import           Data.Tuple.Extra                 (second3)
import           Data.Word                        (Word64)
import           GHC.Float                        (castDoubleToWord64)
import           Nm
import           Nm.IntMap
import           Op
import           Sh

data CSt = CSt { tempU       :: !Int
               , arrU        :: !AL
               , assemblerSt :: !Int
               , label       :: !Label
               , vars        :: IM.IntMap Temp -- track vars so that (Var x) can be replaced at the site
               , pvars       :: IM.IntMap BTemp
               , dvars       :: IM.IntMap FTemp
               , d2vars      :: IM.IntMap F2Temp
               , avars       :: IM.IntMap (Maybe AL, Temp)
               , fvars       :: IM.IntMap (Label, [Arg], RT)
               , _aa         :: AsmData
               , mts         :: IM.IntMap Temp
               }

nextI :: CM Int
nextI = state (\(CSt tϵ ar as l v b d d2 a f aas ts) -> (tϵ, CSt (tϵ+1) ar as l v b d d2 a f aas ts))

nextArr :: Temp -> CM AL
nextArr r = state (\(CSt t a@(AL i) as l v b d d2 aϵ f aas ts) -> (a, CSt t (AL$i+1) as l v b d d2 aϵ f aas (AL.insert a r ts)))

nextAA :: CM Int
nextAA = state (\(CSt t ar as l v b d d2 a f aas ts) -> (as, CSt t ar (as+1) l v b d d2 a f aas ts))

neL :: CM Label
neL = state (\(CSt t ar as l v b d d2 a f aas ts) -> (l, CSt t ar as (l+1) v b d d2 a f aas ts))

nBT :: CM BTemp
nBT = BTemp<$>nextI

nI :: CM Temp
nI = ITemp <$> nextI

nF :: CM FTemp
nF = FTemp <$> nextI

newF2Temp :: CM F2Temp
newF2Temp = F2Temp <$> nextI

addAA :: Int -> [Word64] -> CSt -> CSt
addAA i aa (CSt t ar as l v b d d2 a f aas ts) = CSt t ar as l v b d d2 a f (IM.insert i aa aas) ts

addVar :: Nm a -> Temp -> CM ()
addVar n r = modify (\(CSt t ar as l v b d d2 a f aas ts) -> CSt t ar as l (insert n r v) b d d2 a f aas ts)

addD :: Nm a -> FTemp -> CM ()
addD n r = modify (\(CSt t ar as l v b d d2 a f aas ts) -> CSt t ar as l v b (insert n r d) d2 a f aas ts)

bI :: Nm a -> CM Temp
bI n = state (\(CSt t ar as l v b d d2 a f aas ts) -> let r=ITemp t in (r, CSt (t+1) ar as l (insert n r v) b d d2 a f aas ts))

bD :: Nm a -> CM FTemp
bD n = state (\(CSt t ar as l v b d d2 a f aas ts) -> let r=FTemp t in (r, CSt (t+1) ar as l v b (insert n r d) d2 a f aas ts))

bB :: Nm a -> CM BTemp
bB n = state (\(CSt t ar as l v b d d2 a f aas ts) -> let r=BTemp t in (r, CSt (t+1) ar as l v (insert n r b) d d2 a f aas ts))

addD2 :: Nm a -> F2Temp -> CSt -> CSt
addD2 n r (CSt t ar as l v b d d2 a f aas ts) = CSt t ar as l v b d (insert n r d2) a f aas ts

addB :: Nm a -> BTemp -> CM ()
addB n r = modify (\(CSt t ar as l v b d d2 a f aas ts) -> CSt t ar as l v (insert n r b) d d2 a f aas ts)

addAVar :: Nm a -> (Maybe AL, Temp) -> CM ()
addAVar n r = modify (\(CSt t ar as l v b d d2 a f aas ts) -> CSt t ar as l v b d d2 (insert n r a) f aas ts)

addF :: Nm a -> (Label, [Arg], RT) -> CSt -> CSt
addF n f (CSt t ar as l v b d d2 a fs aas ts) = CSt t ar as l v b d d2 a (insert n f fs) aas ts

getT :: IM.IntMap b -> Nm a -> b
getT st n = findWithDefault (error ("Internal error: variable " ++ show n ++ " not assigned to a temp.")) n st

type CM = State CSt

infix 9 +=
(+=) t i = t =: (Tmp t+i)

fop op e0 = EApp F (EApp (F ~> F) (Builtin (F ~> F ~> F) op) e0)
eMinus = fop Minus
eDiv = fop Div

isF, isI, isB, isIF :: T a -> Bool
isF F = True; isF _ = False
isI I = True; isI _ = False
isB B = True; isB _ = False
isArr Arr{}=True; isArr _=False
isIF I=True; isIF F=True; isIF _=False
isR B=True; isR t=isIF t
nind I=True; nind F=True; nind P{}=True; nind B{}=True; nind _=False
isΠR (P ts)=all isR ts; isΠR _=False
isΠ P{}=True; isΠ _=False

rel :: Builtin -> Maybe IRel
rel Eq=Just IEq; rel Neq=Just INeq; rel Lt=Just ILt; rel Gt=Just IGt; rel Lte=Just ILeq; rel Gte=Just IGeq; rel _=Nothing

mAA :: T a -> Maybe ((T a, Int64), (T a, Int64))
mAA (Arrow t0 t1) = (,) <$> tRnk t0 <*> tRnk t1
mAA _             = Nothing

f1 :: T a -> Bool
f1 (Arr (_ `Cons` Nil) F) = True; f1 _ = False

bT :: Integral b => T a -> b
bT (P ts)=sum (bT<$>ts); bT F=8; bT I=8; bT B=1; bT Arr{}=8

rSz, nSz :: Integral b => T a -> Maybe b
rSz F=Just 8; rSz I=Just 8; rSz B=Just 1; rSz _=Nothing
nSz F=Just 8; nSz I=Just 8; nSz B=Just 1; nSz (P ts)=sum<$>traverse nSz ts; nSz _=Nothing

aB :: Integral b => T a -> Maybe b
aB (Arr (_ `Cons` Nil) t) = nSz t; aB _ = Nothing
aRr (Arr (_ `Cons` Nil) t) = rr t; aRr _ = Nothing
aN (Arr _ t) = nt t; aN _=Nothing

nt :: T a -> Maybe (T a)
nt I=Just I; nt F=Just F; nt B=Just B; nt t@P{} = Just t; nt _=Nothing

rr :: Integral b => T a -> Maybe (T a, b)
rr I=Just (I,8); rr F=Just (F,8); rr B=Just (B,1); rr _=Nothing

szT = scanl' (\off ty -> off+bT ty::Int64) 0

staRnk :: Integral b => Sh a -> Maybe b
staRnk Nil           = Just 0
staRnk (_ `Cons` sh) = (1+) <$> staRnk sh
staRnk _             = Nothing

eRnk :: Sh a -> (Temp, Maybe AL) -> CE
eRnk sh (xR, lX) | Just i <- staRnk sh = ConstI i
                 | otherwise = EAt (ARnk xR lX)

ev, ec :: T a -> (Temp, Maybe AL) -> CE
ev (Arr (Ix _ i `Cons` _) _) _ = ConstI$fromIntegral i; ev _ (xR, lX) = EAt (ADim xR 0 lX)
ec (Arr (_ `Cons` Ix _ j `Cons` _) _) _ = ConstI$fromIntegral j; ec _ (xR, lX) = EAt (ADim xR 1 lX)

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

ipe :: I a -> Bool
ipe (Ix _ i)          = i > 0 && even i
ipe (StaPlus _ i0 i1) = ipe i0&&ipe i1||ipo i0&&ipo i1
ipe (StaMul _ i0 i1)  = ipe i0 || ipe i1
ipe _                 = False

ipo :: I a -> Bool
ipo (Ix _ i)          = odd i
ipo (StaPlus _ i0 i1) = ipe i0&&ipo i1||ipo i0&&ipe i1
ipo (StaMul _ i0 i1)  = ipo i0 && ipo i1
ipo _                 = False

nzSh :: Sh a -> Bool
nzSh (i `Cons` Nil) = nz i
nzSh (i `Cons` sh)  = nz i && nzSh sh
nzSh _              = False

ni1 (Ix _ i) | i > 1 = True
ni1 (StaPlus _ i0 i1) = ni1 i0 || ni1 i1
ni1 (StaMul _ i0 i1) = (nz i0&&ni1 i1) || (nz i1&&ni1 i0)
ni1 _ = False

ne, n1 :: T a -> Bool
ne (Arr (i `Cons` _) _) = nz i; ne _=False
n1 (Arr (i `Cons` _) _) = ni1 i; n1 _=False
nec (Arr (_ `Cons` i `Cons` _) _) = nz i; nec _=False

te, to, nee :: T a -> Bool
te (Arr (i `Cons` _) _) = ipe i; te _ = False
to (Arr (i `Cons` _) _) = ipo i; to _ = False
nee (Arr sh _) = nzSh sh; nee _=False

rof t = if ne t then Rof1 () else Rof ()
for t = if ne t then For1 () else For (); for1 t = if n1 t then For1 () else For ()
forc t = if nec t then For1 () else For ()
fors t = if nee t then For1 () else For ()

f21o (Arr (Ix _ i `Cons` Nil) _) | odd i = \tϵ el c eu ss _ -> F2orE () tϵ el c eu ss
                                 | even i = F2orO ()
f21o _                           = F2or ()


f2or ty | to ty = F2orO ()
        | te ty = \tϵ el c eu ss _ -> F2orE () tϵ el c eu ss
        | otherwise = F2or ()

staR :: Sh a -> [Int64]
staR Nil = []; staR (Ix _ i `Cons` s) = fromIntegral i:staR s

tRnd :: T a -> (T a, [Int64])
tRnd (Arr sh t) = (t, staR sh)

mIFs :: [E a] -> Maybe [Word64]
mIFs = fmap concat.traverse mIFϵ where mIFϵ (FLit _ d)=Just [castDoubleToWord64 d]; mIFϵ (ILit _ n)=Just [fromIntegral n]; mIFϵ (Tup _ xs)=mIFs xs; mIFϵ _=Nothing

writeC :: E (T ()) -> ([CS ()], LSt, AsmData, IM.IntMap Temp)
writeC = π.flip runState (CSt 0 (AL 0) 0 0 IM.empty IM.empty IM.empty IM.empty IM.empty IM.empty IM.empty IM.empty) . writeCM . fmap rLi where π (s, CSt t _ _ l _ _ _ _ _ _ aa a) = (s, LSt l t, aa, a)

writeCM :: E (T ()) -> CM [CS ()]
writeCM eϵ = do
    cs <- traverse (\_ -> nI) [(0::Int)..5]; fs <- traverse (\_ -> nF) [(0::Int)..5]
    (zipWith (\xr xr' -> MX () xr' (FTmp xr)) [F0,F1,F2,F3,F4,F5] fs ++) . (zipWith (\r r' -> r' =: Tmp r) [C0,C1,C2,C3,C4,C5] cs ++) <$> go eϵ fs cs where
    go (Lam _ x@(Nm _ _ F) e) (fr:frs) rs = addD x fr *> go e frs rs
    go (Lam _ x@(Nm _ _ B) e) frs (r:rs) = addB x (bt r) *> go e frs rs where bt (ITemp i)=BTemp i
    go (Lam _ (Nm _ _ F) _) [] _ = error "Not enough floating-point registers!"
    go (Lam _ x@(Nm _ _ I) e) frs (r:rs) = addVar x r *> go e frs rs
    go (Lam _ x@(Nm _ _ Arr{}) e) frs (r:rs) = addAVar x (Nothing, r) *> go e frs rs
    go Lam{} _ [] = error "Not enough registers!"
    go e _ _ | isF (eAnn e) = do {f <- nF ; (++[MX () FRet0 (FTmp f)]) <$> feval e f} -- avoid clash with xmm0 (arg + ret)
             | isI (eAnn e) = do {t <- nI; (++[CRet =: Tmp t]) <$> eval e t} -- avoid clash when calling functions
             | isB (eAnn e) = do {t <- nBT; (++[MB () CBRet (Is t)]) <$> peval e t}
             | isArr (eAnn e) = do {i <- nI; (l,r) <- aeval e i; pure$r++[CRet =: Tmp i]++case l of {Just m -> [RA () m]; Nothing -> []}}
             | P [F,F] <- eAnn e = do {t <- nI; (_,_,_,p) <- πe e t; pure$sac t 16:p++[MX () FRet0 (FAt (Raw t 0 Nothing 8)), MX () FRet1 (FAt (Raw t 1 Nothing 8)), popc 16]}
             | ty@P{} <- eAnn e, b64 <- bT ty, (n,0) <- b64 `quotRem` 8 = let b=ConstI b64 in do {t <- nI; a <- nextArr CRet; (_,_,ls,pl) <- πe e t; pure (sac t b64:pl++MaΠ () a CRet b64:CpyE () (TupM CRet (Just a)) (TupM t Nothing) (ConstI n) 8:popc b64:RA () a:(RA ()<$>ls))}

rtemp :: T a -> CM RT
rtemp F=FT<$>nF; rtemp I=IT<$>nI; rtemp B=PT<$>nBT

bS, fS :: Builtin -> Bool
fS Times = True; fS Plus = True
fS Max = True; fS Min = True
fS _ = False

bS Times = True
bS Plus  = True
bS Minus = True
bS Div   = True
bS Neg   = True
bS Max   = True
bS Min   = True
bS Sqrt  = True
bS _     = False

hasS :: E a -> Bool
hasS (Builtin _ b)  = bS b
hasS (EApp _ e0 e1) = hasS e0&&hasS e1
hasS (Lam _ _ e)    = hasS e
hasS Var{}          = True
hasS FLit{}         = True
hasS Cond{}         = False

write2 :: E (T ()) -> [F2Temp] -> F2Temp -> CM [CS ()]
write2 (Lam _ x e) (v:vs) vret = do
    modify (addD2 x v)
    write2 e vs vret
write2 e [] r = f2eval e r

writeF :: E (T ())
       -> [Arg]
       -> RT
       -> CM (Maybe AL, [CS ()])
writeF (Lam _ x e) (AA r l:rs) ret = addAVar x (l,r) *> writeF e rs ret
writeF (Lam _ x e) (IPA r:rs) ret = addVar x r *> writeF e rs ret
writeF (Lam _ x e) (FA fr:rs) ret = addD x fr *> writeF e rs ret
writeF (Lam _ x e) (BA r:rs) ret = addB x r *> writeF e rs ret
writeF e [] (IT r) | isArr (eAnn e) = aeval e r
writeF e [] (IT r) | isI (eAnn e) = (Nothing,)<$>eval e r
writeF e [] (IT r) | isΠR (eAnn e) = (\ ~(_,_,_,ss) -> (Nothing, ss))<$>πe e r
writeF e [] (FT r) = (Nothing,)<$>feval e r
writeF e [] (PT r) = (Nothing,)<$>peval e r

m'p :: Maybe (CS (), CS ()) -> [CS ()] -> [CS ()]
m'p Nothing       = id
m'p (Just (a,pϵ)) = (++[pϵ]).(a:)

sas :: [Maybe (CS (), CS ())] -> [CS ()] -> [CS ()]
sas = thread.fmap m'p

aSD :: E (T ()) -> [(T (), ArrAcc, Temp)] -> T () -> ArrAcc -> Temp -> CM ([CS ()], [Maybe (CS (), CS ())])
aSD f as rT rAt td = do
    (args, rArgs, pinchArgs) <- unzip3 <$> traverse (\(t,r,xd) -> second3 (:[xd=:(Tmp xd+ConstI (bT t))]) <$> arg t r) as
    (r, wR, pinch) <- rW rT rAt
    ss <- writeRF f args r
    pure (concat rArgs++ss++[wR, td=:(Tmp td+ConstI (bT rT))], pinch:pinchArgs)

aS :: E (T ()) -> [(T (), Int64 -> ArrAcc)] -> T () -> (Int64 -> ArrAcc) -> CM ([CS ()], [Maybe (CS (), CS ())])
aS f as rT rAt = do
    (args, rArgs, pinchArgs) <- unzip3 <$> traverse (\(t,r) -> arg t (r$bT t)) as
    (r, wR, pinch) <- rW rT (rAt$bT rT)
    ss <- writeRF f args r
    pure (rArgs++ss++[wR], pinch:pinchArgs)

arg :: T () -> ArrAcc -> CM (RT, CS (), Maybe (CS (), CS ()))
arg ty at | isR ty = do
    t <- rtemp ty
    pure (t, mt at t, Nothing)
arg ty at | isΠ ty = do
    slop <- nI
    pure $ let sz=bT ty in (IT slop, CpyE () (TupM slop Nothing) at 1 sz, Just (sac slop sz, popc sz))

rW :: T () -> ArrAcc -> CM (RT, CS (), Maybe (CS (), CS ()))
rW ty at | isR ty = do
    t <- rtemp ty
    pure (t, wt at t, Nothing)
rW ty at | isΠ ty = do
    slopO <- nI
    pure $ let sz=bT ty in (IT slopO, CpyE () at (TupM slopO Nothing) 1 sz, Just (sac slopO sz, popc sz))

writeRF :: E (T ()) -> [RT] -> RT -> CM [CS ()]
writeRF e args = fmap snd.writeF e (ra<$>args)

data Arg = IPA !Temp | FA !FTemp | AA !Temp (Maybe AL) | BA !BTemp
data RT = IT !Temp | FT !FTemp | PT !BTemp

mt :: ArrAcc -> RT -> CS ()
mt p (IT t) = t =: EAt p
mt p (FT t) = MX () t (FAt p)
mt p (PT t) = MB () t (PAt p)

wt :: ArrAcc -> RT -> CS ()
wt p (IT t) = Wr () p (Tmp t)
wt p (FT t) = WrF () p (FTmp t)
wt p (PT t) = WrP () p (Is t)

ra (FT f)=FA f; ra (IT r)=IPA r; ra (PT r)=BA r
art (IPA r)=IT r;art (FA r)=FT r; art (BA r)=PT r

eeval :: E (T ()) -> RT -> CM [CS ()]
eeval e (IT t) = eval e t
eeval e (FT t) = feval e t
eeval e (PT t) = peval e t

data RI a b = Cell a | Index b

part :: [RI a b] -> ([a], [b])
part []           = ([], [])
part (Cell i:is)  = first (i:) $ part is
part (Index i:is) = second (i:) $ part is

diml :: (Temp, Maybe AL) -> [CE] -> [CS ()]
diml (t,l) ds = zipWith (\d i -> Wr () (ADim t (ConstI i) l) d) ds [0..]

vSz :: Temp -> CE -> Int64 -> CM (AL, [CS ()])
vSz t n sz = do {a <- nextArr t; pure (a, [Ma () a t 1 n sz, Wr () (ADim t 0 (Just a)) n])}

v8 :: Temp -> CE -> CM (AL, [CS ()])
v8 t n = vSz t n 8

plDim :: Int64 -> (Temp, Maybe AL) -> CM ([Temp], [CS ()])
plDim rnk (a,l) =
    unzip <$> traverse (\at -> do {dt <- nI; pure (dt, dt =: EAt at)}) [ ADim a (ConstI i) l | i <- [0..rnk-1] ]

offByDim :: [Temp] -> CM ([Temp], [CS ()])
offByDim dims = do
    sts <- traverse (\_ -> nI) (undefined:dims)
    let ss=zipWith3 (\s1 s0 d -> s1 =: (Tmp s0*Tmp d)) (tail sts) sts dims
    pure (reverse sts, head sts =: 1:ss)
    -- drop 1 for strides

data Cell a b = Fixed -- set by the larger procedure
              | Bound b -- to be iterated over

forAll is bs = thread (zipWith g is bs) where
    g t b@(ConstI i) | i > 0 = (:[]) . For1 () t 0 ILt b
    g t b            = (:[]) . For () t 0 ILt b

-- the resulting expressions/statement contain free variables that will be iterated over in the main rank-ification loop, these free variables are returned alongside
extrCell :: Int64 -> [Cell () Temp] -> [Temp] -> (Temp, Maybe AL) -> Temp -> CM ([Temp], [CS ()])
extrCell sz fixBounds sstrides (srcP, srcL) dest = do
    (dims, ts, arrIxes, complts) <- switch fixBounds
    i <- nI; t <- nI
    pure (complts, (i =: 0:) $ forAll ts (Tmp<$>dims)
        [t =: EAt (At srcP (Tmp<$>sstrides) (Tmp<$>arrIxes) srcL sz), Wr () (Raw dest (Tmp i) Nothing sz) (Tmp t), i+=1])
    where switch (Bound d:ds) = do {t <- nI; qmap (d:) (t:) (t:) id <$> switch ds}
          switch (Fixed:ds)   = do {f <- nI; qmap id id (f:) (f:) <$> switch ds}
          switch []           = pure ([], [], [], [])

vslop :: Int64 -> Int -> CM (Temp, [CS ()], CS ())
vslop sz n = do
    slopP <- nI
    pure (slopP, [sac slopP szSlop, Wr () (ARnk slopP Nothing) 1, Wr () (ADim slopP 0 Nothing) (fromIntegral n)], popc szSlop)
  where
    szSlop=16+fromIntegral n*sz

plSlop :: Int64 -> Int64 -> [CE] -> CM (Temp, Temp, [CS ()], CS ())
plSlop sz slopRnk dims = do
    slopP <- nI; slopSz <- nI; slopE <- nI
    pure (slopP, slopSz,
            PlProd () slopSz dims
                :slopE=:(Tmp slopSz*ConstI sz+ConstI (8*(slopRnk+1)))
                :sa sz slopP (Tmp slopE):Wr () (ARnk slopP Nothing) (ConstI slopRnk)
                :diml (slopP, Nothing) dims,
         pop sz (Tmp slopE))

codT :: T () -> T ()
codT (Arrow _ t@Arrow{}) = codT t
codT (Arrow _ t)         = t

r00 :: E (T ()) -> Maybe (E (T ()), [E (T ())])
r00 (EApp _ (Builtin _ (Rank is)) f) | all ((==0).fst) is = Just (f, [])
r00 (EApp _ f e) | Arr{} <- eAnn e = second (e:) <$> r00 f
r00 _ = Nothing

llet :: (Nm (T ()), E (T ())) -> CM [CS ()]
llet (n,e') | isArr (eAnn e') = do
    eR <- nI
    (l, ss) <- aeval e' eR
    addAVar n (l,eR) $> ss
llet (n,e') | isI (eAnn e') = do
    eR <- bI n
    eval e' eR
llet (n,e') | isF (eAnn e') = do
    eR <- bD n
    feval e' eR
llet (n,e') | isB (eAnn e') = do
    eR <- bB n
    peval e' eR
llet (n,e') | Arrow tD tC <- eAnn e', isR tD && isR tC = do
    l <- neL
    x <- rtemp tD; y <- rtemp tC
    (_, ss) <- writeF e' [ra x] y
    modify (addF n (l, [ra x], y))
    pure [C.Def () l ss]

aeval :: E (T ()) -> Temp -> CM (Maybe AL, [CS ()])
aeval (LLet _ b e) t = do
    ss <- llet b
    second (ss ++) <$> aeval e t
aeval (Var _ x) t = do
    st <- gets avars
    let (i, r) = {-# SCC "getA" #-} getT st x
    pure (i, [t =: Tmp r])
aeval (EApp ty (EApp _ (Builtin _ A.R) e0) e1) t | (F, ixs) <- tRnd ty = do
    a <- nextArr t
    (plE0,e0e) <- plD e0; (plE1,e1e) <- plD e1
    xR <- nF; scaleR <- nF; k <- nI
    let rnk=fromIntegral(length ixs); n=product ixs
        plRnd = [FRnd () xR, MX () xR (FTmp scaleR*FTmp xR+e0e), WrF () (AElem t rnk (Tmp k) (Just a) 8) (FTmp xR)]
        loop=fors ty k 0 ILt (ConstI n) plRnd
    pure (Just a, plE0 $ plE1 (Ma () a t rnk (ConstI n) 8:diml (t, Just a) (ConstI<$>ixs)++MX () scaleR (e1e-e0e):[loop]))
aeval (EApp ty (EApp _ (Builtin _ A.R) e0) e1) t | (I, ixs) <- tRnd ty = do
    a <- nextArr t
    scaleR <- nI; iR <- nI; k <- nI
    (plE0,e0e) <- plC e0; (plE1,e1e) <- plC e1
    let rnk=fromIntegral$length ixs; n=product ixs
        plRnd = [Rnd () iR, iR =: (Bin IRem (Tmp iR) (Tmp scaleR) + e0e), Wr () (AElem t rnk (Tmp k) (Just a) 8) (Tmp iR)]
        loop=fors ty k 0 ILt (ConstI n) plRnd
    pure (Just a, plE0$plE1$Ma () a t rnk (ConstI n) 8:diml (t, Just a) (ConstI<$>ixs)++scaleR=:(e1e-e0e+1):[loop])
aeval (Builtin ty Eye) t | (I, ixs@[i,_]) <- tRnd ty = do
    a <- nextArr t
    td <- nI; k <- nI
    let rnk=fromIntegral$length ixs; n=product ixs
        loop = fors ty k 0 ILt (ConstI i) [Wr () (At td [ConstI i, 1] [Tmp k, Tmp k] (Just a) 8) (ConstI 1)]
    pure (Just a, Ma () a t rnk (ConstI n) 8:diml (t, Just a) (ConstI<$>ixs)++[td=:DP t rnk, loop])
aeval (Builtin ty Eye) t | (F, ixs@[i,_]) <- tRnd ty = do
    a <- nextArr t
    td <- nI; k <- nI
    let rnk=fromIntegral$length ixs; n=product ixs
        loop = fors ty k 0 ILt (ConstI i) [WrF () (At td [ConstI i, 1] [Tmp k, Tmp k] (Just a) 8) (ConstF 1)]
    pure (Just a, Ma () a t rnk (ConstI n) 8:diml (t, Just a) (ConstI<$>ixs)++[td=:DP t rnk, loop])
aeval (EApp _ (Builtin _ AddDim) x) t | F <- eAnn x = do
    xR <- nF
    plX <- feval x xR
    (a,aV) <- v8 t 1
    pure (Just a, plX++aV++[WrF () (AElem t 1 0 (Just a) 8) (FTmp xR)])
aeval (EApp _ (Builtin _ AddDim) x) t | I <- eAnn x = do
    xR <- nI
    plX <- eval x xR
    (a,aV) <- v8 t 1
    pure (Just a, plX++aV++[Wr () (AElem t 1 0 (Just a) 8) (Tmp xR)])
aeval (EApp _ (Builtin _ AddDim) x) t | P{} <- eAnn x = do
    xR <- nI
    (szs, mS, _, plX) <- πe x xR
    let sz=last szs
    (a,aV) <- vSz t 1 sz
    pure (Just a, m'sa xR mS++plX++aV++[CpyE () (AElem t 1 0 (Just a) sz) (TupM xR Nothing) 1 sz]++m'pop mS)
aeval (EApp _ (Builtin _ AddDim) xs) t | (Arr sh ty) <- eAnn xs, Just sz <- nSz ty = do
    (plX, (lX, xR)) <- plA xs
    xRnk <- nI; szR <- nI; rnk <- nI
    a <- nextArr t
    td <- nI; xRd <- nI
    pure (Just a,
            plX$xRnk=:eRnk sh (xR,lX):SZ () szR xR (Tmp xRnk) lX:rnk =: (Tmp xRnk+1):Ma () a t (Tmp rnk) (Tmp szR) sz:
           [Wr () (ADim t 0 (Just a)) 1, CpyD () (ADim t 1 (Just a)) (ADim xR 0 lX) (Tmp xRnk), td=:DP t (Tmp rnk), xRd=:DP xR (Tmp xRnk), CpyE () (Raw td 0 (Just a) sz) (Raw xRd 0 lX sz) (Tmp szR) sz])
aeval (EApp oTy (Builtin _ Init) x) t | Just sz <- aB oTy = do
    nR <- nI
    (a,aV) <- vSz t (Tmp nR) sz
    (plX, (lX, xR)) <- plA x
    pure (Just a, plX$nR =: (ev (eAnn x) (xR,lX)-1):aV++[CpyE () (AElem t 1 0 (Just a) sz) (AElem xR 1 0 lX sz) (Tmp nR) sz])
aeval (EApp oTy (Builtin _ InitM) x) t | Just sz <- aB oTy = do
    nR <- nI
    (a,aV) <- vSz t (Bin IMax (Tmp nR) 0) sz
    (plX, (lX, xR)) <- plA x
    pure (Just a,
        plX$
        nR =: (ev (eAnn x) (xR,lX)-1)
        :aV++[CpyE () (AElem t 1 0 (Just a) sz) (AElem xR 1 0 lX sz) (Tmp nR) sz])
aeval (EApp oTy (Builtin _ Tail) x) t | Just sz <- aB oTy = do
    nR <- nI
    (a,aV) <- vSz t (Tmp nR) sz
    (plX, (lX, xR)) <- plA x
    pure (Just a, plX$nR =: (ev (eAnn x) (xR,lX)-1):aV++[CpyE () (AElem t 1 0 (Just a) sz) (AElem xR 1 1 lX sz) (Tmp nR) sz])
aeval (EApp _ (Builtin _ Head) xs) t | Just (tX, xRnk) <- tRnk (eAnn xs), Just sz <- nSz tX = do
    a <- nextArr t
    (plX, (lX, xR)) <- plA xs
    (dts, plDs) <- plDim xRnk (xR, lX)
    szA <- nI
    pure (Just a, plX$tail plDs++PlProd () szA (Tmp<$>tail dts):Ma () a t 1 (Tmp szA) sz:CpyD () (ADim t 0 (Just a)) (ADim xR 1 lX) (ConstI$xRnk-1):[CpyE () (AElem t 1 0 (Just a) sz) (AElem xR (ConstI xRnk) 0 lX sz) (Tmp szA) sz])
aeval (EApp _ (Builtin _ Last) xs) t | Just (tX, xRnk) <- tRnk (eAnn xs), Just sz <- nSz tX = do
    a <- nextArr t
    (plX, (lX, xR)) <- plA xs
    (dts, plDs) <- plDim xRnk (xR, lX)
    let n=head dts
    szA <- nI
    pure (Just a, plX$plDs++PlProd () szA (Tmp<$>tail dts):Ma () a t 1 (Tmp szA) sz:CpyD () (ADim t 0 (Just a)) (ADim xR 1 lX) (ConstI$xRnk-1):[CpyE () (AElem t 1 0 (Just a) sz) (AElem xR (ConstI xRnk) ((Tmp n-1)*Tmp szA) lX sz) (Tmp szA) sz])
aeval (EApp _ (Builtin _ Tail) xs) t | Just (tX, xRnk) <- tRnk (eAnn xs), Just sz <- nSz tX = do
    a <- nextArr t
    (plX, (lX, xR)) <- plA xs
    (dts, plDs) <- plDim xRnk (xR, lX)
    let n=head dts; rnkE=ConstI xRnk
    szA <- nI; szz <- nI; d1 <- nI
    pure (Just a, plX$plDs++PlProd () szz (Tmp<$>tail dts):d1=:(Tmp n-1):szA=:(Tmp szz*Tmp d1):Ma () a t rnkE (Tmp szA) sz:Wr () (ADim t 0 (Just a)) (Tmp d1):CpyD () (ADim t 1 (Just a)) (ADim xR 1 lX) (ConstI$xRnk-1):[CpyE () (AElem t rnkE 0 (Just a) sz) (AElem xR rnkE (Tmp szz) lX sz) (Tmp szA) sz])
aeval (EApp _ (Builtin _ Init) xs) t | Just (tX, xRnk) <- tRnk (eAnn xs), Just sz <- nSz tX = do
    a <- nextArr t
    (plX, (lX, xR)) <- plA xs
    (dts, plDs) <- plDim xRnk (xR, lX)
    let n=head dts; rnkE=ConstI xRnk
    szA <- nI; d1 <- nI
    pure (Just a, plX$plDs++d1=:(Tmp n-1):PlProd () szA (Tmp<$>d1:tail dts):Ma () a t rnkE (Tmp szA) sz:Wr () (ADim t 0 (Just a)) (Tmp d1):CpyD () (ADim t 1 (Just a)) (ADim xR 1 lX) (ConstI$xRnk-1):[CpyE () (AElem t rnkE 0 (Just a) sz) (AElem xR rnkE 0 lX sz) (Tmp szA) sz])
aeval (EApp _ (Builtin _ Flat) xs) t | (Arr sh ty) <- eAnn xs, Just sz <- nSz ty = do
    (plX, (lX, xR)) <- plA xs
    xRnk <- nI; szR <- nI
    (a,aV) <- vSz t (Tmp szR) sz
    pure (Just a, plX$xRnk=:eRnk sh (xR,lX):SZ () szR xR (Tmp xRnk) lX:aV++[CpyE () (AElem t 1 0 (Just a) sz) (AElem xR (Tmp xRnk) 0 lX sz) (Tmp szR) sz])
aeval (EApp _ (EApp _ (Builtin _ Map) f) e) t | Arrow F F <- eAnn f, tXs <- eAnn e, hasS f = do
    (plE, (l, xR)) <- plA e
    i <- nI; szR <- nI
    (a,aV) <- v8 t (Tmp szR)
    x0 <- nF; y0 <- nF; x <- newF2Temp; y <- newF2Temp
    ss <- write2 f [x] y
    s1 <- writeRF f [FT x0] (FT y0)
    let step=MX2 () x (FAt (AElem xR 1 (Tmp i) l 8)):ss++[Wr2F () (AElem t 1 (Tmp i) (Just a) 8) (FTmp y)]
        step1=MX () x0 (FAt (AElem xR 1 (Tmp i) l 8)):s1++[WrF () (AElem t 1 (Tmp i) (Just a) 8) (FTmp y0)]
        loop=f2or tXs i 0 ILt (Tmp szR) step step1
    pure (Just a, plE$szR=:ev tXs (xR,l):aV++[loop])
aeval (EApp _ (EApp _ (Builtin _ Map) op) e) t | (Arrow tD tC) <- eAnn op, Just sz <- nSz tC, nind tD = do
    (plE, (l, xR)) <- plA e
    iR <- nI; xRd <- nI; td <- nI; szR <- nI
    (a,aV) <- vSz t (Tmp szR) sz
    (step, pinches) <- aSD op [(tD, Raw xRd 0 l undefined, xRd)] tC (Raw td 0 (Just a) undefined) td
    let loop=rof (eAnn e) iR (Tmp szR) step
    pure (Just a, plE$szR=:ev (eAnn e) (xR,l):aV++xRd=:DP xR 1:td=:DP t 1:sas pinches [loop])
aeval (EApp _ (EApp _ (Builtin _ Filt) p) xs) t | tXs@(Arr (_ `Cons` Nil) tX) <- eAnn xs, Just sz <- nSz tX = do
    a <- nextArr t
    szR <- nI; nR <- nI; b <- nBT
    (plX, (lX, xsR)) <- plA xs
    k <- nI
    (xR, rX, pinch) <- arg tX (AElem xsR 1 (Tmp k) lX sz)
    ss <- writeRF p [xR] (PT b)
    let step = rX:ss++[If () (Is b) [w tX (AElem t 1 (Tmp nR) (Just a) sz) xR, nR+=1] []]
        loop = for (eAnn xs) k 0 ILt (Tmp szR) step
    pure (Just a,
        plX$
        szR =: ev tXs (xsR,lX)
        :Ma () a t 1 (Tmp szR) sz
        :m'p pinch [nR=:0, loop, Wr () (ADim t 0 (Just a)) (Tmp nR)])
  where
    w ty at tt      | isR ty = wt at tt
    w ty at (IT tt) | isΠ ty = CpyE () at (TupM tt Nothing) 1 (bT ty)
aeval (EApp _ (EApp _ (Builtin _ Ices) p) xs) t | tXs@(Arr (_ `Cons` Nil) tX) <- eAnn xs, Just sz <- nSz tX = do
    a <- nextArr t
    szR <- nI; nR <- nI; b <- nBT
    (plX, (lX, xsR)) <- plA xs
    k <- nI
    (xR, rX, pinch) <- arg tX (AElem xsR 1 (Tmp k) lX sz)
    ss <- writeRF p [xR] (PT b)
    let step = rX:ss++[If () (Is b) [Wr () (AElem t 1 (Tmp nR) (Just a) 8) (Tmp k), nR+=1] []]
        loop = for (eAnn xs) k 0 ILt (Tmp szR) step
    pure (Just a,
        plX$
        szR=:ev tXs (xsR,lX)
        :Ma () a t 1 (Tmp szR) 8
        :m'p pinch [nR=:0, loop, Wr () (ADim t 0 (Just a)) (Tmp nR)])
aeval (EApp _ (EApp _ (Builtin _ Map) f) xs) t | (Arrow tD tC) <- eAnn f, Just (_, xRnk) <- tRnk (eAnn xs), Just (ta, rnk) <- tRnk tD, Just szD <- nSz ta, Just sz <- nSz tC = do
    a <- nextArr t
    szR <- nI; xd <- nI; i <- nI; k <- nI
    (plX, (lX, xR)) <- plA xs
    let slopDims=[EAt (ADim xR (ConstI l) lX) | l <- [rnk..(xRnk-1)]]
    (slopP, slopSz, aSlop, pops) <- plSlop szD rnk slopDims
    (y, wRet, pinch) <- rW tC (AElem t 1 (Tmp k) (Just a) sz)
    (_, ss) <- writeF f [AA slopP Nothing] y
    let xDims=[EAt (ADim xR (ConstI l) lX) | l <- [0..(rnk-1)]]
        dimsFromIn=ConstI$xRnk-rnk
        oRnk=xRnk-rnk
        step=CpyE () (AElem slopP (ConstI rnk) 0 Nothing szD) (Raw xd (Tmp i) lX szD) (Tmp slopSz) szD:ss++[wRet, i+=Tmp slopSz]
    pure (Just a,
        plX$
        aSlop
        ++PlProd () szR xDims
        :Ma () a t (ConstI oRnk) (Tmp szR) sz
            :CpyD () (ADim t 0 (Just a)) (ADim xR 0 lX) dimsFromIn
        :xd=:DP xR (ConstI xRnk):i=:0
        :m'p pinch
            (fors (eAnn xs) k 0 ILt (Tmp szR) step:[pops]))
aeval (EApp _ (EApp _ (Builtin _ Map) f) xs) t | (Arrow tD tC) <- eAnn f, Just (_, xRnk) <- tRnk (eAnn xs), Just (ta, rnk) <- tRnk tC, Just szO <- nSz ta, Just dSz <- nSz tD = do
    a <- nextArr t
    y <- nI; y0 <- nI; szX <- nI; szY <- nI
    j <- nI; k <- nI; td <- nI; yd <- nI
    (plX, (lX, xR)) <- plA xs
    (x0, wX0, pinch0) <- arg tD (AElem xR (ConstI xRnk) 0 lX dSz)
    (x, wX, pinch) <- arg tD (AElem xR (ConstI xRnk) (Tmp k) lX dSz)
    (lY0, ss0) <- writeF f [ra x0] (IT y0)
    (lY, ss) <- writeF f [ra x] (IT y)
    let xDims=[EAt (ADim xR (ConstI l) lX) | l <- [0..(xRnk-1)]]
        yDims=[EAt (ADim y0 (ConstI l) lY0) | l <- [0..(rnk-1)]]
        oRnk=xRnk+rnk
        step=wX:ss++[yd=:DP y (ConstI rnk), CpyE () (Raw td (Tmp j) (Just a) szO) (Raw yd 0 lY undefined) (Tmp szY) szO, j+=Tmp szY]
    pure (Just a,
        plX$m'p pinch0 (wX0:ss0)
        ++PlProd () szY yDims
        :PlProd () szX xDims
        :Ma () a t (ConstI oRnk) (Tmp szX*Tmp szY) szO
            :CpyD () (ADim t 0 (Just a)) (ADim xR 0 lX) (ConstI xRnk)
            :CpyD () (ADim t (ConstI xRnk) (Just a)) (ADim y0 0 lY0) (ConstI rnk)
        :td=:DP t (ConstI$xRnk+rnk)
        :j=:0:m'p pinch [fors (eAnn xs) k 0 ILt (Tmp szX) step])
aeval (EApp _ (EApp _ (Builtin _ Map) f) xs) t | tX <- eAnn xs, Just (_, xRnk) <- tRnk tX, Just ((ta0, rnk0), (ta1, rnk1)) <- mAA (eAnn f), Just sz0 <- nSz ta0, Just sz1 <- nSz ta1 = do
    a <- nextArr t
    y <- nI; y0 <- nI
    szR <- nI; szY <- nI
    i <- nI; j <- nI; k <- nI; kL <- nI; xd <- nI; td <- nI
    (plX, (lX, xR)) <- plA xs
    let slopDims=[EAt (ADim xR (ConstI l) lX) | l <- [rnk0..(xRnk-1)]]
    (slopP, slopSz, aSlop, pops) <- plSlop sz1 rnk0 slopDims
    (lY0, ss0) <- writeF f [AA slopP Nothing] (IT y0)
    (lY, ss) <- writeF f [AA slopP Nothing] (IT y)
    let xDims=[EAt (ADim xR (ConstI l) lX) | l <- [0..(rnk0-1)]]
        yDims=[EAt (ADim y0 (ConstI l) lY0) | l <- [0..(rnk1-1)]]
        dimsFromIn=ConstI$xRnk-rnk0
        oRnk=xRnk-rnk0+rnk1
        step=CpyE () (AElem slopP (ConstI rnk0) 0 Nothing sz0) (Raw xd (Tmp i) lX sz0) (Tmp slopSz) sz0:ss++[CpyE () (Raw td (Tmp j) (Just a) sz1) (AElem y (ConstI rnk1) 0 lY sz1) (Tmp szY) sz1, i+=Tmp slopSz, j+=Tmp szY]
    pure (Just a,
        plX$aSlop
        ++xd=:DP xR (ConstI xRnk)
        :CpyE () (AElem slopP (ConstI rnk0) 0 Nothing sz0) (Raw xd 0 lX sz0) (Tmp slopSz) sz0
        :ss0
        ++PlProd () szR (xDims++yDims)
        :Ma () a t (ConstI oRnk) (Tmp szR) sz1
            :CpyD () (ADim t 0 (Just a)) (ADim xR 0 lX) dimsFromIn
            :CpyD () (ADim t dimsFromIn (Just a)) (ADim y0 0 lY0) (ConstI rnk1)
        :td=:DP t (ConstI oRnk)
        :PlProd () szY yDims
        :PlProd () kL xDims:i =: 0:j =: 0
            :fors tX k 0 ILt (Tmp kL) step
        :[pops])
aeval e t | Just (f, xss) <- r00 e, Just xsTys <- traverse (aN.eAnn) xss, tXs@(Arr sh _) <- eAnn (head xss), tC <- codT (eAnn f), Just szC <- nSz tC = do
    a <- nextArr t
    xRds <- traverse (\_ -> nI) xss; tD <- nI
    rnkR <- nI; szR <- nI; i <- nI
    (plXs, (lXs, xRs)) <- second unzip.unzip <$> traverse plA xss
    let xR=head xRs; lX=head lXs
    (step, pinches) <- aS f (reverse$zipWith3 (\tXϵ xRd lXϵ -> (tXϵ, Raw xRd (Tmp i) lXϵ)) xsTys xRds lXs) tC (Raw tD (Tmp i) (Just a))
    let loop=for tXs i 0 ILt (Tmp szR) step
    pure (Just a, thread plXs$rnkR=:eRnk sh (xR,lX):SZ () szR xR (Tmp rnkR) lX:Ma () a t (Tmp rnkR) (Tmp szR) szC:CpyD () (ADim t 0 (Just a)) (ADim xR 0 lX) (Tmp rnkR):zipWith (\xRϵ xRd -> xRd=:DP xRϵ (Tmp rnkR)) xRs xRds++tD=:DP t (Tmp rnkR):sas pinches [loop])
aeval (EApp _ (EApp _ (EApp _ (Builtin _ (Rank [(0, _), (cr, Just ixs)])) op) xs) ys) t | Just (yT, yRnk) <- tRnk (eAnn ys)
                                                                                        , Just (_, xRnk) <- tRnk (eAnn xs)
                                                                                        , Arrow tX (Arrow _ tCod) <- eAnn op
                                                                                        , Just xSz <- nSz tX
                                                                                        , Just (tC, cSz) <- rr tCod
                                                                                        , Just ySz <- nSz yT
                                                                                        = do
    a <- nextArr t
    (plX, (lX, xR)) <- plA xs; (plY, (lY, yR)) <- plA ys
    zR <- rtemp tC
    let ixsIs=IS.fromList ixs; allIx=[ if ix `IS.member` ixsIs then Index() else Cell() | ix <- [1..fromIntegral yRnk] ]
    (dts, dss) <- plDim yRnk (yR, lY)
    (sts, sssϵ) <- offByDim (reverse dts)
    let _:sstrides = sts; sss=init sssϵ
        allDims = zipWith (\ixϵ dt -> case ixϵ of {Cell{} -> Cell dt; Index{} -> Index dt}) allIx dts
        ~(oDims, complDims) = part allDims
        slopRnk=fromIntegral cr::Int64; oRnk=yRnk-slopRnk
    (slopP, _, aSlop, pops) <- plSlop ySz slopRnk (Tmp<$>complDims)
    ix <- nI
    (x, pAX, pinch) <- arg tX (AElem xR (ConstI xRnk) (Tmp ix) lX xSz)
    (_, ss) <- writeF op [ra x, AA slopP Nothing] zR
    let ecArg = zipWith (\d tt -> case (d,tt) of (dϵ,Index{}) -> Bound dϵ; (_,Cell{}) -> Fixed) dts allIx
    oSz <- nI; yRd <- nI; slopPd <- nI
    (complts, place) <- extrCell ySz ecArg sstrides (yRd, lY) slopPd
    let loop=forAll complts (Tmp<$>oDims) $ pAX:place ++ ss ++ [wt (AElem t (ConstI oRnk) (Tmp ix) (Just a) cSz) zR, ix+=1]
    pure (Just a,
        plX$
        plY$
        dss++
        aSlop
        ++[tϵ=:0 | tϵ <- complts]
        ++mt (AElem xR (ConstI xRnk) 0 lX xSz) x
        :sss
        ++yRd=:DP yR (ConstI yRnk):slopPd=:DP slopP (ConstI slopRnk)
        :PlProd () oSz (Tmp<$>oDims)
            :Ma () a t (ConstI oRnk) (Tmp oSz) cSz
            :diml (t, Just a) (Tmp<$>oDims)
        ++ix=:0:m'p pinch loop
        ++[pops])
aeval (EApp _ (EApp _ (EApp _ (Builtin _ (Rank [(0, _), (cr, Just ixs)])) op) xs) ys) t | Just (yT, yRnk) <- tRnk (eAnn ys)
                                                                                        , Just (_, xRnk) <- tRnk (eAnn xs)
                                                                                        , (Arrow tX (Arrow _ tCod)) <- eAnn op
                                                                                        , Just (tC, opRnk) <- tRnk tCod
                                                                                        , Just xSz <- nSz tX
                                                                                        , Just cSz <- rSz tC
                                                                                        , Just ySz <- nSz yT = do
    a <- nextArr t
    zR <- nI
    (plX, (lX, xR)) <- plA xs; (plY, (lY, yR)) <- plA ys
    let ixsIs = IS.fromList ixs; allIx = [ if ix `IS.member` ixsIs then Index() else Cell() | ix <- [1..fromIntegral yRnk] ]
    oSz <- nI; zSz <- nI
    ix <- nI; it <- nI
    (dts, dss) <- plDim yRnk (yR, lY)
    (sts, sssϵ) <- offByDim (reverse dts)
    let _:sstrides = sts; sss=init sssϵ
        allDims = zipWith (\ixϵ dt -> case ixϵ of {Cell{} -> Cell dt; Index{} -> Index dt}) allIx dts
        ~(oDims, complDims) = part allDims
        slopRnk=fromIntegral cr::Int64; oRnk=yRnk+opRnk-slopRnk
    (slopP, _, aSlop, pops) <- plSlop xSz slopRnk (Tmp<$>complDims)
    (x, pAX, pinch) <- arg tX (AElem xR (ConstI xRnk) (Tmp ix) lX xSz)
    (lZ, ss) <- writeF op [ra x, AA slopP Nothing] (IT zR)
    let ecArg = zipWith (\d tt -> case (d,tt) of (dϵ,Index{}) -> Bound dϵ; (_,Cell{}) -> Fixed) dts allIx
    yRd <- nI; slopPd <- nI
    (complts, place) <- extrCell ySz ecArg sstrides (yRd, lY) slopPd
    let loop=forAll complts (Tmp<$>oDims) $ pAX:place ++ ss ++ [CpyE () (AElem t (ConstI oRnk) (Tmp it) (Just a) cSz) (AElem zR (ConstI opRnk) 0 lZ undefined) (Tmp zSz) cSz, ix+=1, it+=Tmp zSz]
    (dots, doss) <- plDim opRnk (zR, lZ)
    pure (Just a,
        plX$
        plY$
        dss
        ++aSlop
        ++[tϵ=:0 | tϵ <- complts]
        ++mt (AElem xR (ConstI xRnk) 0 lX xSz) x
        :sss
        ++yRd =: DP yR (ConstI yRnk):slopPd =: DP slopP (ConstI slopRnk)
        :place
        ++ss++doss
        ++PlProd () zSz (Tmp<$>dots)
        :PlProd () oSz (Tmp<$>(zSz:oDims))
            :Ma () a t (ConstI oRnk) (Tmp oSz) cSz
            :diml (t, Just a) (Tmp<$>(oDims++dots))
        ++ix=:0:it=:0:m'p pinch loop++[pops])
aeval (EApp _ (EApp _ (Builtin _ (Rank [(cr, Just ixs)])) f) xs) t | Just (tA, rnk) <- tRnk (eAnn xs)
                                                                    , (Arrow _ tC) <- eAnn f
                                                                    , Just ySz <- nSz tC
                                                                    , Just aSz <- nSz tA = do
    a <- nextArr t
    (plX, (lX, xR)) <- plA xs
    let ixsIs = IS.fromList ixs; allIx = [ if ix `IS.member` ixsIs then Index() else Cell() | ix <- [1..fromIntegral rnk] ]
    oSz <- nI
    di <- nI
    (dts, dss) <- plDim rnk (xR, lX)
    (sts, sssϵ) <- offByDim (reverse dts)
    let _:sstrides = sts; sss=init sssϵ
        allDims = zipWith (\ix dt -> case ix of {Cell{} -> Cell dt; Index{} -> Index dt}) allIx dts
        ~(oDims, complDims) = part allDims
        oRnk=rnk-fromIntegral cr; slopRnk=fromIntegral cr::Int64
    (slopP, _, aSlop, popS) <- plSlop aSz slopRnk (Tmp<$>complDims)
    (y, wY, pinch) <- rW tC (AElem t (ConstI oRnk) (Tmp di) Nothing ySz)
    (_, ss) <- writeF f [AA slopP Nothing] y
    let ecArg = zipWith (\d tt -> case (d,tt) of (dϵ,Index{}) -> Bound dϵ; (_,Cell{}) -> Fixed) dts allIx
    xRd <- nI; slopPd <- nI
    (complts, place) <- extrCell aSz ecArg sstrides (xRd, lX) slopPd
    let loop=forAll complts (Tmp<$>oDims) $ place ++ ss ++ [wY, di+=1]
    pure (Just a,
        plX $ dss
        ++aSlop
        ++PlProd () oSz (Tmp<$>oDims)
            :Ma () a t (ConstI oRnk) (Tmp oSz) ySz
            :diml (t, Just a) (Tmp<$>oDims)
        ++sss
        ++xRd =: DP xR (ConstI rnk):slopPd =: DP slopP (ConstI slopRnk):di =: 0:m'p pinch loop
        ++[popS])
aeval (EApp _ (EApp _ (Builtin _ (Rank [(cr, Just ixs)])) f) xs) t | Just (tA, xRnk) <- tRnk (eAnn xs)
                                                                    , (Arrow _ tCod) <- eAnn f
                                                                    , Just (tC, opRnk) <- tRnk tCod
                                                                    , Just aSz <- nSz tA
                                                                    , Just cSz <- nSz tC = do
    a <- nextArr t
    (plX, (lX, xR)) <- plA xs
    let ixIs = IS.fromList ixs; allIx = [ if ix `IS.member` ixIs then Index() else Cell() | ix <- [1..fromIntegral xRnk] ]
    yR <- nI; ySz <- nI
    (dts,dss) <- plDim xRnk (xR,lX)
    (sts, sssϵ) <- offByDim (reverse dts)
    let _:sstrides = sts; sss=init sssϵ
        allDims = zipWith (\ix dt -> case ix of {Cell{} -> Cell dt; Index{} -> Index dt}) allIx dts
        ~(oDims, complDims) = part allDims
        slopRnk=fromIntegral cr::Int64; slopRnkE=ConstI slopRnk; oRnk=xRnk+opRnk-slopRnk
    (slopP, _, aSlop, popS) <- plSlop aSz slopRnk (Tmp<$>complDims)
    (lY, ss) <- writeF f [AA slopP Nothing] (IT yR)
    let ecArg = zipWith (\d tt -> case (d,tt) of (dϵ,Index{}) -> Bound dϵ; (_,Cell{}) -> Fixed) dts allIx
    xRd <- nI; slopPd <- nI
    oSz <- nI
    (complts, place) <- extrCell aSz ecArg sstrides (xRd, lX) slopPd
    it <- nI
    let loop=forAll complts (Tmp<$>oDims)
                $ place ++ ss ++ [CpyE () (AElem t (ConstI oRnk) (Tmp it) (Just a) cSz) (AElem yR (ConstI opRnk) 0 lY undefined) (Tmp ySz) cSz, it+=Tmp ySz]
    (dots, doss) <- plDim opRnk (yR, lY)
    pure (Just a,
        plX $ dss
        ++aSlop
        ++[tϵ=:0 | tϵ <- complts]
        ++sss
        ++xRd=:DP xR (ConstI xRnk):slopPd=:DP slopP slopRnkE
        :place
        ++ss
        ++doss
        ++PlProd () ySz (Tmp<$>dots)
        :PlProd () oSz (Tmp<$>(ySz:oDims))
            :Ma () a t (ConstI oRnk) (Tmp oSz) cSz
            :diml (t, Just a) (Tmp<$>(oDims++dots))
        ++it=:0:loop++[popS]
        )
aeval (EApp _ (EApp _ (Builtin _ CatE) x) y) t | tX <- eAnn x, Just (ty, 1) <- tRnk tX = do
    xnR <- nI; ynR <- nI; tn <- nI
    let tyN=bT ty
    (a,aV) <- vSz t (Tmp tn) tyN
    (plX, (lX, xR)) <- plA x; (plY, (lY, yR)) <- plA y
    pure (Just a, plX $ plY $ xnR =: ev tX (xR,lX):ynR =: ev (eAnn y) (yR,lY):tn =: (Tmp xnR+Tmp ynR):aV++CpyE () (AElem t 1 0 (Just a) tyN) (AElem xR 1 0 lX tyN) (Tmp xnR) tyN:[CpyE () (AElem t 1 (Tmp xnR) (Just a) tyN) (AElem yR 1 0 lY tyN) (Tmp ynR) tyN])
aeval (EApp ty (EApp _ (EApp _ (Builtin _ IRange) start) end) (ILit _ 1)) t = do
    n <- nI; startR <- nI; endR <- nI
    (a,aV) <- v8 t (Tmp n)
    i <- nI
    pStart <- eval start startR; pEnd <- eval end endR
    let pN=n =: ((Tmp endR - Tmp startR)+1)
        loop=for ty i 0 ILt (Tmp n) [Wr () (AElem t 1 (Tmp i) (Just a) 8) (Tmp startR), startR+=1]
    pure (Just a, pStart++pEnd++pN:aV++[loop])
aeval (EApp ty (EApp _ (EApp _ (Builtin _ IRange) start) end) incr) t = do
    n <- nI; startR <- nI; endR <- nI; incrR <- nI
    (a,aV) <- v8 t (Tmp n)
    i <- nI
    pStart <- eval start startR; pEnd <- eval end endR; pIncr <- eval incr incrR
    let pN=n =: (Bin Op.IDiv (Tmp endR - Tmp startR) (Tmp incrR)+1)
        loop=for ty i 0 ILt (Tmp n) [Wr () (AElem t 1 (Tmp i) (Just a) 8) (Tmp startR), startR+=Tmp incrR]
    pure (Just a, pStart++pEnd++pIncr++pN:aV++[loop])
aeval (EApp ty (EApp _ (EApp _ (Builtin _ FRange) (FLit _ s)) (FLit _ e)) (ILit _ n)) t = do
    i <- nI
    let nE=ConstI$fromIntegral n
    (a,aV) <- v8 t nE
    accR <- nF; incR <- nF
    let loop=for ty i 0 ILt nE [WrF () (AElem t 1 (Tmp i) (Just a) 8) (FTmp accR), MX () accR (FTmp accR+FTmp incR)]
    pure (Just a, aV++MX () accR (ConstF s):MX () incR (ConstF$(e-s)/(realToFrac n-1)):[loop])
aeval (EApp ty (EApp _ (EApp _ (Builtin _ FRange) start) end) steps) t = do
    i <- nI
    startR <- nF; incrR <- nF; n <- nI
    (a,aV) <- v8 t (Tmp n)
    putStart <- feval start startR; putN <- eval steps n
    putIncr <- feval ((end `eMinus` start) `eDiv` (EApp F (Builtin (Arrow I F) ItoF) steps `eMinus` FLit F 1)) incrR
    let loop=for ty i 0 ILt (Tmp n) [WrF () (AElem t 1 (Tmp i) (Just a) 8) (FTmp startR), MX () startR (FTmp startR+FTmp incrR)]
    pure (Just a, putStart++putIncr++putN++aV++[loop])
aeval (EApp res (EApp _ (Builtin _ Cyc) xs) n) t | Just sz <- aB res = do
    i <- nI; nR <- nI; nO <- nI; szR <- nI
    (a,aV) <- vSz t (Tmp nO) sz
    (plX, (lX, xR)) <- plA xs
    plN <- eval n nR
    ix <- nI
    let loop=for res i 0 ILt (Tmp nR) [CpyE () (AElem t 1 (Tmp ix) (Just a) sz) (AElem xR 1 0 lX sz) (Tmp szR) sz, ix+=Tmp szR]
    pure (Just a, plX $ plN ++ szR =: ev (eAnn xs) (xR,lX):nO =: (Tmp szR*Tmp nR):aV++ix =: 0:[loop])
aeval (EApp _ (EApp _ (Builtin _ VMul) (EApp _ (Builtin _ T) a)) x) t | f1 tX = do
    i <- nI; j <- nI; m <- nI; n <- nI; z <- nF
    (aL,aV) <- v8 t (Tmp m)
    (plAA, (lA, aR)) <- plA a; (plX, (lX, xR)) <- plA x
    aRd <- nI; xRd <- nI; td <- nI
    let loop = forc (eAnn a) i 0 ILt (Tmp m)
                [ MX () z 0,
                  for tX j 0 ILt (Tmp n)
                      [ MX () z (FTmp z+FAt (Raw aRd (Tmp m*Tmp j+Tmp i) lA 8)*FAt (Raw xRd (Tmp j) lX 8)) ]
                , WrF () (Raw td (Tmp i) (Just aL) 8) (FTmp z)
                ]
    pure (Just aL,
        plAA$
        plX$
        m=:ec tA (aR,lA)
        :aV
        ++n=:ev tX (xR,lX)
        :aRd=:DP aR 2:xRd=:DP xR 1:td=:DP t 1
        :[loop])
  where
    tA=eAnn a; tX=eAnn x
aeval (EApp _ (EApp _ (Builtin _ VMul) a) x) t | f1 tX = do
    i <- nI; j <- nI; m <- nI; n <- nI
    aRd <- nI; xRd <- nI; td <- nI
    z <- newF2Temp; z0 <- nF; zs <- nF
    (aL,aV) <- v8 t (Tmp m)
    (plAA, (lA, aR)) <- plA a; (plX, (lX, xR)) <- plA x
    z0 <- nF; zs <- nF; z <- newF2Temp
    let loop = for tA i 0 ILt (Tmp m)
                  [ MX () zs 0, MX2 () z (ConstF (0,0)),
                    -- TODO: maybe f2or should index+1, then use lsl #4 for addressing?
                    -- (would have to put 1-step at end...)
                    f2or tX j 0 ILt (Tmp n)
                        [ MX2 () z (FBin FPlus (FTmp z) (FBin FTimes (FAt (AElem aR 2 (Tmp n*Tmp i+Tmp j) lA 8)) (FAt (AElem xR 1 (Tmp j) lX 8)))) ]
                        [ MX () zs (FAt (AElem aR 2 (Tmp n*Tmp i+Tmp j) lA 8)*FAt (AElem xR 1 (Tmp j) lX 8)) ]
                  , Comb () Op.FPlus z0 z
                  -- TODO: don't need zs if even
                  , WrF () (AElem t 1 (Tmp i) (Just aL) 8) (FTmp zs+FTmp z0)
                  ]
    pure (Just aL,
        plAA$
        plX$
        m=:ev tA (aR,lA)
        :aV
        ++n=:ev tX (xR,lX)
        :aRd=:DP aR 2:xRd=:DP xR 1:td=:DP t 1
        :[loop])
  where
    tA=eAnn a; tX=eAnn x
aeval (EApp _ (EApp _ (Builtin _ Mul) (EApp _ (Builtin _ T) a)) b) t | Just (F, _) <- tRnk tA = do
    aL <- nextArr t
    i <- nI; j <- nI; k <- nI; m <- nI; n <- nI; o <- nI; z <- nF
    aRd <- nI; bRd <- nI; td <- nI
    (plAA, (lA, aR)) <- plA a
    (plB, (lB, bR)) <- plA b
    let loop=forc tA i 0 ILt (Tmp m)
                [forc (eAnn b) j 0 ILt (Tmp o)
                    [ MX () z 0, for tA k 0 ILt (Tmp n)
                        [MX () z (FTmp z+FAt (Raw aRd (Tmp k*Tmp m+Tmp i) lA 8)*FAt (Raw bRd (Tmp k*Tmp o+Tmp j) lB 8))]
                    , WrF () (Raw td (Tmp i*Tmp o+Tmp j) (Just aL) 8) (FTmp z)]
                ]
    pure (Just aL,
        plAA$plB$
        m=:ec tA (aR,lA):o=:ec tB (bR,lB)
        :Ma () aL t 2 (Tmp m*Tmp o) 8:diml (t, Just aL) [Tmp m, Tmp o]
        ++n=:ev tA (aR,lA):aRd=:DP aR 2:bRd=:DP bR 2:td=:DP t 2
        :[loop])
  where
    tA=eAnn a; tB=eAnn b
aeval (EApp _ (EApp _ (Builtin _ Mul) a) (EApp _ (Builtin _ T) b)) t | Just (F, _) <- tRnk tA = do
    aL <- nextArr t
    i <- nI; j <- nI; k <- nI; m <- nI; n <- nI; o <- nI
    (plAA, (lA, aR)) <- plA a
    (plB, (lB, bR)) <- plA b
    z0 <- nF; zs <- nF; z <- newF2Temp
    let loop=for tA i 0 ILt (Tmp m)
                [forc tB j 0 ILt (Tmp o)
                    [ MX () zs 0, MX2 () z (ConstF (0,0)),
                        f2or tB k 0 ILt (Tmp n)
                              [MX2 () z (FBin FPlus (FTmp z) (FBin FTimes (FAt (AElem aR 2 (Tmp n*Tmp i+Tmp k) lA 8)) (FAt (AElem bR 2 (Tmp n*Tmp j+Tmp k) lB 8))))]
                              [MX () zs (FTmp zs+FAt (AElem aR 2 (Tmp n*Tmp i+Tmp k) lA 8)*FAt (AElem bR 2 (Tmp n*Tmp j+Tmp k) lB 8))]
                    , Comb () Op.FPlus z0 z
                    , WrF () (AElem t 2 (Tmp i*Tmp o+Tmp j) (Just aL) 8) (FTmp zs+FTmp z0)]
                    ]
    pure (Just aL,
        plAA$plB$
        m=:ev tA (aR,lA):o=:ev tB (bR,lB)
        :Ma () aL t 2 (Tmp m*Tmp o) 8:diml (t, Just aL) [Tmp m, Tmp o]
        ++n=:ec tA (aR,lA)
        :[loop])
  where
    tA=eAnn a; tB=eAnn b
aeval (EApp _ (EApp _ (Builtin _ Mul) a) b) t | Just (F, _) <- tRnk tA = do
    aL <- nextArr t
    i <- nI; j <- nI; k <- nI; m <- nI; n <- nI; o <- nI; z <- nF
    aRd <- nI; bRd <- nI; td <- nI
    (plAA, (lA, aR)) <- plA a
    (plB, (lB, bR)) <- plA b
    let loop=for tA i 0 ILt (Tmp m)
                [forc tB j 0 ILt (Tmp o)
                    [ MX () z 0, for tB k 0 ILt (Tmp n)
                          [MX () z (FTmp z+FAt (Raw aRd (Tmp n*Tmp i+Tmp k) lA 8)*FAt (Raw bRd (Tmp k*Tmp o+Tmp j) lB 8))]
                    , WrF () (Raw td (Tmp i*Tmp o+Tmp j) (Just aL) 8) (FTmp z)]
                    ]
    pure (Just aL,
        plAA$plB$
        m=:ev tA (aR,lA):o=:ec tB (bR,lB)
        :Ma () aL t 2 (Tmp m*Tmp o) 8:diml (t, Just aL) [Tmp m, Tmp o]
        ++n=:ev tB (bR,lB):aRd=:DP aR 2:bRd=:DP bR 2:td=:DP t 2
        :[loop])
  where
    tA=eAnn a; tB=eAnn b
aeval (EApp _ (EApp _ (Builtin _ ConsE) x) xs) t | tX <- eAnn x, Just sz <- rSz tX = do
    xR <- rtemp tX; nR <- nI; nϵR <- nI
    (a,aV) <- vSz t (Tmp nR) sz
    plX <- eeval x xR
    (plXs, (l, xsR)) <- plA xs
    pure (Just a, plXs$plX++nϵR =: ev (eAnn xs) (xsR,l):nR =: (Tmp nϵR+1):aV++wt (AElem t 1 0 (Just a) sz) xR:[CpyE () (AElem t 1 1 (Just a) sz) (AElem xsR 1 0 l sz) (Tmp nϵR) sz])
aeval (EApp _ (EApp _ (Builtin _ ConsE) x) xs) t | tX <- eAnn x, isΠ tX, sz <- bT tX = do
    xR <- nI; nR <- nI; nϵR <- nI
    (_, mSz, _, plX) <- πe x xR
    (plXs, (lX, xsR)) <- plA xs
    (a,aV) <- vSz t (Tmp nR) sz
    pure (Just a, plXs$m'sa xR mSz++plX++nϵR =: ev (eAnn xs) (xsR,lX):nR =: (Tmp nϵR+1):aV++[CpyE () (AElem t 1 0 (Just a) sz) (TupM xR Nothing) 1 sz, CpyE () (AElem t 1 1 (Just a) sz) (AElem xsR 1 0 lX sz) (Tmp nϵR) sz]++m'pop mSz)
aeval (EApp _ (EApp _ (Builtin _ ConsE) x) xs) t | Just (tX, xRnk) <- tRnk (eAnn x), tXs <- eAnn xs, Just (_, xsRnk) <- tRnk tXs = do
    a <- nextArr t
    (plX, (lX, xR)) <- plA x; (plXs, (lXs, xsR)) <- plA xs
    (dts,dss) <- plDim xRnk (xR, lX)
    d1R <- nI; d1'R <- nI; szR <- nI; nX <- nI
    let rnkE=ConstI xsRnk; szX=bT tX
    pure (Just a, plXs$plX$d1R=:ev tXs (xsR,lXs):dss++d1'R=:(Tmp d1R+1):PlProd () nX (Tmp<$>dts):szR=:(Tmp d1'R*Tmp nX):Ma () a t rnkE (Tmp szR) szX:Wr () (ADim t 0 (Just a)) (Tmp d1'R):CpyD () (ADim t 1 (Just a)) (ADim xsR 1 lXs) (ConstI$xsRnk-1):[CpyE () (AElem t rnkE 0 (Just a) szX) (AElem xR (ConstI xRnk) 0 lX szX) (Tmp nX) szX, CpyE () (AElem t rnkE (Tmp nX) (Just a) szX) (AElem xsR (ConstI xsRnk) 0 lXs szX) (Tmp d1R*Tmp nX) szX])
aeval (EApp _ (EApp _ (Builtin _ Snoc) x) xs) t | tX <- eAnn x, Just sz <- rSz tX = do
    xR <- rtemp tX; nR <- nI; nϵR <- nI
    (a,aV) <- vSz t (Tmp nR) sz
    plX <- eeval x xR
    (plXs, (l, xsR)) <- plA xs
    pure (Just a, plXs$plX++nϵR =: ev (eAnn xs) (xsR,l):nR =: (Tmp nϵR+1):aV++wt (AElem t 1 (Tmp nϵR) (Just a) sz) xR:[CpyE () (AElem t 1 0 (Just a) sz) (AElem xsR 1 0 l sz) (Tmp nϵR) sz])
aeval (EApp _ (EApp _ (Builtin _ Snoc) x) xs) t | tX <- eAnn x, isΠ tX, sz <- bT tX = do
    xR <- nI; nR <- nI; nϵR <- nI
    (_, mSz, _, plX) <- πe x xR
    (plXs, (lX, xsR)) <- plA xs
    (a,aV) <- vSz t (Tmp nR) sz
    pure (Just a, plXs$m'sa xR mSz++plX++nϵR =: ev (eAnn xs) (xsR,lX):nR =: (Tmp nϵR+1):aV++[CpyE () (AElem t 1 (Tmp nϵR) (Just a) sz) (TupM xR Nothing) 1 sz, CpyE () (AElem t 1 0 (Just a) sz) (AElem xsR 1 0 lX sz) (Tmp nϵR) sz]++m'pop mSz)
aeval (EApp _ (EApp _ (Builtin _ Snoc) x) xs) t | Just (tX, xRnk) <- tRnk (eAnn x), tXs <- eAnn xs, Just (_, xsRnk) <- tRnk tXs = do
    a <- nextArr t
    (plX, (lX, xR)) <- plA x; (plXs, (lXs, xsR)) <- plA xs
    (dts,dss) <- plDim xRnk (xR, lX)
    d1R <- nI; d1'R <- nI; szR <- nI; nX <- nI
    let rnkE=ConstI xsRnk; szX=bT tX
    pure (Just a, plXs$plX$d1R=:ev tXs (xsR,lXs):dss++d1'R=:(Tmp d1R+1):PlProd () nX (Tmp<$>dts):szR=:(Tmp d1'R*Tmp nX):Ma () a t rnkE (Tmp szR) szX:Wr () (ADim t 0 (Just a)) (Tmp d1'R):CpyD () (ADim t 1 (Just a)) (ADim xsR 1 lXs) (ConstI$xsRnk-1):[CpyE () (AElem t rnkE (Tmp d1R*Tmp nX) (Just a) szX) (AElem xR (ConstI xRnk) 0 lX szX) (Tmp nX) szX, CpyE () (AElem t rnkE 0 (Just a) szX) (AElem xsR (ConstI xsRnk) 0 lXs szX) (Tmp d1R*Tmp nX) szX])
aeval (EApp ty (EApp _ (Builtin _ Re) n) x) t | tX <- eAnn x, Just xSz <- rSz tX = do
    xR <- rtemp tX; nR <- nI
    (a,aV) <- vSz t (Tmp nR) xSz
    i <- nI
    putN <- eval n nR; putX <- eeval x xR
    let loop=for ty i 0 ILt (Tmp nR) [wt (AElem t 1 (Tmp i) (Just a) xSz) xR]
    pure (Just a, putN++aV++putX++[loop])
aeval (EApp ty (EApp _ (Builtin _ Re) n) x) t | tX <- eAnn x, isΠ tX, sz <- bT tX = do
    xR <- nI; nR <- nI; k <- nI
    plN <- eval n nR
    (a,aV) <- vSz t (Tmp nR) sz
    (_, mSz, _, plX) <- πe x xR
    let loop = for ty k 0 ILt (Tmp nR) [CpyE () (AElem t 1 (Tmp k) (Just a) sz) (TupM xR Nothing) 1 sz]
    pure (Just a, m'sa xR mSz++plX++plN++aV++loop:m'pop mSz)
aeval (EApp ty (EApp _ (Builtin _ Re) n) x) t | (Arr sh tO) <- eAnn x, sz <- bT tO = do
    a <- nextArr t
    nR <- nI; k <- nI
    (plX, (lX, xR)) <- plA x
    plN <- eval n nR
    xRnk <- nI; oRnk <- nI; td <- nI; xRd <- nI; szX <- nI
    let loop = for ty k 0 ILt (Tmp nR) [CpyE () (Raw td (Tmp k*Tmp szX) (Just a) sz) (Raw xRd 0 lX sz) (Tmp szX) sz]
    pure (Just a,
        plX$
        xRnk=:eRnk sh (xR,lX):oRnk=:(Tmp xRnk+1):SZ () szX xR (Tmp xRnk) lX
        :plN
        ++Ma () a t (Tmp oRnk) (Tmp szX*Tmp nR) sz:Wr () (ADim t 0 (Just a)) (Tmp nR):CpyD () (ADim t 1 (Just a)) (ADim xR 0 lX) (Tmp xRnk)
        :td=:DP t (Tmp oRnk)
        :xRd=:DP xR (Tmp xRnk)
        :[loop])
aeval (EApp _ (EApp _ (EApp _ (Builtin _ Zip) op) xs) ys) t | Arrow F (Arrow F F) <- eAnn op, tXs <- eAnn xs, hasS op = do
    nR <- nI; i <- nI
    (a,aV) <- v8 t (Tmp nR)
    (plEX, (lX, xR)) <- plA xs; (plEY, (lY, yR)) <- plA ys
    x0 <- nF; y0 <- nF; z0 <- nF
    x <- newF2Temp; y <- newF2Temp; z <- newF2Temp
    ss <- write2 op [x,y] z
    s1 <- writeRF op (FT<$>[x0,y0]) (FT z0)
    let step=MX2 () x (FAt (AElem xR 1 (Tmp i) lX 8)):MX2 () y (FAt (AElem yR 1 (Tmp i) lY 8)):ss++[Wr2F () (AElem t 1 (Tmp i) (Just a) 8) (FTmp z)]
        step1=MX () x0 (FAt (AElem xR 1 (Tmp i) lX 8)):MX () y0 (FAt (AElem yR 1 (Tmp i) lY 8)):s1++[WrF () (AElem t 1 (Tmp i) (Just a) 8) (FTmp z0)]
        loop=f2or tXs i 0 ILt (Tmp nR) step step1
    pure (Just a, plEX$plEY$nR=:ev tXs (xR,lX):aV++[loop])
aeval (EApp ty (EApp _ (EApp _ (Builtin _ Zip) op) xs) ys) t | (Arrow tX (Arrow tY tC)) <- eAnn op, Just zSz <- nSz tC, nind tX && nind tY = do
    nR <- nI; i <- nI
    (a,aV) <- vSz t (Tmp nR) zSz
    (plEX, (lX, aPX)) <- plA xs; (plEY, (lY, aPY)) <- plA ys
    (step, pinches) <- aS op [(tX, AElem aPX 1 (Tmp i) lX), (tY, AElem aPY 1 (Tmp i) lY)] tC (AElem t 1 (Tmp i) (Just a))
    let loop=for ty i 0 ILt (Tmp nR) step
    pure (Just a, plEX$plEY$nR =: ev (eAnn xs) (aPX,lX):aV++sas pinches [loop])
aeval (EApp _ (EApp _ (EApp _ (Builtin _ ScanS) op) seed) e) t | (Arrow tX (Arrow tY _)) <- eAnn op, Just xSz <- rSz tX, Just ySz <- nSz tY = do
    acc <- rtemp tX; i <- nI; n <- nI
    plS <- eeval seed acc
    (a,aV) <- vSz t (Tmp n) xSz
    (plE, (l, aP)) <- plA e
    (x, wX, pinch) <- arg tY (AElem aP 1 (Tmp i) l ySz)
    ss <- writeRF op [acc, x] acc
    let loopBody=wt (AElem t 1 (Tmp i) (Just a) xSz) acc:wX:ss
        loop=for (eAnn e) i 0 ILt (Tmp n) loopBody
    pure (Just a, plE$plS++n =: (ev (eAnn e) (aP,l)+1):aV++m'p pinch [loop])
aeval (EApp _ (EApp _ (Builtin _ Scan) op) xs) t | (Arrow tAcc (Arrow tX _)) <- eAnn op, Just accSz <- rSz tAcc, Just xSz <- rSz tX = do
    acc <- rtemp tAcc; x <- rtemp tX; i <- nI; n <- nI
    (a,aV) <- vSz t (Tmp n) accSz
    (plE, (l, aP)) <- plA xs
    ss <- writeRF op [acc, x] acc
    let loopBody=wt (AElem t 1 (Tmp i-1) (Just a) accSz) acc:mt (AElem aP 1 (Tmp i) l xSz) x:ss
        loop=for1 (eAnn xs) i 1 ILeq (Tmp n) loopBody
    pure (Just a, plE$n =: ev (eAnn xs) (aP,l):aV++mt (AElem aP 1 0 l xSz) acc:[loop])
    -- TODO: array case
aeval (EApp oTy (EApp _ (Builtin _ (DI n)) op) xs) t | Just (ot, oSz) <- aRr oTy, Just xSz <- aB (eAnn xs) = do
    szR <- nI; sz'R <- nI; i <- nI; fR <- rtemp ot
    (a,aV) <- vSz t (Tmp sz'R) xSz
    (slopP, aSlop, pops) <- vslop xSz n
    (_, ss) <- writeF op [AA slopP Nothing] fR
    (plX, (lX, aP)) <- plA xs
    let sz'=Tmp szR-fromIntegral(n-1)
    let loopBody=CpyE () (AElem slopP 1 0 Nothing xSz) (AElem aP 1 (Tmp i) lX xSz) (fromIntegral n) xSz:ss++[wt (AElem t 1 (Tmp i) (Just a) oSz) fR]
        loop=for oTy i 0 ILt (Tmp sz'R) loopBody
    pure (Just a, plX$szR =: ev (eAnn xs) (aP,lX):sz'R =: sz':aV++aSlop++loop:[pops])
aeval (EApp oTy (EApp _ (Builtin _ (DI n)) op) xs) t | Just ((_, 1), (tO, cRnk)) <- mAA (eAnn op), Just (tX, 1) <- tRnk (eAnn xs) = do
    a <- nextArr t
    d1x <- nI; i <- nI; d1 <- nI
    z0R <- nI; zR <- nI; nC <- nI
    let szX=bT tX; szO=bT tO; oRnk=ConstI$1+cRnk; neϵ=fromIntegral n
    (plX, (lX, xR)) <- plA xs
    (slopP, aSlop, pops) <- vslop szX n
    (lZ0, ss0) <- writeF op [AA slopP Nothing] (IT z0R)
    (lZ, ss) <- writeF op [AA slopP Nothing] (IT zR)
    (dots, plOds) <- plDim cRnk (z0R, lZ0)
    let loopBody = CpyE () (AElem slopP 1 0 Nothing szX) (AElem xR 1 (Tmp i) lX szX) neϵ szX:ss++[CpyE () (AElem t oRnk (Tmp i*Tmp nC) (Just a) szO) (AElem zR (ConstI cRnk) 0 lZ szO) (Tmp nC) szO]
        loop = for oTy i 0 ILt (Tmp d1) loopBody
    pure (Just a,
        plX$
        d1x=:ev (eAnn xs) (xR,lX)
        :d1=:(Tmp d1x-(neϵ-1))
        :aSlop
        ++CpyE () (AElem slopP 1 0 Nothing szX) (AElem xR 1 0 lX szX) neϵ szX:ss0
        ++plOds++PlProd () nC (Tmp<$>dots)
        :Ma () a t oRnk (Tmp d1*Tmp nC) szO
        :zipWith (\j tϵ -> Wr () (ADim t (ConstI j) (Just a)) (Tmp tϵ)) [0..] (d1:dots)
        ++loop
        :[pops])
    -- TODO: array case
aeval (EApp _ (EApp _ (Builtin _ Rot) n) xs) t | tXs <- eAnn xs, Just sz <- aB tXs = do
    c <- nI; szR <- nI
    (plN, nR) <- plEV n
    (plX, (lX, xsR)) <- plA xs
    (a, aV) <- vSz t (Tmp szR) sz
    pure (Just a, plX$plN$szR =: ev tXs (xsR,lX):aV++Ifn't () (IRel IGeq (Tmp nR) 0) [nR+=Tmp szR]:c =: (Tmp szR-Tmp nR):[CpyE () (AElem t 1 0 (Just a) sz) (AElem xsR 1 (Tmp nR) lX sz) (Tmp c) sz, CpyE () (AElem t 1 (Tmp c) (Just a) sz) (AElem xsR 1 0 lX sz) (Tmp nR) sz])
aeval (EApp _ (EApp _ (Builtin _ Rot) n) xs) t | Just (tX, xRnk) <- tRnk (eAnn xs), Just sz <- nSz tX = do
    a <- nextArr t
    c <- nI; szR <- nI
    (plN, nR) <- plEV n
    (plX, (lX, xR)) <- plA xs
    (dts,dss) <- plDim xRnk (xR,lX)
    let d1=head dts; ns=tail dts
        rnkE=ConstI xRnk
    pure (Just a,
        plX$plN$dss
        ++PlProd () szR (Tmp<$>ns)
        :Ma () a t rnkE (Tmp d1*Tmp szR) sz
        :CpyD () (ADim t 0 (Just a)) (ADim xR 0 lX) rnkE
        :Ifn't () (IRel IGeq (Tmp nR) 0) [nR+=Tmp d1]
        :c=:(Tmp d1-Tmp nR)
        :[CpyE () (AElem t rnkE 0 (Just a) sz) (AElem xR rnkE (Tmp nR*Tmp szR) lX sz) (Tmp c*Tmp szR) sz, CpyE () (AElem t rnkE (Tmp c*Tmp szR) (Just a) sz) (AElem xR rnkE 0 lX sz) (Tmp nR*Tmp szR) sz])
aeval (Id _ (AShLit ns es)) t | Just ws <- mIFs es = do
    let rnk=fromIntegral$length ns
    n <- nextAA
    modify (addAA n (rnk:fmap fromIntegral ns++ws))
    pure (Nothing, [t =: LA n])
    -- TODO: boolean lits
aeval (Id (Arr _ at) (AShLit ns es)) t | Just ty <- nt at, sz <- bT ty = do
    let rnk=fromIntegral$length ns; n=fromIntegral$product ns
    a <- nextArr t
    tt <- rtemp ty
    plEs <- zipWithM (\eϵ i -> do {pl <- eeval eϵ tt; pure $ pl ++ [wt (AElem t rnk (ConstI i) (Just a) sz) tt]}) es [0..]
    pure (Just a, Ma () a t rnk n sz:diml (t, Just a) (fromIntegral<$>ns)++concat plEs)
aeval (EApp _ (Builtin _ T) x) t | Just (ty, rnk) <- tRnk (eAnn x) = do
    a <- nextArr t
    let sze=bT ty; dO=ConstI$8+8*rnk
    xd <- nI; td <- nI
    (plX, (l, xR)) <- plA x
    (dts, plDs) <- plDim rnk (xR, l)
    (sts, plSs) <- offByDim (reverse dts)
    (std, plSd) <- offByDim dts
    let n:sstrides = sts; (_:dstrides) = std
    is <- traverse (\_ -> nI) [1..rnk]
    let loop=thread (zipWith (\i tt -> (:[]) . For () i 0 ILt (Tmp tt)) is dts) [CpyE () (At td (Tmp<$>dstrides) (Tmp<$>reverse is) (Just a) sze) (At xd (Tmp<$>sstrides) (Tmp<$>is) l sze) 1 sze]
    pure (Just a, plX$plDs++plSs++Ma () a t (ConstI rnk) (Tmp n) sze:diml (t, Just a) (Tmp<$>reverse dts)++init plSd++xd =: (Tmp xR+dO):td =: (Tmp t+dO):loop)
aeval (EApp _ (EApp _ (EApp _ (Builtin _ Outer) op) xs) ys) t | (Arrow tX (Arrow tY tC)) <- eAnn op, Just zSz <- nSz tC, nind tX && nind tY = do
    a <- nextArr t
    szX <- nI; szY <- nI; i <- nI; j <- nI; k <- nI
    (plX, (lX, xR)) <- plA xs; (plY, (lY, yR)) <- plA ys
    (step, pinches) <- aS op [(tX ,AElem xR 1 (Tmp i) lX), (tY, AElem yR 1 (Tmp j) lY)] tC (AElem t 2 (Tmp k) (Just a))
    let loop=for (eAnn xs) i 0 ILt (Tmp szX) [for (eAnn ys) j 0 ILt (Tmp szY) (step++[k+=1])]
    pure (Just a, plX$plY$szX =: ev (eAnn xs) (xR,lX):szY =: ev (eAnn ys) (yR,lY):Ma () a t 2 (Tmp szX*Tmp szY) zSz:diml (t, Just a) [Tmp szX, Tmp szY]++k=:0:sas pinches [loop])
aeval (EApp _ (EApp _ (EApp _ (Builtin _ Outer) op) xs) ys) t | (Arrow tX (Arrow tY tC)) <- eAnn op, Arr sh tEC <- tC, Just szXT <- nSz tX, Just szYT <- nSz tY, Just szZT <- nSz tEC = do
    a <- nextArr t
    szX <- nI; szY <- nI; szZ <- nI; i <- nI; j <- nI; k <- nI; rnkZ <- nI; rnkO <- nI; z <- nI; z0 <- nI
    (plX, (lX, xR)) <- plA xs; (plY, (lY, yR)) <- plA ys
    (x, wX, pinchX) <- arg tX (AElem xR 1 (Tmp i) lX szXT)
    (y, wY, pinchY) <- arg tY (AElem yR 1 (Tmp j) lY szYT)
    (lZ0, ss0) <- writeF op [ra x, ra y] (IT z0)
    (lZ, ss) <- writeF op [ra x, ra y] (IT z)
    let step=[wX, wY]++ss++[CpyE () (AElem t (Tmp rnkO) (Tmp k*Tmp szZ) (Just a) szZT) (AElem z (Tmp rnkZ) 0 lZ szZT) (Tmp szZ) szZT, k+=1]
        loop=for (eAnn xs) i 0 ILt (Tmp szX) [for (eAnn ys) j 0 ILt (Tmp szY) step]
    pure (Just a,
        plX$
        plY$
        i=:0:j=:0:
        sas [pinchX, pinchY] (
        wX:wY:ss0
        ++rnkZ=:eRnk sh (z0,lZ0)
        :rnkO=:(Tmp rnkZ+2)
        :SZ () szZ z0 (Tmp rnkZ) lZ0
        :szX=:ev (eAnn xs) (xR,lX)
        :szY=:ev (eAnn ys) (yR,lY)
        :Ma () a t (Tmp rnkO) (Tmp szX*Tmp szY*Tmp szZ) szZT
        :diml (t, Just a) [Tmp szX, Tmp szY]
        ++[CpyD () (ADim t 2 (Just a)) (ADim z0 0 lZ0) (Tmp rnkZ), k=:0, loop]
        ))
aeval (EApp ty (EApp _ (Builtin _ Succ) op) xs) t | Arrow tX (Arrow _ tZ) <- eAnn op, Just zSz <- nSz tZ, nind tX = do
    szR <- nI; sz'R <- nI
    (a,aV) <- vSz t (Tmp sz'R) zSz
    (plX, (lX, xR)) <- plA xs
    i <- nI
    (step, pinches) <- aS op [(tX, AElem xR 1 (Tmp i+1) lX), (tX, AElem xR 1 (Tmp i) lX)] tZ (AElem t 1 (Tmp i) (Just a))
    let loop=for ty i 0 ILt (Tmp sz'R) step
    pure (Just a, plX$szR =: ev (eAnn xs) (xR,lX):sz'R =: (Tmp szR-1):aV++sas pinches [loop])
aeval (EApp oTy (Builtin _ RevE) e) t | Just sz <- aB oTy = do
    n <- nI; i <- nI
    (a,aV) <- vSz t (Tmp n) sz
    (plE, (lE, eR)) <- plA e
    let loop=for oTy i 0 ILt (Tmp n) [CpyE () (AElem t 1 (Tmp i) (Just a) sz) (AElem eR 1 (Tmp n-Tmp i-1) lE sz) 1 sz]
    pure (Just a, plE$n =: ev oTy (eR,lE):aV++[loop])
aeval (EApp _ (Builtin _ RevE) e) t | tys <- eAnn e, Just (ty, rnk) <- tRnk tys = do
    a <- nextArr t
    n <- nI; i <- nI; szA <- nI
    (plE, (lE, eR)) <- plA e
    let sz=bT ty; rnkE=ConstI rnk
    (dts, plDs) <- plDim rnk (eR, lE)
    let loop = for ty i 0 ILt (Tmp n) [CpyE () (AElem t rnkE (Tmp i*Tmp szA) (Just a) sz) (AElem eR rnkE ((Tmp n-Tmp i-1)*Tmp szA) lE sz) (Tmp szA) sz]
    pure (Just a, plE$n=:ev ty (eR,lE):tail plDs++PlProd () szA (Tmp<$>tail dts):Ma () a t rnkE (Tmp n*Tmp szA) sz:CpyD () (ADim t 0 (Just a)) (ADim eR 0 lE) rnkE:[loop])
aeval (EApp oTy (EApp _ (EApp _ (Builtin _ Gen) seed) op) n) t | tyS <- eAnn seed, Just sz <- rSz tyS = do
    nR <- nI; plN <- eval n nR; i <- nI
    acc <- rtemp tyS
    plS <- eeval seed acc
    (a,aV) <- vSz t (Tmp nR) sz
    ss <- writeRF op [acc] acc
    let loop=for oTy i 0 ILt (Tmp nR) (wt (AElem t 1 (Tmp i) (Just a) sz) acc:ss)
    pure (Just a, plS++plN++aV++[loop])
aeval (EApp ty (EApp _ (EApp _ (Builtin _ Gen) seed) op) n) t | isΠR (eAnn seed) = do
    nR <- nI; plN <- eval n nR; i <- nI; td <- nI; acc <- nI
    (szs,mP,_,plS) <- πe seed acc
    let πsz=last szs
    (a,aV) <- vSz t (Tmp nR) πsz
    (_, ss) <- writeF op [IPA acc] (IT acc)
    let loop=for ty i 0 ILt (Tmp nR) (CpyE () (Raw td (Tmp i) (Just a) πsz) (TupM acc Nothing) 1 πsz:ss)
    pure (Just a, m'sa acc mP++plS++plN++aV++td=:DP t 1:loop:m'pop mP)
aeval (EApp oTy (EApp _ (Builtin _ (Conv is)) f) x) t
    | (Arrow _ tC) <- eAnn f
    , Just (tX, xRnk) <- tRnk (eAnn x)
    , Just (_, oRnk) <- tRnk oTy
    , Just oSz <- nSz tC, Just xSz <- nSz tX, oRnk==xRnk = do
    a <- nextArr t
    xRd <- nI; szR <- nI; slopP <- nI
    (plX, (lX, xR)) <- plA x
    (dts, plDs) <- plDim xRnk (xR, lX)
    (tdims, dims) <- unzip <$> zipWithM (\dt i -> do {odim <- nI; pure (odim, odim =: (Tmp dt-fromIntegral (i-1)))}) dts is
    io <- traverse (\_ -> nI) tdims
    iw <- traverse (\_ -> nI) is; j <- nI
    let slopSz=product is; slopRnk=length is; slopE=fromIntegral (slopSz*fromIntegral oSz+(slopRnk+1)*8); slopDims=fromIntegral<$>is
        rnk=ConstI oRnk
    z <- rtemp tC; k <- nI; o <- rtemp tX
    (_, ss) <- writeF f [AA slopP Nothing] z
    (sts, plS) <- offByDim (reverse dts)
    let _:strides = sts; sss=init plS
        extrWindow = j=:0:forAll iw (ConstI . fromIntegral<$>is)
                            [mt (At xRd (Tmp<$>strides) (zipWith (\jϵ iϵ -> Tmp jϵ+Tmp iϵ) iw io) lX xSz) o, wt (AElem slopP (ConstI$fromIntegral slopRnk) (Tmp j) Nothing oSz) o, j+=1]
        step = extrWindow++ss++[wt (AElem t rnk (Tmp k) (Just a) oSz) z, k+=1]
        loop=forAll io (Tmp<$>tdims) step
    pure (Just a,
        plX$
        plDs
        ++dims
        ++sss
        ++PlProd () szR (Tmp<$>tdims):Ma () a t rnk (Tmp szR) oSz:diml (t, Just a) (Tmp<$>tdims)
        ++sac slopP slopE:Wr () (ARnk slopP Nothing) (ConstI$fromIntegral slopRnk):diml (slopP, Nothing) slopDims
        ++xRd=:DP xR (ConstI xRnk):k=:0:loop
        ++[popc slopE])
aeval e _ = error (show e)

plC :: E (T ()) -> CM ([CS ()] -> [CS ()], CE)
plC (ILit _ i) = pure (id, ConstI$fromIntegral i)
plC (Var I x)  = do {st <- gets vars; pure (id, Tmp$getT st x)}
plC e          = do {t <- nI; pl <- eval e t; pure ((pl++), Tmp t)}

plD2 :: E (T ()) -> CM ([CS ()] -> [CS ()], F2Temp)
plD2 (Var F x) = do {st <- gets d2vars; pure (id, getT st x)}
plD2 e         = do {t <- newF2Temp; pl <- f2eval e t; pure ((pl++), t)}

plD :: E (T ()) -> CM ([CS ()] -> [CS ()], F1E)
plD (FLit _ x) = pure (id, ConstF x)
plD (Var F x)  = do {st <- gets dvars; pure (id, FTmp$getT st x)}
plD e          = do {t <- nF; pl <- feval e t; pure ((pl++), FTmp t)}

plP :: E (T ()) -> CM ([CS ()] -> [CS ()], PE)
plP (BLit _ b) = pure (id, BConst b)
plP (Var B x)  = do {st <- gets pvars; pure (id, Is$getT st x)}
plP e          = do {t <- nBT; pl <- peval e t; pure ((pl++), Is t)}

plEV :: E (T ()) -> CM ([CS ()] -> [CS ()], Temp)
plEV (Var I x) = do
    st <- gets vars
    pure (id, getT st x)
plEV e = do
    t <- nI
    pl <- eval e t
    pure ((pl++), t)

plF :: E (T ()) -> CM ([CS ()] -> [CS ()], FTemp)
plF (Var F x) = do
    st <- gets dvars
    pure (id, getT st x)
plF e = do
    t <- nF
    pl <- feval e t
    pure ((pl++), t)

plA :: E (T ()) -> CM ([CS ()] -> [CS ()], (Maybe AL, Temp))
plA (Var _ x) = do {st <- gets avars; pure (id, getT st x)}
plA e         = do {t <- nI; (lX,plX) <- aeval e t; pure ((plX++), (lX, t))}

peval :: E (T ()) -> BTemp -> CM [CS ()]
peval (LLet _ b e) t = do
    ss <- llet b
    (ss++) <$> peval e t
peval (BLit _ b) t = pure [MB () t (BConst b)]
peval (EApp _ (EApp _ (Builtin _ A1) e) i) t = do
    (plE, (lE, eR)) <- plA e
    (plI,iE) <- plC i
    pure $ plE $ plI [MB () t (PAt (AElem eR 1 iE lE 8))]
peval (EApp _ (Builtin _ T) e) t = peval e t
peval (EApp _ (Builtin _ Flat) e) t = peval e t
peval (EApp _ (Builtin _ Odd) e0) t = do
    (pl,eR) <- plEV e0
    pure $ pl [Cset () (IUn IOdd (Tmp eR)) t]
peval (EApp _ (Builtin _ Even) e0) t = do
    (pl,eR) <- plEV e0
    pure $ pl [Cset () (IUn IEven (Tmp eR)) t]
peval (EApp _ (EApp _ (Builtin (Arrow I _) op) e0) e1) t | Just iop <- rel op = do
    (plE0,e0e) <- plC e0; (plE1, e1e) <- plC e1
    pure $ plE0 $ plE1 [Cset () (IRel iop e0e e1e) t]
peval (EApp _ (EApp _ (Builtin (Arrow F _) op) e0) e1) t | Just fop' <- frel op = do
    (plE0,e0e) <- plD e0; (plE1, e1e) <- plD e1
    pure $ plE0 $ plE1 [Cset () (FRel fop' e0e e1e) t]
peval (EApp _ (EApp _ (Builtin (Arrow (Arr _ ty) _) Eq) e0) e1) t | Arr sh _ <- eAnn e0, isR ty =do
    (plX0, (lX0, x0R)) <- plA e0; (plX1, (lX1, x1R)) <- plA e1
    rnkR <- nI; szR <- nI
    i <- nI; j <- nI
    x0Rd <- nI; x1Rd <- nI
    let eqDim = Cset () (IRel IEq (EAt (ADim x0R (Tmp i) lX0)) (EAt (ADim x1R (Tmp i) lX1))) t
        eCond = case ty of
            F -> FRel FEq (FAt (Raw x0Rd (Tmp j) lX0 8)) (FAt (Raw x1Rd (Tmp j) lX1 8))
            I -> IRel IEq (EAt (Raw x0Rd (Tmp j) lX0 8)) (EAt (Raw x1Rd (Tmp j) lX1 8))
            B -> Boo BEq (PAt (Raw x0Rd (Tmp j) lX0 1)) (PAt (Raw x1Rd (Tmp j) lX1 1))
    pure $ plX0 $ plX1 $ rnkR=:eRnk sh (x0R,lX0):MB () t (BConst True):i=:0:WT () (Boo AndB (Is t) (IRel ILt (Tmp i) (Tmp rnkR))) [eqDim, i+=1]:SZ () szR x0R (Tmp rnkR) lX0:x0Rd=:DP x0R (Tmp rnkR):x1Rd=:DP x1R (Tmp rnkR):j=:0:[WT () (Boo AndB (Is t) (IRel ILt (Tmp j) (Tmp szR))) [Cset () eCond t, j+=1]]
peval (EApp _ (EApp _ (Builtin _ op) e0) e1) t | Just boo <- mB op = do
    (pl0,e0R) <- plP e0; (pl1,e1R) <- plP e1
    pure $ pl0 $ pl1 [MB () t (Boo boo e0R e1R)]
peval (EApp _ (Builtin _ N) e0) t = do
    (pl,e0R) <- plP e0
    pure $ pl [MB () t (BU BNeg e0R)]
peval (EApp _ (EApp _ (Builtin _ Fold) op) e) acc | (Arrow tX _) <- eAnn op, isB tX = do
    x <- nBT
    i <- nI; szR <- nI
    (plE, (l, aP)) <- plA e
    ss <- writeRF op [PT acc, PT x] (PT acc)
    let loopBody=MB () x (PAt (AElem aP 1 (Tmp i) l 1)):ss
        loop=for1 (eAnn e) i 1 ILt (Tmp szR) loopBody
    pure $ plE$szR =: ev (eAnn e) (aP,l):MB () acc (PAt (AElem aP 1 0 l 1)):[loop]
peval (EApp _ (EApp _ (EApp _ (Builtin _ FoldS) op) seed) e) acc | (Arrow _ (Arrow tY _)) <- eAnn op, Just szY <- nSz tY = do
    i <- nI; szR <- nI
    (plE, (l, aP)) <- plA e
    plAcc <- peval seed acc
    (x, wX, pinch) <- arg tY (AElem aP 1 (Tmp i) l szY)
    ss <- writeRF op [PT acc, x] (PT acc)
    let loopBody=wX:ss
        loop=for (eAnn e) i 0 ILt (Tmp szR) loopBody
    pure $ plE $ plAcc++szR=:ev (eAnn e) (aP,l):m'p pinch [loop]
peval (EApp _ (Builtin _ Head) xs) t = do
    (plX, (l, a)) <- plA xs
    pure $ plX [MB () t (PAt (AElem a 1 0 l 1))]
peval (EApp _ (Builtin _ Last) xs) t = do
    (plX, (l, a)) <- plA xs
    pure $ plX [MB () t (PAt (AElem a 1 (ev (eAnn xs) (a,l)-1) l 1))]
peval (EApp _ (Builtin _ (TAt i)) e) t = do
    k <- nI
    (offs, a, _, plT) <- πe e k
    pure $ m'sa k a++plT ++ MB () t (PAt (Raw k (ConstI$offs!!(i-1)) Nothing 1)):m'pop a
peval e _ = error (show e)

eval :: E (T ()) -> Temp -> CM [CS ()]
eval (LLet _ b e) t = do
    ss <- llet b
    (ss++) <$> eval e t
eval (ILit _ n) t = pure [t =: fromInteger n]
eval (Var _ x) t = do
    st <- gets vars
    pure [t =: Tmp (getT st x)]
eval (EApp _ (EApp _ (Builtin _ A.R) e0) e1) t = do
    (plE0,e0e) <- plC e0; (plE1,e1e) <- plC e1
    pure $ plE0 $ plE1 [Rnd () t, t =: (Bin IRem (Tmp t) (e1e-e0e+1) + e0e)]
eval (EApp _ (EApp _ (Builtin _ Fold) op) e) acc | (Arrow tX _) <- eAnn op, isI tX = do
    x <- nI; szR <- nI
    (plE, (l, aP)) <- plA e
    ss <- writeRF op [IT acc, IT x] (IT acc)
    i <- nI
    let loopBody=x=:EAt (AElem aP 1 (Tmp i) l 8):ss
        loop=for1 (eAnn e) i 1 ILt (Tmp szR) loopBody
    pure $ plE$szR =: ev (eAnn e) (aP,l):acc =: EAt (AElem aP 1 0 l 8):[loop]
eval (EApp _ (EApp _ (EApp _ (Builtin _ FoldS) op) seed) e) acc | (Arrow _ (Arrow tX _)) <- eAnn op, Just xSz <- nSz tX, tArr <- eAnn e = do
    i <- nI; szR <- nI
    (plE, (l, eR)) <- plA e
    plAcc <- eval seed acc
    (x, wX, pinch) <- arg tX (AElem eR 1 (Tmp i) l xSz)
    ss <- writeRF op [IT acc, x] (IT acc)
    let loop=for tArr i 0 ILt (Tmp szR) (wX:ss)
    pure $ plE$plAcc++szR =: ev tArr (eR,l):m'p pinch [loop]
eval (EApp I (EApp _ (Builtin _ op) e0) e1) t | Just cop <- mOp op = do
    (pl0,e0e) <- plC e0; (pl1,e1e) <- plC e1
    pure $ pl0 $ pl1 [t =: Bin cop e0e e1e]
eval (EApp _ (EApp _ (Builtin _ Max) e0) e1) t = do
    (pl0,t0) <- plEV e0
    -- in case t==t1
    t1 <- nI
    pl1 <- eval e1 t1
    pure $ pl0 $ pl1 ++ [t =: Tmp t0, Cmov () (IRel IGt (Tmp t1) (Tmp t0)) t (Tmp t1)]
eval (EApp _ (EApp _ (Builtin _ Min) e0) e1) t = do
    (pl0,t0) <- plEV e0
    -- in case t==t1
    t1 <- nI
    pl1 <- eval e1 t1
    pure $ pl0 $ pl1 ++ [t =: Tmp t0, Cmov () (IRel ILt (Tmp t1) (Tmp t0)) t (Tmp t1)]
eval (EApp _ (EApp _ (Builtin _ A1) e) i) t = do
    (plE, (lE, eR)) <- plA e
    (plI,iE) <- plC i
    pure $ plE $ plI [t =: EAt (AElem eR 1 iE lE 8)]
eval (EApp _ (Builtin _ Head) xs) t = do
    (plX, (l, a)) <- plA xs
    pure $ plX [t =: EAt (AElem a 1 0 l 8)]
eval (EApp _ (Builtin _ Last) xs) t = do
    (plX, (l, a)) <- plA xs
    pure $ plX [t =: EAt (AElem a 1 (ev (eAnn xs) (a,l)-1) l 8)]
eval (EApp _ (Builtin _ Size) xs) t | Just (_, 1) <- tRnk (eAnn xs) = do
    (plE, (l, xsR)) <- plA xs
    pure $ plE [t =: EAt (ADim xsR 0 l)]
eval (EApp _ (Builtin _ Dim) xs) t | Arr (Ix _ i `Cons` _) _ <- eAnn xs = do
    pure [t=:ConstI (fromIntegral i)]
eval (EApp _ (Builtin _ Dim) xs) t = do
    (plE, (l, xsR)) <- plA xs
    pure $ plE [t =: EAt (ADim xsR 0 l)]
eval (EApp _ (Builtin _ Size) xs) t | Arr sh _ <- eAnn xs = do
    (plE, (l, xsR)) <- plA xs
    rnkR <- nI
    pure $ plE [rnkR =: eRnk sh (xsR,l), SZ () t xsR (Tmp rnkR) l]
eval (EApp _ (Builtin _ Size) xs) t | nind (eAnn xs) = pure [t=:1]
eval (EApp _ (EApp _ (Builtin _ IntExp) (FLit _ (-1))) n) t = do
    (plR,nR) <- plEV n
    pure $ plR [t=:1, Cmov () (IUn IOdd (Tmp nR)) t (ConstI (-1))]
eval (EApp _ (EApp _ (Builtin _ IntExp) x) n) t = do
    xR <- nI; nR <- nI
    plX <- eval x xR; plN <- eval n nR
    pure $ plX ++ plN ++ [t=:1, While () nR IGt 0 [Ifn't () (IUn IEven (Tmp nR)) [t=:(Tmp t*Tmp xR)], nR =: Bin IAsr (Tmp nR) 1, MT () xR (Tmp xR*Tmp xR)]]
eval (EApp _ (Builtin _ T) x) t = eval x t
eval (EApp _ (Builtin _ Flat) x) t = eval x t
eval (EApp _ (Builtin _ Floor) x) t = do
    xR <- nF
    plX <- feval x xR
    pure $ plX ++ [t =: CFloor (FTmp xR)]
eval (EApp _ (Builtin _ (TAt i)) e) t = do
    k <- nI
    (offs, a, _, plT) <- πe e k
    pure $ m'sa k a++plT ++ t =: EAt (Raw k (ConstI$offs!!(i-1)) Nothing 1):m'pop a
eval (EApp _ (EApp _ (Builtin _ IOf) p) xs) t | (Arrow tD _) <- eAnn p, Just szX <- nSz tD = do
    pR <- nBT
    szR <- nI; i <- nI; done <- nI
    (plX, (lX, xsR)) <- plA xs
    (x, wX, pinch) <- arg tD (AElem xsR 1 (Tmp i) lX szX)
    ss <- writeRF p [x] (PT pR)
    let loop=While () done INeq 1 (wX:ss++[If () (Is pR) [t=:Tmp i, done=:1] [], i+=1, Cmov () (IRel IGeq (Tmp i) (Tmp szR)) done 1])
    pure $ plX $ szR=:ev (eAnn xs) (xsR,lX):t=:(-1):done=:0:i=:0:m'p pinch [loop]
eval (EApp _ (EApp _ (EApp _ (Builtin _ Iter) f) n) x) t = do
    (plN,nR) <- plC n
    plX <- eval x t
    ss <- writeRF f [IT t] (IT t)
    i <- nI
    let loop=For () i 0 ILt nR ss
    pure $ plX++plN [loop]
eval (Cond _ p e0 e1) t = snd <$> cond p e0 e1 (IT t)
eval (Id _ (FoldOfZip zop op [p])) acc | Just (tP, pSz) <- aRr (eAnn p) = do
    x <- rtemp tP
    i <- nI; szR <- nI
    (plPP, (lP, pR)) <- plA p
    ss <- writeRF op [IT acc, x] (IT acc)
    let step = mt (AElem  pR 1 (Tmp i) lP pSz) x:ss
        loop = for1 (eAnn p) i 1 ILt (Tmp szR) step
    sseed <- writeRF zop [x] (IT acc)
    pure $ plPP$szR =:ev (eAnn p) (pR,lP):mt (AElem pR 1 0 lP pSz) x:sseed++[loop]
eval (Id _ (FoldOfZip zop op [p, q])) acc | tPs <- eAnn p, Just (tP, pSz) <- aRr tPs, Just (tQ, qSz) <- aRr (eAnn q) = do
    x <- rtemp tP; y <- rtemp tQ
    i <- nI; szR <- nI
    (plPP, (lP, pR)) <- plA p; (plQ, (lQ, qR)) <- plA q
    ss <- writeRF op [IT acc, x, y] (IT acc)
    let step = mt (AElem pR 1 (Tmp i) lP pSz) x:mt (AElem qR 1 (Tmp i) lQ qSz) y:ss
        loop = for1 (eAnn p) i 1 ILt (Tmp szR) step
    seed <- writeRF zop [x,y] (IT acc)
    pure $ plPP$plQ$szR =: ev tPs (pR,lP):mt (AElem pR 1 0 lP pSz) x:mt (AElem qR 1 0 lQ qSz) y:seed++[loop]
eval e _          = error (show e)

frel :: Builtin -> Maybe FRel
frel Gte=Just FGeq; frel Lte=Just FLeq; frel Eq=Just FEq; frel Neq=Just FNeq; frel Lt=Just FLt; frel Gt=Just FGt; frel _=Nothing

mFop :: Builtin -> Maybe FBin
mFop Plus=Just FPlus; mFop Times=Just FTimes; mFop Minus=Just FMinus; mFop Div=Just FDiv; mFop Exp=Just FExp
mFop Max=Just FMax; mFop Min=Just FMin; mFop _=Nothing

mB :: Builtin -> Maybe BBin
mB And=Just AndB;mB Or=Just OrB;mB Xor=Just XorB; mB Eq=Just BEq; mB _=Nothing

mOp :: Builtin -> Maybe IBin
mOp Plus=Just IPlus;mOp Times=Just ITimes;mOp Minus=Just IMinus; mOp Mod=Just IRem
mOp Sl=Just IAsl;mOp Sr=Just IAsr;mOp A.IDiv=Just Op.IDiv;mOp a=BI<$>mB a

mFun :: Builtin -> Maybe FUn
mFun Sqrt=Just FSqrt; mFun Log=Just FLog; mFun Sin=Just FSin; mFun Cos=Just FCos; mFun Abs=Just FAbs; mFun Neg=Just FNeg; mFun _=Nothing

mFEval :: E (T ()) -> Maybe (CM F1E)
mFEval (FLit _ d) = Just (pure $ ConstF d)
mFEval (Var _ x) = Just $ do
    st <- gets dvars
    pure (FTmp (getT st x))
mFEval _ = Nothing

cond :: E (T ()) -> E (T ()) -> E (T ()) -> RT -> CM (Maybe AL, [CS ()])
cond (EApp _ (EApp _ (Builtin (Arrow F _) op) c0) c1) e e1 (FT t) | Just cmp <- frel op, Just cfe <- mFEval e1 = do
    c0R <- nF; c1R <- nF
    plC0 <- feval c0 c0R; plC1 <- feval c1 c1R
    eR <- nF; fe <- cfe
    plE <- feval e eR
    pure (Nothing, plC0 ++ plC1 ++ [MX () t fe] ++ plE ++ [Fcmov () (FRel cmp (FTmp c0R) (FTmp c1R)) t (FTmp eR)])
cond (EApp _ (EApp _ (Builtin (Arrow F _) o) c0) c1) e0 e1 t | Just f <- frel o, isIF (eAnn e0) = do
    c0R <- nF; c1R <- nF
    plC0 <- feval c0 c0R; plC1 <- feval c1 c1R
    plE0 <- eeval e0 t; plE1 <- eeval e1 t
    pure (Nothing, plC0 ++ plC1 ++ [If () (FRel f (FTmp c0R) (FTmp c1R)) plE0 plE1])
cond (EApp _ (EApp _ (Builtin (Arrow I _) op) c0) c1) e e1 (FT t) | Just cmp <- rel op, Just cfe <- mFEval e1 = do
    c0R <- nI
    plC0 <- eval c0 c0R
    (plC1,c1e) <- plC c1
    eR <- nF; fe <- cfe
    plE <- feval e eR
    pure (Nothing, plC0 ++ plC1 ([MX () t fe] ++ plE ++ [Fcmov () (IRel cmp (Tmp c0R) c1e) t (FTmp eR)]))
cond (EApp _ (EApp _ (Builtin (Arrow I _) op) c0) c1) e0 e1 t | Just cmp <- rel op, isIF (eAnn e0) = do
    c0R <- nI; c1R <- nI
    plC0 <- eval c0 c0R; plC1 <- eval c1 c1R
    plE0 <- eeval e0 t; plE1 <- eeval e1 t
    pure (Nothing, plC0 ++ plC1 ++ [If () (IRel cmp (Tmp c0R) (Tmp c1R)) plE0 plE1])
cond p e0 e1 t | isIF (eAnn e0) = do
    pR <- nBT
    plPP <- peval p pR; plE0 <- eeval e0 t; plE1 <- eeval e1 t
    pure (Nothing, plPP ++ [If () (Is pR) plE0 plE1])

f2eval :: E (T ()) -> F2Temp -> CM [CS ()]
f2eval (LLet _ b e) t = do
    ss <- llet b
    (ss++) <$> f2eval e t
f2eval (Var _ x) t = do {st <- gets d2vars; pure [MX2 () t (FTmp $ getT st x)]}
f2eval (EApp _ (EApp _ (Builtin _ Plus) e0) (EApp _ (EApp _ (Builtin _ Times) e1) e2)) t = do
    (pl0,t0) <- plD2 e0; (pl1,t1) <- plD2 e1; (pl2,t2) <- plD2 e2
    pure $ pl0 $ pl1 $ pl2 [MX2 () t (FBin FPlus (FTmp t0) (FBin FTimes (FTmp t1) (FTmp t2)))]
f2eval (EApp _ (EApp _ (Builtin _ op) e0) e1) t | Just fb <- mFop op = do
    (pl0,e0R) <- plD2 e0; (pl1,e1R) <- plD2 e1
    pure $ pl0 $ pl1 [MX2 () t (FBin fb (FTmp e0R) (FTmp e1R))]
f2eval (EApp _ (Builtin _ f) e) t | Just ff <- mFun f = do
    (plE,eC) <- plD2 e
    pure $ plE [MX2 () t (FUn ff (FTmp eC))]
f2eval (FLit _ x) t = pure [MX2 () t (ConstF (x,x))]
f2eval e _ = error (show e)

feval :: E (T ()) -> FTemp -> CM [CS ()]
feval (LLet _ b e) t = do
    ss <- llet b
    (ss++) <$> feval e t
feval (ILit _ x) t = pure [MX () t (ConstF $ fromIntegral x)] -- if it overflows you deserve it
feval (FLit _ x) t = pure [MX () t (ConstF x)]
feval (Var _ x) t = do
    st <- gets dvars
    pure [MX () t (FTmp $ getT st x)]
feval (EApp _ (EApp _ (Builtin _ A.R) (FLit _ 0)) (FLit _ 1)) t = pure [FRnd () t]
feval (EApp _ (EApp _ (Builtin _ A.R) (FLit _ 0)) e1) t = do
    (plE1,e1e) <- plD e1
    pure $ plE1 [FRnd () t, MX () t (FTmp t*e1e)]
feval (EApp _ (EApp _ (Builtin _ A.R) e0) e1) t = do
    (plE0,e0e) <- plD e0; (plE1, e1e) <- plD e1
    pure $ plE0 $ plE1 [FRnd () t, MX () t ((e1e-e0e)*FTmp t+e0e)]
feval (EApp _ (EApp _ (Builtin _ Plus) e0) (EApp _ (EApp _ (Builtin _ Times) e1) e2)) t = do
    (pl0,t0) <- plF e0; (pl1,t1) <- plF e1; (pl2,t2) <- plF e2
    pure $ pl0 $ pl1 $ pl2 [MX () t (FTmp t0+FTmp t1*FTmp t2)]
feval (EApp _ (EApp _ (Builtin _ Minus) e0) (EApp _ (EApp _ (Builtin _ Times) e1) e2)) t = do
    (pl0,t0) <- plF e0; (pl1,t1) <- plF e1; (pl2,t2) <- plF e2
    pure $ pl0 $ pl1 $ pl2 [MX () t (FTmp t0-FTmp t1*FTmp t2)]
feval (EApp _ (EApp _ (Builtin _ op) e0) e1) t | Just fb <- mFop op = do
    (pl0,e0e) <- plD e0; (pl1,e1R) <- plF e1
    pure $ pl0 $ pl1 [MX () t (FBin fb e0e (FTmp e1R))]
feval (EApp _ (EApp _ (Builtin _ IntExp) (FLit _ (-1))) n) t = do
    (plR,nR) <- plEV n
    pure $ plR [MX () t 1, Fcmov () (IUn IOdd (Tmp nR)) t (ConstF (-1))]
feval (EApp _ (EApp _ (Builtin _ IntExp) x) n) t = do
    xR <- nF; nR <- nI
    plX <- feval x xR; plN <- eval n nR
    pure $ plX ++ plN ++ [MX () t 1, While () nR IGt 0 [Ifn't () (IUn IEven (Tmp nR)) [MX () t (FTmp t*FTmp xR)], nR =: Bin IAsr (Tmp nR) 1, MX () xR (FTmp xR*FTmp xR)]]
feval (EApp _ (Builtin _ f) e) t | Just ff <- mFun f = do
    (plE,eC) <- plD e
    pure $ plE [MX () t (FUn ff eC)]
feval (EApp _ (Builtin _ Neg) x) t = do
    (plE,f) <- plD x
    pure $ plE [MX () t (negate f)]
feval (EApp _ (Builtin _ ItoF) e) t = do
    (pl,iE) <- plC e
    pure $ pl [MX () t (IE iE)]
feval (Cond _ p e0 e1) t = snd <$> cond p e0 e1 (FT t)
feval (EApp _ (Builtin _ Head) xs) t = do
    (plX, (l, a)) <- plA xs
    pure $ plX [MX () t (FAt (AElem a 1 0 l 8))]
feval (EApp _ (Builtin _ T) x) t = feval x t
feval (EApp _ (Builtin _ Flat) x) t = feval x t
feval (EApp _ (EApp _ (Builtin _ A1) e) i) t = do
    (plE, (lE, eR)) <- plA e; (plI, iR) <- plC i
    pure $ plE $ plI [MX () t (FAt (AElem eR 1 iR lE 8))]
feval (EApp _ (Builtin _ Last) xs) t = do
    (plX, (l, a)) <- plA xs
    pure $ plX [MX () t (FAt (AElem a 1 (ev (eAnn xs) (a,l)-1) l 8))]
feval (Id _ (FoldOfZip zop op [p])) acc | tPs <- eAnn p, Just (tP, pSz) <- aRr tPs = do
    x <- rtemp tP
    i <- nI; szR <- nI
    (plPP, (lP, pR)) <- plA p
    ss <- writeRF op [FT acc, x] (FT acc)
    let step = mt (AElem pR 1 (Tmp i) lP pSz) x:ss
        loop = for1 (eAnn p) i 1 ILt (Tmp szR) step
    sseed <- writeRF zop [x] (FT acc)
    pure $ plPP$szR =: ev tPs (pR,lP):mt (AElem pR 1 0 lP pSz) x:sseed++[loop]
feval (Id _ (FoldOfZip zop op [EApp _ (EApp _ (EApp _ (Builtin _ FRange) (FLit _ start)) (FLit _ end)) (ILit _ steps), ys])) acc | Just (tQ, qSz) <- aRr (eAnn ys) = do
    x <- nF; y <- rtemp tQ
    incrR <- nF; i <- nI
    plY <- eeval (EApp tQ (Builtin undefined Head) ys) y
    (plYs, (lY, yR)) <- plA ys
    plIncr <- feval (FLit F$(end-start)/realToFrac (steps-1)) incrR
    seed <- writeRF zop [FT x, y] (FT acc)
    ss <- writeRF op [FT acc, FT x, y] (FT acc)
    pure $ plYs $ plY ++ MX () x (ConstF start):seed ++ plIncr ++ [for1 (eAnn ys) i 1 ILt (ConstI$fromIntegral steps) (mt (AElem yR 1 (Tmp i) lY qSz) y:MX () x (FTmp x+FTmp incrR):ss)]
feval (Id _ (FoldOfZip zop op [EApp _ (EApp _ (EApp _ (Builtin _ FRange) start) end) steps, ys])) acc | Just (tQ, qSz) <- aRr (eAnn ys) = do
    x <- nF; y <- rtemp tQ
    incrR <- nF; i <- nI; n <- nI
    plX <- feval start x; plY <- eeval (EApp tQ (Builtin undefined Head) ys) y
    (plYs, (lY, yR)) <- plA ys
    plN <- eval steps n
    plIncr <- feval ((end `eMinus` start) `eDiv` (EApp F (Builtin (Arrow I F) ItoF) steps `eMinus` FLit F 1)) incrR
    seed <- writeRF zop [FT x, y] (FT acc)
    ss <- writeRF op [FT acc, FT x, y] (FT acc)
    pure $ plYs $ plY ++ plX ++ seed ++ plIncr ++ plN ++ [for1 (eAnn ys) i 1 ILt (Tmp n) (mt (AElem yR 1 (Tmp i) lY qSz) y:MX () x (FTmp x+FTmp incrR):ss)]
feval (Id _ (FoldOfZip zop op [EApp _ (EApp _ (EApp _ (Builtin _ IRange) start) _) incr, ys])) acc | Just (tQ, qSz) <- aRr (eAnn ys) = do
    x <- nI; y <- rtemp tQ
    szR <- nI; i <- nI
    plX <- eval start x; plY <- eeval (EApp tQ (Builtin undefined Head) ys) y
    (plYs, (lY, yR)) <- plA ys
    (plI,iE) <- plC incr
    seed <- writeRF zop [IT x, y] (FT acc)
    ss <- writeRF op [FT acc, IT x, y] (FT acc)
    pure $ plYs $ plY ++ plX ++ seed ++ plI (szR =: ev (eAnn ys) (yR,lY):[for1 (eAnn ys) i 1 ILt (Tmp szR) (mt (AElem yR 1 (Tmp i) lY qSz) y:x+=iE:ss)])
feval (Id _ (FoldOfZip zop op [p, q])) acc | tPs <- eAnn p, Just (tP, pSz) <- aRr tPs, Just (tQ, qSz) <- aRr (eAnn q) = do
    x <- rtemp tP; y <- rtemp tQ
    i <- nI; szR <- nI
    (plPP, (lP, pR)) <- plA p; (plQ, (lQ, qR)) <- plA q
    ss <- writeRF op [FT acc, x, y] (FT acc)
    let step = mt (AElem pR 1 (Tmp i) lP pSz) x:mt (AElem qR 1 (Tmp i) lQ qSz) y:ss
        loop = for1 tP i 1 ILt (Tmp szR) step
    seed <- writeRF zop [x,y] (FT acc)
    pure $ plPP$plQ$szR =: ev tPs (pR,lP):mt (AElem pR 1 0 lP pSz) x:mt (AElem qR 1 0 lQ qSz) y:seed++[loop]
feval (EApp _ (EApp _ (Builtin _ Fold) op) e) acc | tXs <- eAnn e, Just c <- fca op = do
    x0 <- nF; acc0 <- nF; acc2 <- newF2Temp; x <- newF2Temp
    i <- nI; szR <- nI
    (plX, (lX, xR)) <- plA e
    ss1 <- writeRF op [FT acc, FT x0] (FT acc)
    ss <- write2 op [acc2, x] acc2
    let seedO = case c of {FPlus -> MX2 () acc2 (ConstF (0,0)); FTimes -> MX2 () acc2 (ConstF (1,1)); FMax -> Fill () acc2 acc; FMin -> Fill () acc2 acc}
    -- TODO: use shape information (F2orE etc.)
    let loop = f21o tXs i 1 (ILt) (Tmp szR) (MX2 () x (FAt (AElem xR 1 (Tmp i) lX 8)):ss) (MX () x0 (FAt (AElem xR 1 (Tmp i) lX 8)):ss1)
    pure $ plX$szR=:ev tXs (xR,lX):MX () acc (FAt (AElem xR 1 0 lX 8)):seedO:[loop, Comb () c acc0 acc2, MX () acc (FBin c (FTmp acc) (FTmp acc0))]
  where
    fca (Lam _ _ (Lam _ _ (EApp _ (EApp _ (Builtin _ b) _) _))) | fS b = mFop b; fca _=Nothing
feval (EApp _ (EApp _ (Builtin _ Fold) op) e) acc | tXs <- eAnn e = do
    x <- nF; i <- nI; szR <- nI
    (plE, (l, aP)) <- plA e
    ss <- writeRF op [FT acc, FT x] (FT acc)
    let loopBody=MX () x (FAt (AElem aP 1 (Tmp i) l 8)):ss
        loop=for1 tXs i 1 ILt (Tmp szR) loopBody
    pure $ plE$szR =: ev tXs (aP,l):MX () acc (FAt (AElem aP 1 0 l 8)):[loop]
feval (EApp _ (EApp _ (EApp _ (Builtin _ Foldl) op) seed) e) acc | (Arrow _ (Arrow tX _)) <- eAnn op, isIF tX = do
    x <- rtemp tX
    i <- nI
    (plE, (l, eR)) <- plA e
    plAcc <- feval seed acc
    ss <- writeRF op [x, FT acc] (FT acc)
    let loopBody=mt (AElem eR 1 (Tmp i) l 8) x:ss++[i =: (Tmp i-1)]
        loop=While () i IGeq 0 loopBody
    pure $ plE $ plAcc++i =: (ev (eAnn e) (eR,l)-1):[loop]
feval (EApp _ (EApp _ (EApp _ (Builtin _ FoldA) op) seed) xs) acc | tXs@(Arr sh _) <- eAnn xs, (Arrow _ (Arrow tX _)) <- eAnn op, isIF tX = do
    x <- rtemp tX
    rnkR <- nI; szR <- nI; k <- nI
    (plE, (lX, xsR)) <- plA xs
    plAcc <- feval seed acc
    ss <- writeRF op [x, FT acc] (FT acc)
    xsRd <- nI
    let step=mt (Raw xsRd (Tmp k) lX 8) x:ss
        loop=for tXs k 0 ILt (Tmp szR) step
        plSz = case tIx tXs of {Just (_, is) -> szR=:ConstI (product is); Nothing -> SZ () szR xsR (Tmp rnkR) lX}
    pure $ plE $ plAcc ++ [rnkR =: eRnk sh (xsR, lX), plSz, xsRd=:DP xsR (Tmp rnkR), loop]
feval (EApp _ (EApp _ (EApp _ (Builtin _ FoldS) op) seed) (EApp _ (EApp _ (EApp _ (Builtin _ IRange) start) end) incr)) acc = do
    i <- nI; endR <- nI
    (plI,iE) <- plC incr
    plStart <- eval start i; plAcc <- feval seed acc; plEnd <- eval end endR
    ss <- writeRF op [FT acc, IT i] (FT acc)
    pure $ plStart ++ plAcc ++ plEnd ++ plI [While () i ILeq (Tmp endR) (ss++[i+=iE])]
feval (EApp _ (EApp _ (EApp _ (Builtin _ FoldS) op) seed) (EApp ty (EApp _ (EApp _ (Builtin _ FRange) start) end) nSteps)) acc = do
    i <- nI; startR <- nF; incrR <- nF; xR <- nF; endI <- nI
    plStart <- feval start startR; plAcc <- feval seed acc
    plEnd <- eval nSteps endI
    plIncr <- feval ((end `eMinus` start) `eDiv` (EApp F (Builtin (Arrow I F) ItoF) nSteps `eMinus` FLit F 1)) incrR
    ss <- writeRF op [FT acc, FT xR] (FT acc)
    pure $ plStart ++ MX () xR (FTmp startR):plEnd++plIncr++plAcc++[for ty i 0 ILt (Tmp endI) (ss++[MX () xR (FTmp xR+FTmp incrR)])]
feval (EApp _ (EApp _ (EApp _ (Builtin _ FoldS) op) seed) e) acc | (Arrow _ (Arrow tX _)) <- eAnn op, Just xSz <- nSz tX = do
    i <- nI; szR <- nI
    (plE, (l, eR)) <- plA e
    plAcc <- feval seed acc
    (x, wX, pinch) <- arg tX (AElem eR 1 (Tmp i) l xSz)
    ss <- writeRF op [FT acc, x] (FT acc)
    let loopBody=wX:ss
        loop=for (eAnn e) i 0 ILt (Tmp szR) loopBody
    pure $ plE $ plAcc++szR =: ev (eAnn e) (eR,l):m'p pinch [loop]
feval (EApp _ (EApp _ (EApp _ (Builtin _ FoldS) op) seed) e) acc | (Arrow _ (Arrow tX _)) <- eAnn op, Just xSz <- nSz tX, tArr <- eAnn e = do
    i <- nI; szR <- nI
    plAcc <- feval seed acc
    (plX, (lX, xR)) <- plA e
    (x, wX, pinch) <- arg tX (AElem xR 1 (Tmp i) lX xSz)
    ss <- writeRF op [FT acc, x] (FT acc)
    let loop=for tArr i 0 ILt (Tmp szR) (wX:ss)
    pure $ plX$plAcc++szR=:ev tArr (xR,lX):m'p pinch [loop]
feval (EApp _ (EApp _ (EApp _ (Builtin _ Iter) f) n) x) t = do
    (plN,nR) <- plC n
    plX <- feval x t
    ss <- writeRF f [FT t] (FT t)
    i <- nI
    let loop=For () i 0 ILt nR ss
    pure $ plX ++ plN [loop]
feval (EApp _ (Builtin _ (TAt i)) e) t = do
    k <- nI
    (offs, a, _, plT) <- πe e k
    pure $ m'sa k a++plT ++ MX () t (FAt (Raw k (ConstI$offs!!(i-1)) Nothing 1)):m'pop a
feval (EApp _ (Var _ f) x) t | isR (eAnn x) = do
    st <- gets fvars
    let (l, [a], FT r) = getT st f
    plX <- eeval x (art a)
    retL <- neL
    pure $ plX ++ [G () l retL, MX () t (FTmp r)]
feval (Id _ (FoldGen seed g f n)) t = do
    x <- nF; acc <- nF
    nR <- nI; k <- nI
    (plSeed,seedR) <- plF seed
    plN <- eval n nR
    uss <- writeRF g [FT x] (FT x)
    fss <- writeRF f [FT acc, FT x] (FT acc)
    pure $ plSeed $ plN++[MX () acc (FTmp seedR), MX () x (FTmp seedR), For () k 0 ILt (Tmp nR) (fss++uss), MX () t (FTmp acc)]
feval e _ = error (show e)

sac t = Sa8 () t.ConstI
popc = Pop8().ConstI

sa sz | sz `rem` 8 == 0 = Sa8 () | otherwise = Sa ()
pop sz | sz `rem` 8 == 0 = Pop8 () | otherwise = Pop ()

m'pop = maybe [] ((:[]).popc)
m'sa t = maybe []  ((:[]).sac t)

-- TODO: allow this to target multiple registers
πe :: E (T ()) -> Temp -> CM ([Int64], Maybe Int64, [AL], [CS ()]) -- element offsets, size to be popped off the stack, array labels kept live
πe (EApp (P tys) (Builtin _ Head) xs) t | offs <- szT tys, sz <- last offs = do
    xR <- nI
    (lX, plX) <- aeval xs xR
    pure (offs, Just sz, [], plX++[CpyE () (TupM t Nothing) (AElem xR 1 0 lX sz) 1 sz])
πe (EApp (P tys) (Builtin _ Last) xs) t | offs <- szT tys, sz <- last offs = do
    xR <- nI
    (lX, plX) <- aeval xs xR
    pure (offs, Just sz, [], plX++[CpyE () (TupM t Nothing) (AElem xR 1 (ev (eAnn xs) (xR,lX)-1) lX sz) 1 sz])
πe (Tup (P tys) es) t | offs <- szT tys, sz <- last offs = do
    (ls, ss) <- unzip <$>
        zipWithM (\e off ->
            case eAnn e of
                F     -> do {(plX, f) <- plD e; pure (Nothing, plX [WrF () (Raw t (ConstI off) Nothing 1) f])}
                I     -> do {(plX, i) <- plC e; pure (Nothing, plX [Wr () (Raw t (ConstI off) Nothing 1) i])}
                B     -> do {(plX, r) <- plP e; pure (Nothing, plX [WrP () (Raw t (ConstI off) Nothing 1) r])}
                Arr{} -> do {(pl, (l, r)) <- plA e; pure (l, pl [Wr () (Raw t (ConstI off) Nothing 1) (Tmp r)])}) es offs
    pure (offs, Just sz, catMaybes ls, concat ss)
πe (EApp (P tys) (EApp _ (Builtin _ A1) e) i) t | offs <- szT tys, sz <- last offs = do
    xR <- nI; iR <- nI
    (lX, plX) <- aeval e xR; plI <- eval i iR
    pure (offs, Just sz, mempty, plX ++ plI ++ [CpyE () (TupM t Nothing) (AElem xR 1 (Tmp iR) lX sz) 1 sz])
πe (Var (P tys) x) t = do
    st <- gets vars
    pure (szT tys, Nothing, undefined, [t =: Tmp (getT st x)])
πe (EApp _ (Builtin _ T) x) t = πe x t
πe (EApp _ (Builtin _ Flat) x) t = πe x t
πe (LLet _ b e) t = do
    ss <- llet b
    fourth (ss++) <$> πe e t
πe (EApp _ (EApp _ (EApp _ (Builtin _ Iter) f) n) x) t = do
    pre <- nI; ttemp <- nI
    (plN,nR) <- plC n
    (offs, mSz, _, plX) <- πe x pre
    let sz=last offs
    (_, ss) <- writeF f [IPA pre] (IT t)
    i <- nI
    let loop=For () i 0 ILt nR (ss++[CpyE () (TupM ttemp Nothing) (TupM t Nothing) 1 sz, CpyE () (TupM pre Nothing) (TupM ttemp Nothing) 1 sz])
    pure (offs, Just sz, [], m'sa pre mSz++plX++plN [sac ttemp sz, loop, popc sz]++m'pop mSz)
πe e _ = error (show e)

fourth f ~(x,y,z,w) = (x,y,z,f w)

qmap f g h k ~(x,y,z,w) = (f x, g y, h z, k w)
