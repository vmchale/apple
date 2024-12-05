module C.Trans ( writeC ) where

import           A
import           C
import           CF.AL                            (AL (..))
import qualified CF.AL                            as AL
import           Control.Composition              (thread, (-$))
import           Control.Monad                    (zipWithM)
import           Control.Monad.Trans.State.Strict (State, gets, modify, runState, state)
import           Data.Bifunctor                   (first, second)
import           Data.Functor                     (($>))
import           Data.Int                         (Int64)
import qualified Data.IntMap                      as IM
import qualified Data.IntSet                      as IS
import           Data.List                        (find, genericLength, scanl')
import           Data.Maybe                       (catMaybes)
import           Data.Tuple.Extra                 (second3)
import           Data.Word                        (Word64)
import           GHC.Float                        (castDoubleToWord64)
import           Nm
import           Nm.IntMap                        as Nm
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

nextI = state (\(CSt tϵ ar as l v b d d2 a f aas ts) -> (tϵ, CSt (tϵ+1) ar as l v b d d2 a f aas ts))
nextAA = state (\(CSt t ar as l v b d d2 a f aas ts) -> (as, CSt t ar (as+1) l v b d d2 a f aas ts))
nextArr r = state (\(CSt t a@(AL i) as l v b d d2 aϵ f aas ts) -> (a, CSt t (AL$i+1) as l v b d d2 aϵ f aas (AL.insert a r ts)))
neL = state (\(CSt t ar as l v b d d2 a f aas ts) -> (l, CSt t ar as (l+1) v b d d2 a f aas ts))

nI = ITemp <$> nextI; nBT = BTemp <$> nextI
nF = FTemp <$> nextI; nF2 = F2Temp <$> nextI

nIs = traverse (\_ -> nI); nFs = traverse (\_ -> nF); nF2s = traverse (\_ -> nF2)

addAA i aa = modify (\(CSt t ar as l v b d d2 a f aas ts) -> CSt t ar as l v b d d2 a f (IM.insert i aa aas) ts)
addVar n r = modify (\(CSt t ar as l v b d d2 a f aas ts) -> CSt t ar as l (insert n r v) b d d2 a f aas ts)
addD n r = modify (\(CSt t ar as l v b d d2 a f aas ts) -> CSt t ar as l v b (insert n r d) d2 a f aas ts)
addD2 n r = modify (\(CSt t ar as l v b d d2 a f aas ts) -> CSt t ar as l v b d (insert n r d2) a f aas ts)
addB n r = modify (\(CSt t ar as l v b d d2 a f aas ts) -> CSt t ar as l v (insert n r b) d d2 a f aas ts)
addAVar n r = modify (\(CSt t ar as l v b d d2 a f aas ts) -> CSt t ar as l v b d d2 (insert n r a) f aas ts)
addF n f = modify (\(CSt t ar as l v b d d2 a fs aas ts) -> CSt t ar as l v b d d2 a (insert n f fs) aas ts)

bI n = state (\(CSt t ar as l v b d d2 a f aas ts) -> let r=ITemp t in (r, CSt (t+1) ar as l (insert n r v) b d d2 a f aas ts))
bD n = state (\(CSt t ar as l v b d d2 a f aas ts) -> let r=FTemp t in (r, CSt (t+1) ar as l v b (insert n r d) d2 a f aas ts))
bB n = state (\(CSt t ar as l v b d d2 a f aas ts) -> let r=BTemp t in (r, CSt (t+1) ar as l v (insert n r b) d d2 a f aas ts))

{-# SCC getT2 #-}
getT2 :: Nm a -> CSt -> Either FTemp F2Temp
getT2 n (CSt _ _ _ _ _ _ d d2 _ _ _ _) = case Nm.lookup n d2 of {Just f2 -> Right f2; Nothing -> Left$getT d n}

getT :: IM.IntMap b -> Nm a -> b
getT st n = findWithDefault (error ("Internal error: variable " ++ show n ++ " not assigned to a temp.")) n st

type CM = State CSt

infix 9 +=
(+=) t i = t =: (Tmp t+i)

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
eRnk sh (xR, lX) | Just i <- staRnk sh = KI i
                 | otherwise = EAt (ARnk xR lX)

ev, ec :: T a -> (Temp, Maybe AL) -> CE
ev (Arr (Ix _ i `Cons` _) _) _ = KI$fromIntegral i; ev _ (xR, lX) = EAt (ADim xR 0 lX)
ec (Arr (_ `Cons` Ix _ j `Cons` _) _) _ = KI$fromIntegral j; ec _ (xR, lX) = EAt (ADim xR 1 lX)

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

ipe, ipo :: I a -> Bool
ipe (Ix _ i)          = i > 0 && even i
ipe (StaPlus _ i0 i1) = ipe i0&&ipe i1||ipo i0&&ipo i1
ipe (StaMul _ i0 i1)  = ipe i0 || ipe i1
ipe _                 = False

ipo (Ix _ i)          = odd i
ipo (StaPlus _ i0 i1) = ipe i0&&ipo i1||ipo i0&&ipe i1
ipo (StaMul _ i0 i1)  = ipo i0 && ipo i1
ipo _                 = False

nzSh :: Sh a -> Bool
nzSh (i `Cons` Nil) = nz i
nzSh (i `Cons` sh)  = nz i && nzSh sh
nzSh _              = False

n1 :: Sh a -> Bool
n1 (i `Cons` _) = ni1 i; n1 _=False

ni1 (Ix _ i) | i > 1 = True
ni1 (StaPlus _ i0 i1) = ni1 i0 || ni1 i1
ni1 (StaMul _ i0 i1) = (nz i0&&ni1 i1) || (nz i1&&ni1 i0)
ni1 _ = False

nec :: T a -> Bool
nec (Arr (_ `Cons` i `Cons` _) _) = nz i; nec _=False

ro,re,ce,ao,ae :: Sh a -> Bool
ro (i `Cons` _) = ipo i; ro _ = False
re (i `Cons` _) = ipe i; re _ = False
ce (_ `Cons` i `Cons` _) = ipe i; ce _ = False
ao (i `Cons` Nil) = ipo i; ao (i `Cons` sh) = ipo i && ao sh; ao _ = False
ae (i `Cons` Nil) = ipe i; ae (i `Cons` sh) = ipe i || ae sh; ae _ = False

ter,tor,toc,tec :: T a -> Bool
ter (Arr sh _) = re sh; ter _ = False
tor (Arr sh _) = ro sh; tor _ = False
tec (Arr (_ `Cons` i `Cons` _) _) = ipe i; tec _ = False
toc (Arr (_ `Cons` i `Cons` _) _) = ipo i; toc _ = False

for (i `Cons` _) | nz i = For1 () 1; for _ = For () 1

rof sh = if nzSh sh then Rof1 () else Rof (); rof1 sh = if n1 sh then Rof1 () else Rof ()
fort (Arr sh _) = for sh; fort _ = For () 1
forc t = if nec t then For1 () 1 else For () 1

f21o (Arr (Ix _ i `Cons` Nil) _) | odd i = \tϵ el c eu ss _ -> F2orE () tϵ el c eu ss
                                 | even i = F2orO ()
f21o _                           = F2or ()

r21 (Arr (Ix _ i `Cons` Nil) _) | odd i = \tϵ c ss _ -> R2ofE () tϵ c ss | even i = R2ofO ()
r21 _                           = R2of ()

r2of ty | tor ty = R2ofO ()
        | ter ty = \tϵ c ss _ -> R2ofE () tϵ c ss
        | otherwise = R2of ()

f2or sh | ro sh = F2orO ()
        | re sh = \tϵ el c eu ss _ -> F2orE () tϵ el c eu ss
        | otherwise = F2or ()

f2ort (Arr sh _) = f2or sh; f2ort _ = F2or ()

f2orc ty | toc ty = F2orO ()
         | tec ty = \tϵ el c eu ss _ -> F2orE () tϵ el c eu ss
         | otherwise = F2or ()

f2ors sh | ao sh = F2or ()
         | ae sh = \tϵ el c eu ss _ -> F2orE () tϵ el c eu ss
         | otherwise = F2or ()

mIFs :: [E a] -> Maybe [Word64]
mIFs = fmap concat.traverse mIFϵ where mIFϵ (FLit _ d)=Just [castDoubleToWord64 d]; mIFϵ (ILit _ n)=Just [fromIntegral n]; mIFϵ (Tup _ xs)=mIFs xs; mIFϵ _=Nothing

writeC :: E (T ()) -> ([CS ()], LSt, AsmData, IM.IntMap Temp)
writeC = π.flip runState (CSt 0 (AL 0) 0 0 IM.empty IM.empty IM.empty IM.empty IM.empty IM.empty IM.empty IM.empty) . writeCM . fmap rLi where π (s, CSt t _ _ l _ _ _ _ _ _ aa a) = (s, LSt l t, aa, a)

writeCM :: E (T ()) -> CM [CS ()]
writeCM eϵ = do
    cs <- nIs [(0::Int)..5]; fs <- nFs [(0::Int)..5]
    (zipWith (\xr xr' -> MX () xr' (FTmp xr)) [F0,F1,F2,F3,F4,F5] fs ++) . (zipWith (\r r' -> r' =: Tmp r) [C0,C1,C2,C3,C4,C5] cs ++) <$> go eϵ fs cs where
    go (Lam _ x@(Nm _ _ F) e) (fr:frs) rs = addD x fr *> go e frs rs
    go (Lam _ x@(Nm _ _ B) e) frs (r:rs) = addB x (bt r) *> go e frs rs where bt (ITemp i)=BTemp i
    go (Lam _ (Nm _ _ F) _) [] _ = error "Not enough floating-point registers."
    go (Lam _ x@(Nm _ _ I) e) frs (r:rs) = addVar x r *> go e frs rs
    go (Lam _ x@(Nm _ _ Arr{}) e) frs (r:rs) = addAVar x (Nothing, r) *> go e frs rs
    go Lam{} _ [] = error "Not enough registers."
    go e _ _ | isF (eAnn e) = do {f <- nF ; (++[MX () FRet0 (FTmp f)]) <$> feval e f} -- avoid clash with xmm0 (arg + ret)
             | isI (eAnn e) = do {t <- nI; (++[CRet =: Tmp t]) <$> eval e t} -- avoid clash when calling functions
             | isB (eAnn e) = do {t <- nBT; (++[MB () CBRet (Is t)]) <$> peval e t}
             | isArr (eAnn e) = do {(i,l,r) <- maa e; pure$r++[CRet =: Tmp i]++case l of {Just m -> [RA () m]; Nothing -> []}}
             | P [F,F] <- eAnn e = do {t <- nI; (_,_,_,p) <- πe e t; pure$sac t 16:p++[MX () FRet0 (FAt (Raw t 0 Nothing 8)), MX () FRet1 (FAt (Raw t 1 Nothing 8)), popc 16]}
             | ty@P{} <- eAnn e, b64 <- bT ty, (n,0) <- b64 `quotRem` 8 = do {t <- nI; a <- nextArr CRet; (_,_,ls,pl) <- πe e t; pure (sac t b64:pl++MaΠ () a CRet b64:CpyE () (TupM CRet (Just a)) (TupM t Nothing) (KI n) 8:popc b64:RA () a:(RA ()<$>ls))}

rtemp :: T a -> CM RT
rtemp F=FT<$>nF; rtemp I=IT<$>nI; rtemp B=PT<$>nBT

fc :: FBin -> Maybe (FTemp -> F2Temp -> CS ())
fc FPlus = Just (\_ x -> MX2 () x (ConstF (0,0))); fc FTimes = Just (\_ x -> MX2 () x (ConstF (1,1)))
fc FMax = Just (\x₀ x -> DS () x x₀); fc FMin = Just (\x₀ x -> DS () x x₀)
fc _ = Nothing

fS :: Builtin -> Bool
fS Times = True; fS Plus = True
fS Max = True; fS Min = True
fS _ = False

hasS :: E a -> Bool
hasS (EApp _ e0 e1)    = hasS e0&&hasS e1
hasS (Lam _ _ e)       = hasS e
hasS Var{}             = True
hasS FLit{}            = True
hasS Cond{}            = False
hasS (LLet _ (_,e) e') = hasS e&&hasS e'
hasS (Builtin _ b)     = bS b
  where
    bS Times = True; bS Plus = True; bS Minus = True; bS Div  = True
    bS Neg   = True; bS Max  = True; bS Min   = True; bS Sqrt = True
    bS Abs   = True; bS _    = False

write2 :: E (T ()) -> [F2Temp] -> F2Temp -> CM [CS ()]
write2 (Lam _ x e) (v:vs) vret = addD2 x v *> write2 e vs vret
write2 e [] r                  = f2eval e r

writeA :: E (T ())
       -> [Arg]
       -> CM (Temp, Maybe AL, [CS ()])
writeA e as | isArr (codT$eAnn e) = do {r <- nI; (\(x,y) -> (r,x,y)) <$> writeF e as (IT r)}
            | otherwise = error "Internal error. writeA called on a function not returning an array."

writeF :: E (T ())
       -> [Arg]
       -> RT
       -> CM (Maybe AL, [CS ()])
writeF (Lam _ x e) (AA r l:rs) ret = addAVar x (l,r) *> writeF e rs ret
writeF (Lam _ x e) (IPA r:rs) ret = addVar x r *> writeF e rs ret
writeF (Lam _ x e) (FA fr:rs) ret = addD x fr *> writeF e rs ret
writeF (Lam _ x e) (BA r:rs) ret = addB x r *> writeF e rs ret
writeF e [] (IT r) | isArr (eAnn e) = do {l <- nextArr r; (Just l,)<$>aeval e r l}
writeF e [] (IT r) | isΠR (eAnn e) = (\ ~(_,_,_,ss) -> (Nothing, ss))<$>πe e r
writeF e [] r = (Nothing,)<$>eeval e r

m'p :: Maybe (CS (), CS ()) -> [CS ()] -> [CS ()]
m'p (Just (a,pϵ)) = (++[pϵ]).(a:)
m'p Nothing       = id

sas :: [Maybe (CS (), CS ())] -> [CS ()] -> [CS ()]
sas = thread.fmap m'p

aSD :: E (T ()) -> [(T (), ArrAcc, Temp)] -> T () -> ArrAcc -> Temp -> CM ([CS ()], [Maybe (CS (), CS ())])
aSD f as rT rAt td = do
    (args, rArgs, pinchArgs) <- unzip3 <$> traverse (\(t,r,xd) -> second3 ((:[xd=:(Tmp xd+KI (bT t))]).($undefined)) <$> arg t (\_ -> r)) as
    (r, wR, pinch) <- rW rT (\_ -> rAt)
    ss <- writeRF f args r
    pure (concat rArgs++ss++[wR undefined, td=:(Tmp td+KI (bT rT))], pinch:pinchArgs)

aS :: E (T ()) -> [(T (), Temp -> Int64 -> ArrAcc)] -> T () -> (Temp -> Int64 -> ArrAcc) -> CM ([Temp] -> Temp -> [CS ()], [Maybe (CS (), CS ())])
aS f as rT rAt = do
    (args, rArgs, pinchArgs) <- unzip3 <$> traverse (\(t,r) -> arg t (r-$bT t)) as
    (r, wR, pinch) <- rW rT (rAt-$bT rT)
    ss <- writeRF f args r
    pure (\is j -> zipWith ($) rArgs is++ss++[wR j], pinch:pinchArgs)

type Ix'd = Temp -> ArrAcc

iXelem t rnk l sz ix = AElem t rnk l (Tmp ix) sz
ixarg t rnk l = AElem t rnk l.Tmp

infixr 8 .%
(.%) :: (a -> b -> c) -> (d -> a) -> b -> d -> c
(.%) f g x y = f (g y) x

arg :: T () -> Ix'd -> CM (RT, Temp -> CS (), Maybe (CS (), CS ()))
arg ty at | isR ty = do
    t <- rtemp ty
    pure (t, (mt.%at) t, Nothing)
arg ty at | isΠ ty = do
    slop <- nI
    pure $ let sz=bT ty in (IT slop, \k -> Mv () (TupM slop Nothing) (at k) sz, Just (sac slop sz, popc sz))

rW :: T () -> Ix'd -> CM (RT, Temp -> CS (), Maybe (CS (), CS ()))
rW ty at | isR ty = do
    t <- rtemp ty
    pure (t, (wt.%at) t, Nothing)
rW ty at | isΠ ty = do
    slopO <- nI
    pure $ let sz=bT ty in (IT slopO, \k -> Mv () (at k) (TupM slopO Nothing) sz, Just (sac slopO sz, popc sz))

aiA slopD (xd,lX) i n = cpy (Raw slopD 0 Nothing) (Raw xd (i*n) lX) n
aiR (td,l) (yR,lY,yRnk) n sz = [cpy (Raw td 0 l) (AElem yR yRnk lY 0) n sz, td+=(n*KI sz)]

writeRF :: E (T ()) -> [RT] -> RT -> CM [CS ()]
writeRF e args = fmap snd.writeF e (ra<$>args)

data Arg = IPA !Temp | FA !FTemp | AA !Temp !(Maybe AL) | BA !BTemp
data RT = IT !Temp | FT !FTemp | PT !BTemp

mt :: ArrAcc -> RT -> CS ()
mt p (FT t) = MX () t (FAt p); mt p (PT t) = MB () t (PAt p)
mt p (IT t) = t =: EAt p

wt :: ArrAcc -> RT -> CS ()
wt p (IT t) = Wr () p (Tmp t); wt p (FT t) = WrF () p (FTmp t)
wt p (PT t) = WrP () p (Is t)

cpy asrc dest n sz = CpyE () (asrc sz) (dest sz) n sz
mv asrc dest sz = Mv () (asrc sz) (dest sz) sz

ra (FT f)=FA f; ra (IT r)=IPA r; ra (PT r)=BA r
art (IPA r)=IT r;art (FA r)=FT r; art (BA r)=PT r

eeval :: E (T ()) -> RT -> CM [CS ()]
eeval e (IT t) = eval e t; eeval e (FT t) = feval e t
eeval e (PT t) = peval e t

data RI a b = Cell a | Index b

part :: [RI a b] -> ([a], [b])
part []           = ([], [])
part (Cell i:is)  = first (i:) $ part is
part (Index i:is) = second (i:) $ part is

iterDims allIx dts = part $ zipWith (\ixϵ dt -> case ixϵ of {Cell{} -> Cell dt; Index{} -> Index dt}) allIx dts

diml :: (Temp, Maybe AL) -> [CE] -> [CS ()]
diml (t,l) ds = zipWith (\d i -> Wr () (ADim t (KI i) l) d) ds [0..]

vSz :: Sh () -> Temp -> AL -> CE -> Int64 -> [CS ()]
vSz sh t a n sz = [Ma () sh a t 1 n sz, Wr () (ADim t 0 (Just a)) n]

v8 :: Sh () -> Temp -> AL -> CE -> [CS ()]
v8 sh t a n = vSz sh t a n 8

plDim :: Int64 -> (Temp, Maybe AL) -> CM ([Temp], [CS ()])
plDim rnk (a,l) =
    unzip <$> traverse (\at -> do {dt <- nI; pure (dt, dt =: EAt at)}) [ ADim a (KI i) l | i <- [0..rnk-1] ]

offByDim :: [Temp] -> CM ([Temp], [CS ()])
offByDim dims = do
    sts <- nIs (undefined:dims)
    let ss=zipWith3 (\s1 s0 d -> s1 =: (Tmp s0*Tmp d)) (tail sts) sts dims
    pure (reverse sts, head sts =: 1:ss)
    -- drop 1 for strides

data Cell a b = Fixed -- set by the larger procedure
              | Bound b -- to be iterated over

aall is ds bs cs = do {i <- nI; pure (i=:0:forAll is ds bs (cs i++[i+=1]))}

forAll is ds bs = thread (zipWith3 g is ds bs) where
    g t d b@(KI i) | i > 0 = (:[]) . For1 () d t 0 ILt b
    g t d b                = (:[]) . For () d t 0 ILt b

forAll1 is = forAll is (repeat 1); aall1 is = aall is (repeat 1)

cc :: [RI a b]
   -> (Temp, Maybe AL) -- ^ Data pointer
   -> [Temp]
   -> [Temp]
   -> Int64 -- ^ Element size
   -> Temp -- ^ Slop data pointer
   -> CM ([Temp], [CS ()]) -- ^ Temps associated with loop (to iterate over), copy cell in loop
cc rIx (srcP, srcL) dts strides sz dest =
    extrCell ecArg
  where
    ecArg = zipWith (\d tt -> case (d,tt) of (dϵ,Index{}) -> Bound dϵ; (_,Cell{}) -> Fixed) dts rIx

    extrCell :: [Cell () Temp] -> CM ([Temp], [CS ()])
    extrCell fixBounds = do
        (dims, ts, arrIxes, complts) <- switch fixBounds
        t <- nI; destd <- nI
        pure (complts, (destd =: Tmp dest:) $ forAll1 ts (Tmp<$>dims)
            [t =: EAt (At srcP (Tmp<$>strides) (Tmp<$>arrIxes) srcL sz), Wr () (Raw destd 0 Nothing sz) (Tmp t), destd+=KI sz])
      where switch (Bound d:ds) = do {t <- nI; qmap (d:) (t:) (t:) id <$> switch ds}
            switch (Fixed:ds)   = do {f <- nI; qmap id id (f:) (f:) <$> switch ds}
            switch []           = pure ([], [], [], [])

loopCell cr ixs (xR, lX) rnk sz = do
    (dts, dss) <- plDim rnk (xR, lX)
    (sts, sssϵ) <- offByDim (reverse dts)
    let _:strides = sts; sss=init sssϵ

    let (oDims, complDims) = iterDims allIx dts
    (slopP, _, aSlop, popS) <- plSlop sz slopRnk (Tmp<$>complDims)
    xRd <- nI; slopPd <- nI
    (complts, copyCell) <- cc allIx (xRd, lX) dts strides sz slopPd
    let pinchCell = (++[popS]) . ((dss++aSlop)++)
    pure (oDims, complts, sss++[xRd=:DP xR (KI rnk), slopPd=:DP slopP slopRnkE], pinchCell, slopP, copyCell)
  where
    allIx = let ixIs = IS.fromList ixs in [ if ix `IS.member` ixIs then Index() else Cell() | ix <- [1..fromIntegral rnk] ]
    slopRnk=fromIntegral cr; slopRnkE=KI slopRnk

vslop :: Int64 -> Int -> CM (Temp, [CS ()], CS ())
vslop sz n = do
    slopP <- nI
    pure (slopP, [sac slopP szSlop, Wr () (ARnk slopP Nothing) 1, Wr () (ADim slopP 0 Nothing) (fromIntegral n)], popc szSlop)
  where
    szSlop=16+fromIntegral n*sz

idims rnk oRnk xR lX = [EAt (ADim xR (KI l) lX) | l <- take (fromIntegral rnk) [oRnk-1,oRnk-2..] ]

plSlop :: Int64 -> Int64 -> [CE] -> CM (Temp, Temp, [CS ()], CS ())
plSlop sz slopRnk dims = do
    slopP <- nI; slopSz <- nI; slopE <- nI
    pure (slopP, slopSz,
            PlProd () slopSz dims
                :slopE=:(Tmp slopSz*KI sz+KI (8*(slopRnk+1)))
                :sa sz slopP (Tmp slopE):Wr () (ARnk slopP Nothing) (KI slopRnk)
                :diml (slopP, Nothing) dims,
         pop sz (Tmp slopE))

codT :: T () -> T ()
codT (Arrow _ t@Arrow{}) = codT t; codT (Arrow _ t) = t

r00 :: E (T ()) -> Maybe (E (T ()), [E (T ())])
r00 (EApp _ (Builtin _ (Rank is)) f) | all ((==0).fst) is = Just (f, [])
r00 (EApp _ f e) | Arr{} <- eAnn e = second (e:) <$> r00 f
r00 _ = Nothing

unroll :: T () -> [T ()]
unroll (Arrow t t') = t:unroll t'
unroll t            = [t]

llet :: (Nm (T ()), E (T ())) -> CM [CS ()]
llet (n,e') | isArr (eAnn e') = do
    (eR,l,ss) <- maa e'
    addAVar n (l,eR) $> ss
llet (n,e') | isI (eAnn e') = do {eR <- bI n; eval e' eR}
llet (n,e') | isF (eAnn e') = do {eR <- bD n; feval e' eR}
llet (n,e') | isB (eAnn e') = do {eR <- bB n; peval e' eR}
llet (n,e') | (tArgs, tC) <- ur (eAnn e'), all isR tArgs && isR tC = do
    l <- neL
    xs <- traverse rtemp tArgs; y <- rtemp tC
    let rrs=ra<$>xs
    (_, ss) <- writeF e' rrs y
    addF n (l, rrs, y)
    pure [C.Def () l ss]
  where
    ur (Arrow t t'@Arrow{}) = first (t:) (ur t')
    ur (Arrow t t')         = ([t],t')

data AD = AD { eigen :: !Temp, alabel :: !(Maybe AL), eit :: Maybe (T ())
             , ernk :: Maybe CE
             , elemSz :: Maybe Int64
             , arrn :: Maybe CE
             }

data RA = AI !AD | NA !RT | ΠA !Temp

fill :: E (T ()) -> AD -> [RA] -> CM ([Maybe (CS (), CS ())], [CS ()])
fill (EApp _ (Builtin _ Zip) op) (AD t lA (Just (Arr sh _)) _ _ _) [AI (AD aPX lX _ _ _ (Just n)), AI (AD aPY lY _ _ _ _)]
    | (Arrow tX (Arrow tY tC)) <- eAnn op, nind tX && nind tY && nind tC = do
    (step, pinches) <- aS op [(tX, ixarg aPX 1 lX), (tY, ixarg aPY 1 lY)] tC (ixarg t 1 lA)
    loop <- afor sh 0 ILt n $ \i -> step (repeat i) i
    pure (pinches, [loop])
fill (EApp _ (Builtin _ Succ) op) (AD t lA (Just (Arr sh _)) _ _ (Just n')) [AI (AD xR lX _ _ _ _)]
    | Arrow tX (Arrow _ tZ) <- eAnn op = do
    (step, pinches) <- aS op [(tX, \iϵ -> AElem xR 1 lX (Tmp iϵ+1)), (tX, ixarg xR 1 lX)] tZ (ixarg t 1 lA)
    loop <- afor sh 0 ILt n' $ \i -> step (repeat i) i
    pure (pinches, [loop])
fill (EApp _ (Builtin _ ScanS) op) (AD t lA _ _ _ (Just n)) [NA acc, AI (AD aP l (Just tXs) _ _ _)]
    | Arrow tX (Arrow tY _) <- eAnn op, Just xSz <- rSz tX, Just ySz <- nSz tY = do
    (x, wX, pinch) <- arg tY (iXelem aP 1 l ySz)
    ss <- writeRF op [acc, x] acc
    loop <- afort tXs 0 ILt n (\i -> wt (AElem t 1 lA (Tmp i) xSz) acc:wX i:ss)
    pure ([pinch], [loop])

rfill :: E (T ()) -> AD -> [RA] -> CM [CS ()]
rfill (Builtin (Arr sh F) Eye) (AD t lA _ _ _ _) [] | Just [i,_] <- staIx sh = do
    td <- nI
    loop <- afors sh 0 ILt (KI i) $ \k -> [WrF () (At td [KI i, 1] [Tmp k, Tmp k] lA 8) (ConstF 1)]
    -- could use cache instruction here?
    pure [td=:DP t 2, loop]
rfill (Builtin (Arr sh I) Eye) (AD t lA _ _ _ _) [] | Just [i,_] <- staIx sh = do
    td <- nI
    loop <- afors sh 0 ILt (KI i) $ \k -> [Wr () (At td [KI i, 1] [Tmp k, Tmp k] lA 8) (KI 1)]
    pure [td=:DP t 2, loop]
rfill (Builtin _ Init) (AD t lA _ _ (Just sz) (Just n)) [AI (AD xR lX _ _ _ _)] =
    pure [cpy (AElem t 1 lA 0) (AElem xR 1 lX 0) n sz]
rfill (Builtin _ InitM) (AD t lA _ _ (Just sz) (Just n)) [AI (AD xR lX _ _ _ _)] =
    pure [cpy (AElem t 1 lA 0) (AElem xR 1 lX 0) n sz]
rfill (Builtin _ Tail) (AD t lA _ _ (Just sz) (Just n)) [AI (AD xR lX _ _ _ _)] =
    pure [cpy (AElem t 1 lA 0) (AElem xR 1 lX 1) n sz]
rfill (EApp _ (Builtin _ Map) f) (AD t lA _ _ _ (Just n)) [AI (AD xR lX (Just tXs) _ _ _)] | Arrow F F <- eAnn f, hasS f = do
    td <- nI; xRd <- nI; i <- nI
    x <- nF2; y <- nF2; x₀ <- nF; y₀ <- nF
    ss <- write2 f [x] y
    s1 <- writeRF f [FT x₀] (FT y₀)
    let step=MX2 () x (FAt (Raw xRd 0 lX 8)):xRd=:(Tmp xRd+16):ss++[Wr2F () (Raw td 0 lA 8) (FTmp y), td=:(Tmp td+16)]
        step1=MX () x₀ (FAt (Raw xRd 0 lX 8)):xRd=:(Tmp xRd+8):s1++[WrF () (Raw td 0 lA 8) (FTmp y₀), td=:(Tmp td+8)]
        loop=r2of tXs i n step step1
    pure [xRd=:DP xR 1,td=:DP t 1, loop]
rfill (EApp _ (Builtin _ Map) op) (AD t lA (Just (Arr sh _)) _ _ (Just n)) [AI (AD xR l _ _ _ _)] | (Arrow tD tC) <- eAnn op, nind tD = do
    xRd <- nI; td <- nI;
    (step, pinches) <- aSD op [(tD, Raw xRd 0 l undefined, xRd)] tC (Raw td 0 lA undefined) td
    loop <- arof sh n step
    pure (xRd=:DP xR 1:td=:DP t 1:sas pinches [loop])
rfill (Builtin _ CatE) (AD t lA _ _ (Just sz) _) [AI (AD xR lX _ _ _ (Just xn)), AI (AD yR lY _ _ _ (Just yn))] =
    pure [cpy (AElem t 1 lA 0) (AElem xR 1 lX 0) xn sz, cpy (AElem t 1 lA xn) (AElem yR 1 lY 0) yn sz]
rfill (Builtin _ ConsE) (AD t lA _ _ (Just sz) _) [NA xR, AI (AD xsR lX _ _ _ (Just n))] =
    pure [wt (AElem t 1 lA 0 sz) xR, cpy (AElem t 1 lA 1) (AElem xsR 1 lX 0) n sz]
rfill (Builtin _ ConsE) (AD t lA _ _ _ _) [ΠA xR, AI (AD xsR lX _ _ (Just sz) (Just n))] =
    pure [Mv () (AElem t 1 lA 0 sz) (TupM xR Nothing) sz, cpy (AElem t 1 lA 1) (AElem xsR 1 lX 0) n sz]
rfill (Builtin _ Snoc) (AD t lA _ _ (Just sz) _) [NA xR, AI (AD xsR lX _ _ _ (Just n))] =
    pure [wt (AElem t 1 lA n sz) xR, cpy (AElem t 1 lA 0) (AElem xsR 1 lX 0) n sz]
rfill (Builtin _ Snoc) (AD t lA _ _ _ _) [ΠA xR, AI (AD xsR lX _ _ (Just sz) (Just n))] =
    pure [Mv () (AElem t 1 lA n sz) (TupM xR Nothing) sz, cpy (AElem t 1 lA 0) (AElem xsR 1 lX 0) n sz]
rfill (Builtin _ Cyc) (AD t lA (Just (Arr oSh _)) _ (Just sz) _) [AI (AD xR lX _ _ _ (Just nx)), NA (IT nR)] = do
    ix <- nI
    loop <- arof oSh (Tmp nR) [cpy (AElem t 1 lA (Tmp ix)) (AElem xR 1 lX 0) nx sz, ix+=nx]
    pure [ix=:0, loop]
rfill (Builtin _ Re) (AD t lA (Just (Arr sh _)) _ (Just sz) _) [NA (IT nR), NA xR] =
    (:[]) <$> afor sh 0 ILt (Tmp nR) (\i -> [wt (AElem t 1 lA (Tmp i) sz) xR])
rfill (EApp _ (Builtin _ Scan) op) (AD t lA (Just (Arr oSh _)) _ (Just accSz) (Just n)) [AI (AD xR lX _ _ (Just xSz) _), NA acc, NA x] = do
    ss <- writeRF op [acc, x] acc
    loop <- afor1 oSh 1 ILeq n (\i -> wt (AElem t 1 lA (Tmp i-1) accSz) acc:mt (AElem xR 1 lX (Tmp i) xSz) x:ss)
    pure [mt (AElem xR 1 lX 0 xSz) acc, loop]
rfill (EApp _ (Builtin _ Outer) op) (AD t lA _ _ _ _) [AI (AD xR lX (Just tXs) _ _ (Just nx)), AI (AD yR lY (Just tYs) _ _ (Just ny))]
    | Arrow tX (Arrow tY tC) <- eAnn op = do
    i <- nI; j <- nI; k <- nI
    (step, pinches) <- aS op [(tX, ixarg xR 1 lX), (tY, ixarg yR 1 lY)] tC (ixarg t 2 lA)
    let loop=fort tXs i 0 ILt nx [fort tYs j 0 ILt ny (step [i,j] k++[k+=1])]
    pure (k=:0:sas pinches [loop])
rfill (Builtin _ Rot) (AD t lA _ _ _ _) [AI (AD xsR lX _ _ (Just sz) (Just nx)), NA (IT nR)] = do
    c <- nI
    pure [Ifn't () (IRel IGeq (Tmp nR) 0) [nR+=nx], c =: (nx-Tmp nR), cpy (AElem t 1 lA 0) (AElem xsR 1 lX (Tmp nR)) (Tmp c) sz, cpy (AElem t 1 lA (Tmp c)) (AElem xsR 1 lX 0) (Tmp nR) sz]
rfill (Builtin _ Re) (AD t lA (Just (Arr sh _)) _ (Just sz) _) [NA (IT nR), ΠA xR] =
    (:[]) <$> afor sh 0 ILt (Tmp nR) (\k -> [Mv () (AElem t 1 lA (Tmp k) sz) (TupM xR Nothing) sz])
rfill (Builtin _ RevE) (AD t lA (Just (Arr oSh _)) _ (Just sz) _) [AI (AD xR lX _ _ _ (Just n))] =
    (:[]) <$> afor oSh 0 ILt n (\i -> [mv (AElem t 1 lA (Tmp i)) (AElem xR 1 lX (n-Tmp i-1)) sz])
rfill (Builtin _ AddDim) (AD t lA _ (Just rnk) (Just sz) _) [AI (AD xR lX _ (Just xRnk) _ (Just n))] = do
    td <- nI; xRd <- nI
    pure [td=:DP t rnk, xRd=:DP xR xRnk, cpy (Raw td 0 lA) (Raw xRd 0 lX) n sz]

afor sh el c eu ss = do {i <- nI; pure (for sh i el c eu (ss i))}
afor1 sh el c eu ss = do {i <- nI; pure (ff () 1 i el c eu (ss i))} where
    ff | n1 sh = For1 | otherwise = For
afort (Arr sh _) el c eu ss = do {i <- nI; pure (for sh i el c eu (ss i))}
afors sh el c eu ss = do {i <- nI; pure (ff () 1 i el c eu (ss i))} where
    ff | nzSh sh = For1 | otherwise = For
arof sh n ss = do {i <- nI; pure (rof sh i n ss)}; arof1 sh n ss = do {k <- nI; pure (rof1 sh k n ss)}

maa :: E (T ()) -> CM (Temp, Maybe AL, [CS ()])
maa (Var _ x) = do
    st <- gets avars
    let (l,t) = {-# SCC "getA" #-} getT st x
    pure (t,l,[])
maa (Id _ (AShLit ns es)) | Just ws <- mIFs es = do
    t <- nI; n <- nextAA
    addAA n (rnk:fmap fromIntegral ns++ws)
    -- TODO: boolean lits
    pure (t, Nothing, [t =: LA n])
  where
    rnk=genericLength ns
maa e = do {t <- nI; a <- nextArr t; (t,Just a,) <$> aeval e t a}

aeval :: E (T ()) -> Temp -> AL -> CM [CS ()]
aeval (LLet _ b e) t a = do
    ss0 <- llet b
    ss1 <- aeval e t a
    pure (ss0++ss1)
aeval (Cond _ (EApp _ (EApp _ (Builtin (Arrow I _) op) c0) c1) e0 e1) t a | Just cmp <- rel op = do
    (plC0, c0E) <- plC c0; (plC1, c1E) <- plC c1
    plE0 <- aeval e0 t a; plE1 <- aeval e1 t a
    pure $ plC0 $ plC1 [If () (IRel cmp c0E c1E) plE0 plE1]
aeval (EApp (Arr sh F) (EApp _ (Builtin _ A.R) e0) e1) t a | Just ixs <- staIx sh = do
    (plE0,e0e) <- plD e0; (plE1,e1e) <- plD e1
    xR <- nF; scaleR <- nF
    let rnk=genericLength ixs; n=product ixs
    loop <- afors sh 0 ILt (KI n) $ \k ->
              [FRnd () xR, MX () xR (FTmp scaleR*FTmp xR+e0e), WrF () (AElem t rnk (Just a) (Tmp k) 8) (FTmp xR)]
    pure (plE0 $ plE1 (Ma () sh a t rnk (KI n) 8:diml (t, Just a) (KI<$>ixs)++MX () scaleR (e1e-e0e):[loop]))
aeval (EApp (Arr sh I) (EApp _ (Builtin _ A.R) e0) e1) t a | Just ixs <- staIx sh = do
    scaleR <- nI; iR <- nI
    (plE0,e0e) <- plC e0; (plE1,e1e) <- plC e1
    let rnk=genericLength ixs; n=product ixs
    loop <- afors sh 0 ILt (KI n) $ \k ->
              [Rnd () iR, iR =: (Bin IRem (Tmp iR) (Tmp scaleR) + e0e), Wr () (AElem t rnk (Just a) (Tmp k) 8) (Tmp iR)]
    pure (plE0$plE1$Ma () sh a t rnk (KI n) 8:diml (t, Just a) (KI<$>ixs)++scaleR=:(e1e-e0e+1):[loop])
aeval (EApp (Arr oSh ty) (Builtin _ Di) e) t a | Just sz <- nSz ty = do
    (plX, (lX, xR)) <- plA e
    td <- nI; xRd <- nI; n <- nI
    ll <- afor oSh 0 ILt (Tmp n) $ \i ->
            [mv (Raw td 0 (Just a)) (At xRd [Tmp n, 1] [Tmp i, Tmp i] lX) sz, td+=KI sz]
    pure $ plX$n=:ev (eAnn e) (xR,lX):vSz oSh t a (Tmp n) sz++[xRd=:DP xR 2, td=:DP t 1, ll]
aeval e@(Builtin (Arr sh _) Eye) t a | Just ixs <- staIx sh = do
    let n=product ixs
    contents <- rfill e (AD t (Just a) Nothing Nothing Nothing (Just$KI n)) []
    pure (Ma () sh a t 2 (KI n) 8:diml (t, Just a) (KI<$>ixs)++contents)
aeval (EApp (Arr sh _) (Builtin _ AddDim) x) t a | Just (ty,sz) <- rr (eAnn x) = do
    xR <- rtemp ty
    plX <- eeval x xR
    pure (plX++vSz sh t a 1 sz++[wt (AElem t 1 (Just a) 0 8) xR])
aeval (EApp (Arr sh _) (Builtin _ AddDim) x) t a | P{} <- eAnn x = do
    xR <- nI
    (szs, mS, _, plX) <- πe x xR
    let sz=last szs
    pure (m'sa xR mS++plX++vSz sh t a 1 sz++[Mv () (AElem t 1 (Just a) 0 sz) (TupM xR Nothing) sz]++m'pop mS)
aeval (EApp (Arr oSh _) g@(Builtin _ AddDim) xs) t a | (Arr sh ty) <- eAnn xs, Just sz <- nSz ty = do
    (plX, (lX, xR)) <- plA xs
    xRnk <- nI; szR <- nI; rnk <- nI
    contents <- rfill g (AD t (Just a) Nothing (Just$Tmp rnk) (Just sz) Nothing) [AI (AD xR lX Nothing (Just$Tmp xRnk) Nothing (Just$Tmp szR))]
    pure (plX$xRnk=:eRnk sh (xR,lX):SZ () szR xR (Tmp xRnk) lX:rnk =: (Tmp xRnk+1):Ma () oSh a t (Tmp rnk) (Tmp szR) sz:
           [Wr () (ADim t 0 (Just a)) 1, CpyD () (ADim t 1 (Just a)) (ADim xR 0 lX) (Tmp xRnk)]++contents)
aeval (EApp oTy@(Arr oSh _) e@(Builtin _ Init) x) t a | Just sz <- aB oTy = do
    nR <- nI
    (plX, (lX, xR)) <- plA x
    contents <- rfill e (AD t (Just a) Nothing Nothing (Just sz) (Just$Tmp nR)) [AI (AD xR lX Nothing Nothing Nothing Nothing)]
    pure (plX$nR =: (ev (eAnn x) (xR,lX)-1):vSz oSh t a (Tmp nR) sz++contents)
aeval (EApp oTy@(Arr oSh _) e@(Builtin _ InitM) x) t a | Just sz <- aB oTy = do
    nR <- nI
    (plX, (lX, xR)) <- plA x
    contents <- rfill e (AD t (Just a) Nothing Nothing (Just sz) (Just$Tmp nR)) [AI (AD xR lX Nothing Nothing Nothing Nothing)]
    pure (plX$nR =: Bin IMax (ev (eAnn x) (xR,lX)-1) 0:vSz oSh t a (Tmp nR) sz++contents)
aeval (EApp oTy@(Arr oSh _) e@(Builtin _ Tail) x) t a | Just sz <- aB oTy = do
    nR <- nI
    (plX, (lX, xR)) <- plA x
    contents <- rfill e (AD t (Just a) Nothing Nothing (Just sz) (Just$Tmp nR)) [AI (AD xR lX Nothing Nothing Nothing Nothing)]
    pure (plX$nR =: (ev (eAnn x) (xR,lX)-1):vSz oSh t a (Tmp nR) sz++contents)
aeval (EApp (Arr oSh _) (Builtin _ Head) xs) t a | Just (tX, xRnk) <- tRnk (eAnn xs), Just sz <- nSz tX = do
    (plX, (lX, xR)) <- plA xs
    (dts, plDs) <- plDim xRnk (xR, lX)
    szA <- nI
    pure (plX$tail plDs++PlProd () szA (Tmp<$>tail dts):Ma () oSh a t 1 (Tmp szA) sz:CpyD () (ADim t 0 (Just a)) (ADim xR 1 lX) (KI$xRnk-1):[cpy (AElem t 1 (Just a) 0) (AElem xR (KI xRnk) lX 0) (Tmp szA) sz])
                                               | otherwise = unsupported
aeval (EApp (Arr oSh _) (Builtin _ Last) xs) t a | Just (tX, xRnk) <- tRnk (eAnn xs), Just sz <- nSz tX = do
    (plX, (lX, xR)) <- plA xs
    (dts, plDs) <- plDim xRnk (xR, lX)
    let n=head dts
    szA <- nI
    pure (plX$plDs++PlProd () szA (Tmp<$>tail dts):Ma () oSh a t 1 (Tmp szA) sz:CpyD () (ADim t 0 (Just a)) (ADim xR 1 lX) (KI$xRnk-1):[cpy (AElem t 1 (Just a) 0) (AElem xR (KI xRnk) lX ((Tmp n-1)*Tmp szA)) (Tmp szA) sz])
                                               | otherwise = unsupported
aeval (EApp (Arr oSh _) (Builtin _ Tail) xs) t a | Just (tX, xRnk) <- tRnk (eAnn xs), Just sz <- nSz tX = do
    (plX, (lX, xR)) <- plA xs
    (dts, plDs) <- plDim xRnk (xR, lX)
    let n=head dts; rnkE=KI xRnk
    szA <- nI; szz <- nI; d1 <- nI
    pure (plX$plDs++PlProd () szz (Tmp<$>tail dts):d1=:(Tmp n-1):szA=:(Tmp szz*Tmp d1):Ma () oSh a t rnkE (Tmp szA) sz:Wr () (ADim t 0 (Just a)) (Tmp d1):CpyD () (ADim t 1 (Just a)) (ADim xR 1 lX) (KI$xRnk-1):[cpy (AElem t rnkE (Just a) 0) (AElem xR rnkE lX (Tmp szz)) (Tmp szA) sz])
                                               | otherwise = unsupported
aeval (EApp (Arr oSh _) (Builtin _ Init) xs) t a | Just (tX, xRnk) <- tRnk (eAnn xs), Just sz <- nSz tX = do
    (plX, (lX, xR)) <- plA xs
    (dts, plDs) <- plDim xRnk (xR, lX)
    let n=head dts; rnkE=KI xRnk
    szA <- nI; d1 <- nI
    pure (plX$plDs++d1=:(Tmp n-1):PlProd () szA (Tmp<$>d1:tail dts):Ma () oSh a t rnkE (Tmp szA) sz:Wr () (ADim t 0 (Just a)) (Tmp d1):CpyD () (ADim t 1 (Just a)) (ADim xR 1 lX) (KI$xRnk-1):[cpy (AElem t rnkE (Just a) 0) (AElem xR rnkE lX 0) (Tmp szA) sz])
aeval (EApp (Arr oSh _) (Builtin _ Flat) xs) t a | (Arr sh ty) <- eAnn xs, Just sz <- nSz ty = do
    (plX, (lX, xR)) <- plA xs
    xRnk <- nI; szR <- nI
    pure (plX$xRnk=:eRnk sh (xR,lX):SZ () szR xR (Tmp xRnk) lX:vSz oSh t a (Tmp szR) sz++[cpy (AElem t 1 (Just a) 0) (AElem xR (Tmp xRnk) lX 0) (Tmp szR) sz])
aeval (EApp _ f@(EApp _ (Builtin _ Map) op) e) t a | tX@(Arr sh _) <- eAnn e, (Arrow tD tC) <- eAnn op, Just sz <- nSz tC, nind tD = do
    (plE, (l, xR)) <- plA e
    nR <- nI
    contents <- rfill f (AD t (Just a) (Just tX) Nothing Nothing (Just$Tmp nR)) [AI (AD xR l (Just tX) Nothing Nothing Nothing)]
    pure (plE$nR=:ev tX (xR,l):vSz sh t a (Tmp nR) sz++contents)
aeval (EApp _ (EApp _ (Builtin _ Filt) p) xs) t a | Arrow tX _ <- eAnn p, tXs@(Arr sh _) <- eAnn xs, Just sz <- nSz tX = do
    szR <- nI; nR <- nI; b <- nBT
    (plX, (lX, xsR)) <- plA xs
    (xR, rX, pinch) <- arg tX (\kϵ -> AElem xsR 1 lX (Tmp kϵ) sz)
    ss <- writeRF p [xR] (PT b)
    loop <- afor sh 0 ILt (Tmp szR) $ \k -> rX k:ss++[If () (Is b) [w tX (AElem t 1 (Just a) (Tmp nR) sz) xR, nR+=1] []]
    pure (plX$szR =: ev tXs (xsR,lX)
        :Ma () sh a t 1 (Tmp szR) sz
        :m'p pinch [nR=:0, loop, Wr () (ADim t 0 (Just a)) (Tmp nR)])
  where
    w ty at tt      | isR ty = wt at tt
    w ty at (IT tt) | isΠ ty = Mv () at (TupM tt Nothing) (bT ty)
aeval (EApp _ (EApp _ (Builtin _ Ices) p) xs) t a | Arrow tX _ <- eAnn p, tXs@(Arr sh _) <- eAnn xs, Just sz <- nSz tX = do
    szR <- nI; nR <- nI; b <- nBT
    (plX, (lX, xsR)) <- plA xs
    (xR, rX, pinch) <- arg tX (iXelem xsR 1 lX sz)
    ss <- writeRF p [xR] (PT b)
    loop <- afor sh 0 ILt (Tmp szR) $ \k -> rX k:ss++[If () (Is b) [Wr () (AElem t 1 (Just a) (Tmp nR) 8) (Tmp k), nR+=1] []]
    pure (plX$szR=:ev tXs (xsR,lX)
        :Ma () sh a t 1 (Tmp szR) 8
        :m'p pinch [nR=:0, loop, Wr () (ADim t 0 (Just a)) (Tmp nR)])
aeval (EApp (Arr oSh _) (EApp _ (Builtin _ Map) f) xs) t a
    | (Arrow tD tC) <- eAnn f
    , Arr xSh _ <- eAnn xs
    , Just xRnk <- staRnk xSh
    , Just (ta, rnk) <- tRnk tD
    , Just szD <- nSz ta, Just sz <- nSz tC = do
    szR <- nI; xd <- nI; i <- nI
    (plX, (lX, xR)) <- plA xs
    (slopP, slopSz, aSlop, pops) <- plSlop szD rnk (idims rnk xRnk xR lX)
    (y, wRet, pinch) <- rW tC (iXelem t 1 (Just a) sz)
    (_, ss) <- writeF f [AA slopP Nothing] y
    let xDims=[EAt (ADim xR (KI l) lX) | l <- [0..(rnk-1)]]
        dimsFromIn=KI$xRnk-rnk
        oRnk=xRnk-rnk
    loop <- afors xSh 0 ILt (Tmp szR) $ \k -> cpy (AElem slopP (KI rnk) Nothing 0) (Raw xd (Tmp i) lX) (Tmp slopSz) szD:ss++[wRet k, i+=Tmp slopSz]
    pure (plX$aSlop++PlProd () szR xDims
        :Ma () oSh a t (KI oRnk) (Tmp szR) sz
            :CpyD () (ADim t 0 (Just a)) (ADim xR 0 lX) dimsFromIn
        :xd=:DP xR (KI xRnk):i=:0
        :m'p pinch
            (loop:[pops]))
aeval (EApp (Arr oSh _) (EApp _ (Builtin _ Map) f) xs) t a
    | (Arrow tD tC) <- eAnn f
    , (Arr xSh _) <- eAnn xs
    , Just xRnk <- staRnk xSh
    , Just (ta, rnk) <- tRnk tC
    , Just szO <- nSz ta, Just dSz <- nSz tD = do
    y <- nI; y0 <- nI; szX <- nI; szY <- nI
    td <- nI
    (plX, (lX, xR)) <- plA xs
    (x0, wX0, pinch0) <- arg tD (\_ -> AElem xR (KI xRnk) lX 0 dSz)
    (x, wX, pinch) <- arg tD (\kϵ -> AElem xR (KI xRnk) lX (Tmp kϵ) dSz)
    (lY0, ss0) <- writeF f [ra x0] (IT y0)
    (lY, ss) <- writeF f [ra x] (IT y)
    let xDims=[EAt (ADim xR (KI l) lX) | l <- [0..(xRnk-1)]]
        yDims=[EAt (ADim y0 (KI l) lY0) | l <- [0..(rnk-1)]]
        oRnk=xRnk+rnk
    loop <- afors xSh 0 ILt (Tmp szX) $ \k ->
                wX k:ss++aiR (td,Just a) (y,lY,KI rnk) (Tmp szY) szO
    pure (plX$m'p pinch0 (wX0 undefined:ss0)
        ++PlProd () szY yDims
        :PlProd () szX xDims
        :Ma () oSh a t (KI oRnk) (Tmp szX*Tmp szY) szO
            :CpyD () (ADim t 0 (Just a)) (ADim xR 0 lX) (KI xRnk)
            :CpyD () (ADim t (KI xRnk) (Just a)) (ADim y0 0 lY0) (KI rnk)
        :td=:DP t (KI$xRnk+rnk)
        :m'p pinch [loop])
aeval (EApp (Arr oSh _) (EApp _ (Builtin _ Map) f) xs) t a
    | (Arr xSh _) <- eAnn xs
    , Just xRnk <- staRnk xSh
    , Just ((ta0, rnk0), (ta1, rnk1)) <- mAA (eAnn f)
    , Just sz0 <- nSz ta0, Just sz1 <- nSz ta1 = do
    szR <- nI; szY <- nI
    i <- nI; j <- nI; kL <- nI; xd <- nI; td <- nI
    (plX, (lX, xR)) <- plA xs
    (slopP, slopSz, aSlop, pops) <- plSlop sz1 rnk0 (idims rnk0 xRnk xR lX)
    (y0, lY0, ss0) <- writeA f [AA slopP Nothing]
    (y, lY, ss) <- writeA f [AA slopP Nothing]
    let xDims=[EAt (ADim xR (KI l) lX) | l <- [0..(rnk0-1)]]
        yDims=[EAt (ADim y0 (KI l) lY0) | l <- [0..(rnk1-1)]]
        dimsFromIn=KI$xRnk-rnk0
        oRnk=xRnk-rnk0+rnk1
    loop <- afors xSh 0 ILt (Tmp kL) $ \_ ->
                cpy (AElem slopP (KI rnk0) Nothing 0) (Raw xd (Tmp i) lX) (Tmp slopSz) sz0:ss++[cpy (Raw td (Tmp j) (Just a)) (AElem y (KI rnk1) lY 0) (Tmp szY) sz1, i+=Tmp slopSz, j+=Tmp szY]
    pure (plX$aSlop++xd=:DP xR (KI xRnk)
        :cpy (AElem slopP (KI rnk0) Nothing 0) (Raw xd 0 lX) (Tmp slopSz) sz0
        :ss0
        ++PlProd () szR (xDims++yDims)
        :Ma () oSh a t (KI oRnk) (Tmp szR) sz1
            :CpyD () (ADim t 0 (Just a)) (ADim xR 0 lX) dimsFromIn
            :CpyD () (ADim t dimsFromIn (Just a)) (ADim y0 0 lY0) (KI rnk1)
        :td=:DP t (KI oRnk)
        :PlProd () szY yDims
        :PlProd () kL xDims:i=:0:j=:0:loop
        :[pops])
                                                         | otherwise = unsupported
aeval e t a | (Arr oSh _) <- eAnn e, Just (f, xss) <- r00 e, all isF (unroll$eAnn f), (Arr sh _) <- eAnn (head xss), hasS f = do
    xRds <- nIs xss; tD <- nI
    rnkR <- nI; szR <- nI; i <- nI
    (plXs, (lXs, xRs)) <- second unzip.unzip <$> traverse plA xss
    let xR=head xRs; lX=head lXs
    arg1s <- nFs xss; ret1 <- nF
    args <- nF2s xss; ret <- nF2
    ss1 <- writeRF f [FT fa | fa <- reverse arg1s] (FT ret1)
    ss <- write2 f (reverse args) ret
    let m1s = zipWith3 (\arg1 xRd lXϵ -> MX () arg1 (FAt (Raw xRd (Tmp i) lXϵ 8))) arg1s xRds lXs; wr1 = WrF () (Raw tD (Tmp i) (Just a) 8) (FTmp ret1)
        ms = zipWith3 (\argϵ xRd lXϵ -> MX2 () argϵ (FAt (Raw xRd (Tmp i) lXϵ 8))) args xRds lXs; wr = Wr2F () (Raw tD (Tmp i) (Just a) 8) (FTmp ret)
        step1=m1s++ss1++[wr1]
        step=ms++ss++[wr]
        loop=f2or sh i 0 ILt (Tmp szR) step step1
    pure (thread plXs$rnkR=:eRnk sh (xR,lX):SZ () szR xR (Tmp rnkR) lX:Ma () oSh a t (Tmp rnkR) (Tmp szR) 8:CpyD () (ADim t 0 (Just a)) (ADim xR 0 lX) (Tmp rnkR):zipWith (\xRϵ xRd -> xRd=:DP xRϵ (Tmp rnkR)) xRs xRds++tD=:DP t (Tmp rnkR):[loop])
aeval e t a
    | Just (f, xss) <- r00 e
    , Just xsTys <- traverse (aN.eAnn) xss
    , (Arr sh _) <- eAnn (head xss)
    , tC <- codT (eAnn f)
    , Just szC <- nSz tC
    , Arr oSh _ <- eAnn e = do
    xRds <- nIs xss; tD <- nI
    rnkR <- nI; szR <- nI
    (plXs, (lXs, xRs)) <- second unzip.unzip <$> traverse plA xss
    let xR=head xRs; lX=head lXs
    (step, pinches) <- aS f (reverse$zipWith3 (\tXϵ xRd lXϵ -> (tXϵ, \iϵ -> Raw xRd (Tmp iϵ) lXϵ)) xsTys xRds lXs) tC (\iϵ -> Raw tD (Tmp iϵ) (Just a))
    loop <- afor sh 0 ILt (Tmp szR) (\i -> step (repeat i) i)
    pure (thread plXs$rnkR=:eRnk sh (xR,lX):SZ () szR xR (Tmp rnkR) lX:Ma () oSh a t (Tmp rnkR) (Tmp szR) szC:CpyD () (ADim t 0 (Just a)) (ADim xR 0 lX) (Tmp rnkR):zipWith (\xRϵ xRd -> xRd=:DP xRϵ (Tmp rnkR)) xRs xRds++tD=:DP t (Tmp rnkR):sas pinches [loop])
aeval (EApp (Arr oSh _) (EApp _ (EApp _ (Builtin _ (Rank [(0, _), (cr, Just ixs)])) op) xs) ys) t a
    | Just (yT, yRnk) <- tRnk (eAnn ys), Just (_, xRnk) <- tRnk (eAnn xs)
    , Arrow tX (Arrow _ tCod) <- eAnn op, Just (tC, cSz) <- rr tCod
    , Just xSz <- nSz tX, Just ySz <- nSz yT = do
    (plX, (lX, xR)) <- plA xs; (plY, (lY, yR)) <- plA ys
    zR <- rtemp tC
    let oRnk=yRnk-fromIntegral cr
    (x, pAX, pinch) <- arg tX (\ixϵ -> AElem xR (KI xRnk) lX (Tmp ixϵ) xSz)
    (oDims, complts, dps, pinchC, slopP, copyCell) <- loopCell cr ixs (yR, lY) yRnk ySz
    (_, ss) <- writeF op [ra x, AA slopP Nothing] zR
    oSz <- nI
    loop <- aall1 complts (Tmp<$>oDims) $ \ix -> pAX ix:copyCell ++ ss ++ [wt (AElem t (KI oRnk) (Just a) (Tmp ix) cSz) zR]
    pure (plX$plY$pinchC$
        [tϵ=:0 | tϵ <- complts]
        ++mt (AElem xR (KI xRnk) lX 0 xSz) x
        :dps
        ++PlProd () oSz (Tmp<$>oDims)
            :Ma () oSh a t (KI oRnk) (Tmp oSz) cSz
            :diml (t, Just a) (Tmp<$>oDims)
        ++m'p pinch loop)
aeval (EApp (Arr oSh _) (EApp _ (EApp _ (Builtin _ (Rank [(0, _), (cr, Just ixs)])) op) xs) ys) t a
    | Just (yT, yRnk) <- tRnk (eAnn ys), Just (_, xRnk) <- tRnk (eAnn xs)
    , (Arrow tX (Arrow _ tCod)) <- eAnn op, Just (tC, opRnk) <- tRnk tCod
    , Just xSz <- nSz tX, Just cSz <- rSz tC, Just ySz <- nSz yT = do
    (plX, (lX, xR)) <- plA xs; (plY, (lY, yR)) <- plA ys
    oSz <- nI; zSz <- nI
    td <- nI
    let oRnk=KI$yRnk+opRnk-fromIntegral cr
    (x, pAX, pinch) <- arg tX (\ixϵ -> AElem xR (KI xRnk) lX (Tmp ixϵ) xSz)
    (oDims, complts, ds, pinchC, slopP, copyCell) <- loopCell cr ixs (yR, lY) yRnk ySz
    (zR, lZ, ss) <- writeA op [ra x, AA slopP Nothing]
    loop <- aall1 complts (Tmp<$>oDims) $ \ix -> pAX ix:copyCell++ss++aiR (td,Just a) (zR,lZ,KI opRnk) (Tmp zSz) cSz
    (dots, doss) <- plDim opRnk (zR, lZ)
    pure (plX$plY$pinchC$
        [tϵ=:0 | tϵ <- complts]
        ++mt (AElem xR (KI xRnk) lX 0 xSz) x
        :ds++copyCell
        ++ss++doss
        ++PlProd () zSz (Tmp<$>dots)
        :PlProd () oSz (Tmp<$>(zSz:oDims))
            :Ma () oSh a t oRnk (Tmp oSz) cSz
            :diml (t, Just a) (Tmp<$>(oDims++dots))
        ++td=:DP t oRnk:m'p pinch loop)
aeval (EApp (Arr oSh _) (EApp _ (Builtin _ (Rank [(cr, Just ixs)])) f) xs) t a
    | Just (tA, rnk) <- tRnk (eAnn xs)
    , (Arrow _ tC) <- eAnn f
    , Just ySz <- nSz tC, Just aSz <- nSz tA = do
    (plX, (lX, xR)) <- plA xs
    oSz <- nI
    let oRnk=rnk-fromIntegral cr
    (oDims, complts, ds, pinchC, slopP, copyCell) <- loopCell cr ixs (xR, lX) rnk aSz
    (y, wY, pinch) <- rW tC (iXelem t (KI oRnk) (Just a) ySz)
    (_, ss) <- writeF f [AA slopP Nothing] y
    loop <- aall1 complts (Tmp<$>oDims) $ \di -> copyCell ++ ss ++ [wY di]
    pure (plX$pinchC$
        PlProd () oSz (Tmp<$>oDims)
            :Ma () oSh a t (KI oRnk) (Tmp oSz) ySz
            :diml (t, Just a) (Tmp<$>oDims)
        ++ds++m'p pinch loop)
aeval (EApp (Arr oSh _) (EApp _ (Builtin _ (Rank [(cr, Just ixs)])) f) xs) t a
    | Just (tA, xRnk) <- tRnk (eAnn xs)
    , (Arrow _ tCod) <- eAnn f
    , Just (tC, opRnk) <- tRnk tCod, Just cSz <- nSz tC, Just aSz <- nSz tA = do
    (plX, (lX, xR)) <- plA xs
    ySz <- nI; td <- nI; oSz <- nI
    let oRnk=KI$xRnk+opRnk-fromIntegral cr
    (oDims, complts, ds, pinchC, slopP, copyCell) <- loopCell cr ixs (xR, lX) xRnk aSz
    (yR, lY, ss) <- writeA f [AA slopP Nothing]
    let loop=forAll1 complts (Tmp<$>oDims)
                $ copyCell ++ ss ++ aiR (td,Just a) (yR,lY,KI opRnk) (Tmp ySz) cSz
    (dots, doss) <- plDim opRnk (yR, lY)
    pure (plX$pinchC$
        [tϵ=:0 | tϵ <- complts]
        ++ds++copyCell
        ++ss
        ++doss
        ++PlProd () ySz (Tmp<$>dots)
        :PlProd () oSz (Tmp<$>(ySz:oDims))
            :Ma () oSh a t oRnk (Tmp oSz) cSz
            :diml (t, Just a) (Tmp<$>(oDims++dots))
        ++td=:DP t oRnk:loop)
aeval (EApp oTy@(Arr oSh _) (EApp _ g@(Builtin _ CatE) x) y) t a | Just (ty, 1) <- tRnk oTy = do
    xnR <- nI; ynR <- nI; tn <- nI
    let sz=bT ty
    (plX, (lX, xR)) <- plA x; (plY, (lY, yR)) <- plA y
    contents <- rfill g (AD t (Just a) Nothing Nothing (Just sz) (Just$Tmp tn)) [AI (AD xR lX Nothing Nothing Nothing (Just$Tmp xnR)), AI (AD yR lY Nothing Nothing Nothing (Just$Tmp ynR))]
    -- TODO: if size is statically known, could place y later (one less alloc...)
    pure (plX$plY$xnR =: ev (eAnn x) (xR,lX):ynR =: ev (eAnn y) (yR,lY):tn =: (Tmp xnR+Tmp ynR):vSz oSh t a (Tmp tn) sz++contents)
aeval (EApp oTy@(Arr oSh _) (EApp _ g@(Builtin _ Cyc) xs) n) t a | Just sz <- aB oTy = do
    nO <- nI; nx <- nI
    (plN, nR) <- plEV n; (plX, (lX, xR)) <- plA xs
    contents <- rfill g (AD t (Just a) (Just oTy) Nothing (Just sz) Nothing) [AI (AD xR lX Nothing Nothing Nothing (Just$Tmp nx)), NA (IT nR)]
    pure (plX$plN$nx =: ev (eAnn xs) (xR,lX):nO =: (Tmp nx*Tmp nR):vSz oSh t a (Tmp nO) sz++contents)
aeval (EApp (Arr oSh _) (EApp _ (Builtin _ VMul) a) x) t aL
    | Just (F, [n_i]) <- tIx tX
    , Just ɴ <- mT n_i, ɴc <- KI ɴ = do
    i <- nI; j₀ <- nI; j <- nI; l <- nI; m <- nI; n <- nI; z <- nF2; za <- nF2; zx <- nF2; z₀ <- nF
    aRd <- nI; xRd <- nI; td <- nI; aid <- nI; xid <- nI
    (plAA, (lA, aR)) <- plA a; (plX, (lX, xR)) <- plA x
    let zero=f2or oSh l 0 ILt (Tmp m)
                [Wr2F () (Raw td (Tmp l) (Just aL) 8) (ConstF (0,0))]
                [WrF () (Raw td (Tmp l) (Just aL) 8) 0]
        loop = For1 () ɴc j₀ 0 ILt (Tmp n) [
                  fort tA i 0 ILt (Tmp m) $
                      let zr=Raw td (Tmp i) (Just aL) 8 in
                      [ aid=:(Tmp aRd+(Tmp n*Tmp i+Tmp j₀)*8)
                      , xid=:(Tmp xRd+Tmp j₀*8)
                      , MX () z₀ (FAt zr)
                      , Ins () z z₀
                      , For1 () 2 j 0 ILt ɴc
                             [ MX2 () za (FAt (Raw aid 0 lA 8)), aid+=16
                             , MX2 () zx (FAt (Raw xid 0 lX 8)), xid+=16
                             , MX2 () z (FBin FPlus (FTmp z) (FBin FTimes (FTmp za) (FTmp zx)))
                             ]
                      , Comb () Op.FPlus z₀ z
                      , WrF () zr (FTmp z₀)
                      ]
                  ]
    pure (plAA$
        plX$
        m=:ev tA (aR,lA)
        :v8 oSh t aL (Tmp m)
        ++n=:ev tX (xR,lX)
        :aRd=:DP aR 2:xRd=:DP xR 1:td=:DP t 1
        :[zero,loop])
  where
    tA=eAnn a; tX=eAnn x
    mT n = find (\k -> n `rem` k == 0) [32,16,8,4]
aeval (EApp (Arr oSh _) (EApp _ (Builtin _ VMul) a) x) t aL | Arr xSh F <- tX = do
    i <- nI; j <- nI; m <- nI; n <- nI; z0 <- nF; z <- nF2
    aRd <- nI; xRd <- nI; td <- nI
    (plAA, (lA, aR)) <- plA a; (plX, (lX, xR)) <- plA x
    (prologue, et, ~(Just zs)) <- if re xSh then pure (id, FTmp z0, Nothing) else do {zs <- nF; pure ((MX () zs 0:), FTmp zs+FTmp z0, Just zs)}
    let loop = fort tA i 0 ILt (Tmp m) $ prologue
                  [ MX2 () z (ConstF (0,0))
                  , f2or xSh j 0 ILt (Tmp n)
                        [ MX2 () z (FBin FPlus (FTmp z) (FBin FTimes (FAt (Raw aRd (Tmp n*Tmp i+Tmp j) lA 8)) (FAt (Raw xRd (Tmp j) lX 8)))) ]
                        [ MX () zs (FAt (Raw aRd (Tmp n*Tmp i+Tmp j) lA 8)*FAt (Raw xRd (Tmp j) lX 8)) ]
                  , Comb () Op.FPlus z0 z
                  , WrF () (Raw td 0 (Just aL) 8) et, td+=8
                  ]
    pure (plAA$plX$m=:ev tA (aR,lA):v8 oSh t aL (Tmp m)++n=:ev tX (xR,lX):aRd=:DP aR 2:xRd=:DP xR 1:td=:DP t 1:[loop])
  where
    tA=eAnn a; tX=eAnn x
aeval (EApp (Arr oSh _) (EApp _ (Builtin _ Mul) a) (EApp _ (Builtin _ T) b)) t aL
    | Just (F, [m,n]) <- tIx tA
    , Just (F, [o,_]) <- tIx tB
    , Just ɴ <- mT n, Just ᴍ <- mT m, Just ᴏ <- mT o = do
    let oᴋ=[0..(ᴏ-1)]; ᴏE=KI ᴏ
        mE=KI m;nE=KI n;oE=KI o
    i₀ <- nI; j₀ <- nI; k₀ <- nI; i <- nI; j <- nI; k <- nI; l <- nI
    aRd <- nI; bRd <- nI; td <- nI
    aid <- nI; bid <- nI; tid <- nI
    za <- nF2; z₀s <- nFs [1..ᴏ]; zs <- nF2s [1..ᴏ]; zbs <- nF2s [1..ᴏ]
    (plAA, (lA, aR)) <- plA a; (plB, (lB, bR)) <- plA b
    let zero=f2ors oSh l 0 ILt (mE*oE)
                [Wr2F () (Raw td (Tmp l) (Just aL) 8) (ConstF (0,0))]
                [WrF () (Raw td (Tmp l) (Just aL) 8) 0]
        loop=For1 () ᴍ i₀ 0 ILt mE [
                For1 () ᴏE j₀ 0 ILt oE [
                    For1 () ɴ k₀ 0 ILt nE [
                      For1 () 1 i 0 ILt ᴍ
                            [ tid=:(Tmp td+((Tmp i+Tmp i₀)*oE+Tmp j₀)*8)
                            , For1 () ᴏE j 0 ILt ᴏE $
                                  zipWith (\z₀ toffs -> MX () z₀ (FAt (Raw tid (KI toffs) (Just aL) 8))) z₀s oᴋ
                                ++zipWith (Ins ()) zs z₀s
                                ++[ aid=:(Tmp aRd+((Tmp i₀+Tmp i)*nE+Tmp k₀)*8)
                                  , bid=:(Tmp bRd+((Tmp j₀+Tmp j)*nE+Tmp k₀)*8)
                                  , For1 () 2 k 0 ILt ɴ $
                                      zipWith (\zb bo -> MX2 () zb (FAt (Raw bid (nE*KI bo) lB 8))) (drop 1 zbs) (drop 1 oᴋ)
                                      ++MX2 () za (FAt (Raw aid 0 lA 8)):aid+=16
                                      :MX2 () (head zbs) (FAt (Raw bid 0 lB 8)):bid+=16
                                      :zipWith (\z zb -> MX2 () z (FBin FPlus (FTmp z) (FBin FTimes (FTmp za) (FTmp zb)))) zs zbs
                                  ]
                                ++zipWith (Comb () Op.FPlus) z₀s zs
                                ++zipWith (\z₀ toff -> WrF () (Raw tid (KI toff) (Just aL) 8) (FTmp z₀)) (rot1 z₀s) (rot1 oᴋ)
                                ++[tid+=(ᴏE*8)]
                            ]
                        ]
                    ]
             ]
    pure (plAA$plB$
        Ma () oSh aL t 2 (KI$m*o) 8:diml (t, Just aL) [mE,oE]
        ++aRd=:DP aR 2:bRd=:DP bR 2:td=:DP t 2
        :[zero,loop])
  where
    tA=eAnn a; tB=eAnn b
    mT n | n `rem` 8 == 0 = Just 8 | n `rem` 4 == 0 = Just 4 | otherwise = Nothing
    rot1 xs = take (length xs) $ drop 1 $ cycle xs
aeval (EApp (Arr oSh _) (EApp _ (Builtin _ Mul) a) (EApp _ (Builtin _ T) b)) t aL | Arr _ F <- tA, Arr bSh _ <- tB = do
    i <- nI; j <- nI; k <- nI; m <- nI; l <- nI; n <- nI; o <- nI
    z <- nF2; z0 <- nF; za <- nF2; zb <- nF2; za1 <- nF; zb1 <- nF
    aRd <- nI; bRd <- nI; td <- nI
    tid <- nI; bid <- nI; aid <- nI
    (plAA, (lA, aR)) <- plA a; (plB, (lB, bR)) <- plA b
    (prologue, et, ~(Just zs)) <- if ce bSh then pure (id, FTmp z0, Nothing) else do {zs <- nF; pure ((MX () zs 0:), FTmp zs+FTmp z0, Just zs)}
    let zero=f2ors oSh l 0 ILt (Tmp m*Tmp o)
                [Wr2F () (Raw td (Tmp l) (Just aL) 8) (ConstF (0,0))]
                [WrF () (Raw td (Tmp l) (Just aL) 8) 0]
        loop=fort tA i 0 ILt (Tmp m)
                [ MT () tid (Tmp td+(Tmp i*Tmp o)*8)
                , forc tB j 0 ILt (Tmp o) $ prologue
                    [ MX2 () z (ConstF (0,0))
                    , MT () aid (Tmp aRd+(Tmp n*Tmp i)*8)
                    , MT () bid (Tmp bRd+(Tmp n*Tmp j)*8)
                    , f2orc tB k 0 ILt (Tmp n)
                            [ MX2 () za (FAt (Raw aid 0 lA 8)), aid+=16
                            , MX2 () zb (FAt (Raw bid 0 lB 8)), bid+=16
                            , MX2 () z (FBin FPlus (FTmp z) (FBin FTimes (FTmp za) (FTmp zb)))]
                            [ MX () za1 (FAt (Raw aid 0 lA 8)), aid+=8
                            , MX () zb1 (FAt (Raw bid 0 lB 8)), bid+=8
                            , MX () zs (FTmp zs+FTmp za1*FTmp zb1)
                            ]
                    , Comb () Op.FPlus z0 z
                    , WrF () (Raw tid 0 (Just aL) 8) et
                    , tid+=8
                    ]
                ]
    pure (plAA$plB$m=:ev tA (aR,lA):o=:ev tB (bR,lB)
        :Ma () oSh aL t 2 (Tmp m*Tmp o) 8:diml (t, Just aL) [Tmp m, Tmp o]
        ++n=:ec tA (aR,lA):aRd=:DP aR 2:bRd=:DP bR 2:td=:DP t 2
        :[zero,loop])
  where
    tA=eAnn a; tB=eAnn b
aeval (EApp (Arr oSh _) (EApp _ (Builtin _ Mul) a) b) t aL = do
    m <- nI; n <- nI; o <- nI; i <- nI; j <- nI; k <- nI; l <- nI; zr <- nF2; zr₀ <- nF; z₀ <- nF2; z₁ <- nF2; z₀₀ <- nF; z₁₀ <- nF
    aRd <- nI; bRd <- nI; td <- nI; bid <- nI; bidϵ <- nI
    (plAA, (lA, aR)) <- plA a; (plB, (lB, bR)) <- plA b
    let zero=f2ors oSh l 0 ILt (Tmp m*Tmp o)
                [Wr2F () (Raw td (Tmp l) (Just aL) 8) (ConstF (0,0))]
                [WrF () (Raw td (Tmp l) (Just aL) 8) 0]
        kjloop = f2ort tB k 0 ILt (Tmp n)
                    [ MX () z₀₀ (FAt (Raw aRd (Tmp k) lA 8))
                    , MX () z₁₀ (FAt (Raw aRd (Tmp k+1) lA 8))
                    -- thabove could be a single fetch (dup works on indexed SIMD registers)
                    , DS () z₀ z₀₀, DS () z₁ z₁₀
                    , let za=Raw td (Tmp j) (Just aL) 8 in
                        f2orc tB j 0 ILt (Tmp o)
                            [ MX2 () zr (FAt za)
                            , MX2 () zr (FBin FPlus (FTmp zr) (FBin FTimes (FTmp z₀) (FAt (Raw bid (Tmp j) lB 8))))
                            , MX2 () zr (FBin FPlus (FTmp zr) (FBin FTimes (FTmp z₁) (FAt (Raw bidϵ (Tmp j) lB 8))))
                            , Wr2F () za (FTmp zr)
                            ]
                            [ MX () zr₀ (FAt za)
                            , MX () zr₀ (FTmp zr₀+FTmp z₀₀*FAt (Raw bid (Tmp j) lB 8))
                            , MX () zr₀ (FTmp zr₀+FTmp z₁₀*FAt (Raw bidϵ (Tmp j) lB 8))
                            , WrF () za (FTmp zr₀)
                            ]
                    , bid+=(Tmp o*16)
                    , bidϵ+=(Tmp o*16)
                    ]
                    [ MX () z₀₀ (FAt (Raw aRd (Tmp k) lA 8))
                    , DS () z₀ z₀₀
                    , let za=Raw td (Tmp j) (Just aL) 8 in
                        f2orc tB j 0 ILt (Tmp o)
                          [ Wr2F () za (FBin FPlus (FAt za) (FBin FTimes (FTmp z₀) (FAt (Raw bid (Tmp j) lB 8)))) ]
                          [ WrF () za (FAt za+FTmp z₀₀*FAt (Raw bid (Tmp j) lB 8))]
                    , bid+=(Tmp o*8)
                    , bidϵ+=(Tmp o*8)
                    ]
        loop=fort tA i 0 ILt (Tmp m) [bid=:Tmp bRd, bidϵ=:(Tmp bid+Tmp o*8), kjloop, aRd+=(Tmp n*8), td+=(Tmp o*8)]
    pure (plAA$plB$m=:ev tA (aR,lA):n=:ec tA (aR,lA):o=:ec tB (bR,lB):
        Ma () oSh aL t 2 (Tmp m*Tmp o) 8:diml (t, Just aL) [Tmp m,Tmp o]
        ++aRd=:DP aR 2:bRd=:DP bR 2:td=:DP t 2
        :[zero,loop])
  where
    tA=eAnn a; tB=eAnn b
aeval (EApp (Arr oSh _) (EApp _ g@(Builtin _ ConsE) x) xs) t a | tX <- eAnn x, Just sz <- rSz tX = do
    xR <- rtemp tX; nR <- nI; nϵR <- nI
    plX <- eeval x xR
    (plXs, (l, xsR)) <- plA xs
    contents <- rfill g (AD t (Just a) Nothing Nothing (Just sz) Nothing) [NA xR, AI (AD xsR l Nothing Nothing Nothing (Just$Tmp nϵR))]
    pure (plX++plXs (nϵR =: ev (eAnn xs) (xsR,l):nR =: (Tmp nϵR+1):vSz oSh t a (Tmp nR) sz++contents))
aeval (EApp (Arr oSh _) (EApp _ g@(Builtin _ ConsE) x) xs) t a | tX <- eAnn x, isΠ tX, sz <- bT tX = do
    xR <- nI; nR <- nI; nϵR <- nI
    (_, mSz, _, plX) <- πe x xR
    (plXs, (lX, xsR)) <- plA xs
    contents <- rfill g (AD t (Just a) Nothing Nothing (Just sz) Nothing) [ΠA xR, AI (AD xsR lX Nothing Nothing (Just sz) (Just$Tmp nϵR))]
    pure (m'sa xR mSz++plX++plXs (nϵR =: ev (eAnn xs) (xsR,lX):nR =: (Tmp nϵR+1):vSz oSh t a (Tmp nR) sz++contents++m'pop mSz))
aeval (EApp (Arr oSh _) (EApp _ (Builtin _ ConsE) x) xs) t a | Just (tX, xRnk) <- tRnk (eAnn x), tXs <- eAnn xs, Just (_, xsRnk) <- tRnk tXs = do
    (plX, (lX, xR)) <- plA x; (plXs, (lXs, xsR)) <- plA xs
    (dts,dss) <- plDim xRnk (xR, lX)
    d1R <- nI; d1'R <- nI; szR <- nI; nX <- nI
    let rnkE=KI xsRnk; szX=bT tX
    pure (plXs$plX$d1R=:ev tXs (xsR,lXs):dss++d1'R=:(Tmp d1R+1):PlProd () nX (Tmp<$>dts):szR=:(Tmp d1'R*Tmp nX):Ma () oSh a t rnkE (Tmp szR) szX:Wr () (ADim t 0 (Just a)) (Tmp d1'R):CpyD () (ADim t 1 (Just a)) (ADim xsR 1 lXs) (KI$xsRnk-1):[cpy (AElem t rnkE (Just a) 0) (AElem xR (KI xRnk) lX 0) (Tmp nX) szX, cpy (AElem t rnkE (Just a) (Tmp nX)) (AElem xsR (KI xsRnk) lXs 0) (Tmp d1R*Tmp nX) szX])
                                                           | otherwise = unsupported
aeval (EApp (Arr oSh _) (EApp _ g@(Builtin _ Snoc) x) xs) t a | tX <- eAnn x, Just sz <- rSz tX = do
    xR <- rtemp tX; nR <- nI; nϵR <- nI
    plX <- eeval x xR
    (plXs, (l, xsR)) <- plA xs
    contents <- rfill g (AD t (Just a) Nothing Nothing (Just sz) Nothing) [NA xR, AI (AD xsR l Nothing Nothing Nothing (Just$Tmp nϵR))]
    pure (plXs$plX++nϵR =: ev (eAnn xs) (xsR,l):nR =: (Tmp nϵR+1):vSz oSh t a (Tmp nR) sz++contents)
aeval (EApp (Arr oSh _) (EApp _ g@(Builtin _ Snoc) x) xs) t a | tX <- eAnn x, isΠ tX, sz <- bT tX = do
    xR <- nI; nR <- nI; nϵR <- nI
    (_, mSz, _, plX) <- πe x xR
    (plXs, (lX, xsR)) <- plA xs
    contents <- rfill g (AD t (Just a) Nothing Nothing Nothing Nothing) [ΠA xR, AI (AD xsR lX Nothing Nothing (Just sz) (Just$Tmp nϵR))]
    pure (plXs$m'sa xR mSz++plX++nϵR =: ev (eAnn xs) (xsR,lX):nR =: (Tmp nϵR+1):vSz oSh t a (Tmp nR) sz++contents++m'pop mSz)
aeval (EApp (Arr oSh _) (EApp _ (Builtin _ Snoc) x) xs) t a | Just (tX, xRnk) <- tRnk (eAnn x), tXs <- eAnn xs, Just (_, xsRnk) <- tRnk tXs = do
    (plX, (lX, xR)) <- plA x; (plXs, (lXs, xsR)) <- plA xs
    (dts,dss) <- plDim xRnk (xR, lX)
    d1R <- nI; d1'R <- nI; szR <- nI; nX <- nI
    let rnkE=KI xsRnk; szX=bT tX
    pure (plXs$plX$d1R=:ev tXs (xsR,lXs):dss++d1'R=:(Tmp d1R+1):PlProd () nX (Tmp<$>dts):szR=:(Tmp d1'R*Tmp nX):Ma () oSh a t rnkE (Tmp szR) szX:Wr () (ADim t 0 (Just a)) (Tmp d1'R):CpyD () (ADim t 1 (Just a)) (ADim xsR 1 lXs) (KI$xsRnk-1):[cpy (AElem t rnkE (Just a) (Tmp d1R*Tmp nX)) (AElem xR (KI xRnk) lX 0) (Tmp nX) szX, cpy (AElem t rnkE (Just a) 0) (AElem xsR (KI xsRnk) lXs 0) (Tmp d1R*Tmp nX) szX])
                                                          | otherwise = unsupported
aeval (EApp oTy@(Arr sh _) (EApp _ g@(Builtin _ Re) n) x) t a | tX <- eAnn x, Just xSz <- rSz tX = do
    (plN, nR) <- plEV n
    xR <- rtemp tX; putX <- eeval x xR
    contents <- rfill g (AD t (Just a) (Just oTy) Nothing (Just xSz) Nothing) [NA$IT nR, NA xR]
    pure (plN$vSz sh t a (Tmp nR) xSz++putX++contents)
aeval (EApp oTy@(Arr sh _) (EApp _ g@(Builtin _ Re) n) x) t a | tX <- eAnn x, isΠ tX, sz <- bT tX = do
    xR <- nI
    (plN, nR) <- plEV n
    (_, mSz, _, plX) <- πe x xR
    contents <- rfill g (AD t (Just a) (Just oTy) Nothing (Just sz) Nothing) [NA (IT nR), ΠA xR]
    pure (plN$vSz sh t a (Tmp nR) sz++m'sa xR mSz++plX++contents++m'pop mSz)
aeval (EApp (Arr oSh _) (EApp _ (Builtin _ Re) n) x) t a | (Arr sh tO) <- eAnn x, sz <- bT tO = do
    (plN, nR) <- plEV n; (plX, (lX, xR)) <- plA x
    xRnk <- nI; oRnk <- nI; td <- nI; xRd <- nI; szX <- nI
    loop <- afor oSh 0 ILt (Tmp nR) $ \k -> [cpy (Raw td (Tmp k*Tmp szX) (Just a)) (Raw xRd 0 lX) (Tmp szX) sz]
    pure (plX$xRnk=:eRnk sh (xR,lX):oRnk=:(Tmp xRnk+1):SZ () szX xR (Tmp xRnk) lX
        :plN (Ma () oSh a t (Tmp oRnk) (Tmp szX*Tmp nR) sz:Wr () (ADim t 0 (Just a)) (Tmp nR):CpyD () (ADim t 1 (Just a)) (ADim xR 0 lX) (Tmp xRnk)
        :td=:DP t (Tmp oRnk):xRd=:DP xR (Tmp xRnk):[loop]))
aeval (EApp (Arr oSh _) (EApp _ (EApp _ (Builtin _ Zip) op) xs) ys) t a | Arrow F (Arrow F F) <- eAnn op, tXs <- eAnn xs, hasS op = do
    nR <- nI; i <- nI
    (plEX, (lX, xR)) <- plA xs; (plEY, (lY, yR)) <- plA ys
    xRd <- nI; yRd <- nI; td <- nI
    x <- nF2; y <- nF2; z <- nF2; x0 <- nF; y0 <- nF; z0 <- nF
    ss <- write2 op [x,y] z
    s1 <- writeRF op (FT<$>[x0,y0]) (FT z0)
    let step=MX2 () x (FAt (Raw xRd 0 lX 8)):xRd=:(Tmp xRd+16):MX2 () y (FAt (Raw yRd 0 lY 8)):yRd=:(Tmp yRd+16):ss++[Wr2F () (Raw td 0 (Just a) 8) (FTmp z), td=:(Tmp td+16)]
        step1=MX () x0 (FAt (Raw xRd 0 lX 8)):xRd=:(Tmp xRd+8):MX () y0 (FAt (Raw yRd 0 lY 8)):yRd=:(Tmp yRd+8):s1++[WrF () (Raw td 0 (Just a) 8) (FTmp z0), td=:(Tmp td+8)]
        loop=r2of tXs i (Tmp nR) step step1
    pure (plEX$plEY$nR=:ev tXs (xR,lX):v8 oSh t a (Tmp nR)++xRd=:DP xR 1:yRd=:DP yR 1:td=:DP t 1:[loop])
aeval (EApp oTy@(Arr sh _) (EApp _ g@(EApp _ (Builtin _ Zip) op) xs) ys) t a | (Arrow tX (Arrow tY tC)) <- eAnn op, Just zSz <- nSz tC, nind tX && nind tY = do
    nR <- nI
    (plEX, (lX, aPX)) <- plA xs; (plEY, (lY, aPY)) <- plA ys
    (aa, contents) <- fill g (AD t (Just a) (Just oTy) Nothing Nothing Nothing) [AI (AD aPX lX Nothing Nothing Nothing (Just$Tmp nR)), AI (AD aPY lY Nothing Nothing Nothing Nothing)]
    pure (plEX$plEY$nR =: ev (eAnn xs) (aPX,lX):vSz sh t a (Tmp nR) zSz++sas aa contents)
aeval (EApp (Arr oSh _) (EApp _ g@(EApp _ (Builtin _ ScanS) op) seed) e) t a | (Arrow tX (Arrow tY _)) <- eAnn op, Just xSz <- rSz tX, nind tY = do
    acc <- rtemp tX; n <- nI
    plS <- eeval seed acc
    (plE, (l, aP)) <- plA e
    (pinch, loop) <- fill g (AD t (Just a) Nothing Nothing Nothing (Just$Tmp n)) [NA acc, AI (AD aP l (Just tXs) Nothing Nothing Nothing)]
    pure (plE$n =: (ev tXs (aP,l)+1):vSz oSh t a (Tmp n) xSz++sas pinch (plS++loop))
  where
    tXs=eAnn e
aeval (EApp (Arr oSh _) (EApp _ g@(EApp _ (Builtin _ ScanS) op) seed) e) t a | (Arrow tX (Arrow tY _)) <- eAnn op, isΠ tX, xSz <- bT tX, nind tY = do
    acc <- nI; n <- nI
    (_, mSz, _, plS) <- πe seed acc
    (plE, (l, aP)) <- plA e
    (pinch, loop) <- fill g (AD t (Just a) Nothing Nothing Nothing (Just$Tmp n)) [ΠA acc, AI (AD aP l (Just tXs) Nothing (Just xSz) Nothing)]
    pure (plE$n =: (ev tXs (aP,l)+1):vSz oSh t a (Tmp n) xSz++m'sa acc mSz++sas pinch (plS++loop)++m'pop mSz)
  where
    tXs=eAnn e
aeval (EApp oTy@(Arr sh _) g@(EApp _ (Builtin _ Scan) op) xs) t a | (Arrow tAcc (Arrow tX _)) <- eAnn op, Just accSz <- rSz tAcc, Just xSz <- rSz tX = do
    acc <- rtemp tAcc; x <- rtemp tX; n <- nI
    (plE, (l, aP)) <- plA xs
    contents <- rfill g (AD t (Just a) (Just oTy) Nothing (Just accSz) (Just$Tmp n)) [AI (AD aP l Nothing Nothing (Just xSz) Nothing), NA acc, NA x]
    pure (plE$n =: ev (eAnn xs) (aP,l):vSz sh t a (Tmp n) accSz++contents)
aeval (EApp oTy@(Arr oSh _) (EApp _ (Builtin _ (DI n)) op) xs) t a | Just (ot, oSz) <- aRr oTy, tXs <- eAnn xs, Just xSz <- aB tXs = do
    szR <- nI; sz'R <- nI; fR <- rtemp ot
    (slopP, aSlop, pops) <- vslop xSz n
    td <- nI
    (_, ss) <- writeF op [AA slopP Nothing] fR
    (plX, (lX, aP)) <- plA xs
    loop <- afor oSh 0 ILt (Tmp sz'R) $ \i ->
              cpy (AElem slopP 1 Nothing 0) (AElem aP 1 lX (Tmp i)) (fromIntegral n) xSz
              :ss++[wt (Raw td 0 (Just a) oSz) fR, td+=KI oSz]
    pure (plX$szR =: ev tXs (aP,lX):sz'R =: (Tmp szR-fromIntegral (n-1)):vSz oSh t a (Tmp sz'R) xSz++aSlop++td=:DP t 1:loop:[pops])
aeval (EApp (Arr oSh _) (EApp _ (Builtin _ (DI n)) op) xs) t a | Just ((_, 1), (tO, cRnk)) <- mAA (eAnn op), Just (tX, 1) <- tRnk tXs = do
    d1x <- nI; d1 <- nI; nC <- nI
    let szX=bT tX; szO=bT tO; oRnk=KI$1+cRnk; neϵ=fromIntegral n
    (plX, (lX, xR)) <- plA xs
    (slopP, aSlop, pops) <- vslop szX n
    slopPd <- nI; xRd <- nI; td <- nI
    (z0R, lZ0, ss0) <- writeA op [AA slopP Nothing]
    (zR, lZ, ss) <- writeA op [AA slopP Nothing]
    (dots, plOds) <- plDim cRnk (z0R, lZ0)
    loop <- afor oSh 0 ILt (Tmp d1) $ \i ->
                aiA slopPd (xRd,lX) (Tmp i) neϵ szX
                :ss++aiR (td,Just a) (zR,lZ,KI cRnk) (Tmp nC) szO
    pure (plX$
        d1x=:ev tXs (xR,lX)
        :d1=:(Tmp d1x-(neϵ-1))
        :aSlop
        ++slopPd=:DP slopP 1:xRd=:DP xR 1
        :aiA slopPd (xRd,lX) 0 neϵ szX:ss0
        ++plOds++PlProd () nC (Tmp<$>dots)
        :Ma () oSh a t oRnk (Tmp d1*Tmp nC) szO
        :zipWith (\j tϵ -> Wr () (ADim t (KI j) (Just a)) (Tmp tϵ)) [0..] (d1:dots)
        ++td=:DP t oRnk:loop
        :[pops])
  where
    tXs=eAnn xs
    -- TODO: array case
aeval (EApp (Arr oSh _) (EApp _ g@(Builtin _ Rot) n) xs) t a | Just sz <- aB tXs = do
    (plN, nR) <- plEV n
    (plX, (lX, xsR)) <- plA xs
    nx <- nI
    contents <- rfill g (AD t (Just a) Nothing Nothing Nothing Nothing) [AI (AD xsR lX Nothing Nothing (Just sz) (Just$Tmp nx)), NA (IT nR)]
    pure (plX$nx =: ev tXs (xsR,lX):vSz oSh t a (Tmp nx) sz++plN contents)
  where
    tXs=eAnn xs
aeval (EApp (Arr oSh _) (EApp _ (Builtin _ Rot) n) xs) t a | Just (tX, xRnk) <- tRnk (eAnn xs), Just sz <- nSz tX = do
    c <- nI; szR <- nI
    (plN, nR) <- plEV n
    (plX, (lX, xR)) <- plA xs
    (dts,dss) <- plDim xRnk (xR,lX)
    let d1=head dts; ns=tail dts
        rnkE=KI xRnk
    pure (plX$plN$dss
        ++PlProd () szR (Tmp<$>ns)
        :Ma () oSh a t rnkE (Tmp d1*Tmp szR) sz
        :CpyD () (ADim t 0 (Just a)) (ADim xR 0 lX) rnkE
        :Ifn't () (IRel IGeq (Tmp nR) 0) [nR+=Tmp d1]
        :c=:(Tmp d1-Tmp nR)
        :[cpy (AElem t rnkE (Just a) 0) (AElem xR rnkE lX (Tmp nR*Tmp szR)) (Tmp c*Tmp szR) sz, cpy (AElem t rnkE (Just a) (Tmp c*Tmp szR)) (AElem xR rnkE lX 0) (Tmp nR*Tmp szR) sz])
                                                         | otherwise = unsupported
aeval (Id (Arr sh at) (AShLit ns es)) t a | Just ty <- nt at, sz <- bT ty = do
    let rnk=genericLength ns; n=fromIntegral$product ns
    tt <- rtemp ty
    plEs <- zipWithM (\eϵ i -> do {pl <- eeval eϵ tt; pure $ pl ++ [wt (AElem t rnk (Just a) (KI i) sz) tt]}) es [0..]
    pure (Ma () sh a t rnk n sz:diml (t, Just a) (fromIntegral<$>ns)++concat plEs)
aeval (EApp _ (Builtin _ T) x) t a | Arr sh ty <- eAnn x, Just rnk <- staRnk sh = do
    let sze=bT ty; dO=KI$8+8*rnk
    xd <- nI; td <- nI
    (plX, (l, xR)) <- plA x
    (dts, plDs) <- plDim rnk (xR, l)
    (sts, plSs) <- offByDim (reverse dts)
    (std, plSd) <- offByDim dts
    let _:sstrides = sts; (_:dstrides) = std
    is <- nIs [1..rnk]
    let loop=thread (zipWith (\i tt -> (:[]) . For () 1 i 0 ILt (Tmp tt)) is dts) [mv (At td (Tmp<$>dstrides) (Tmp<$>reverse is) (Just a)) (At xd (Tmp<$>sstrides) (Tmp<$>is) l) sze]
    pure (plX$plDs++init plSs++Ma () sh a t (KI rnk) (Tmp (head dts)*Tmp (head sstrides)) sze:diml (t, Just a) (Tmp<$>reverse dts)++init plSd++xd =: (Tmp xR+dO):td =: (Tmp t+dO):loop)
                                 | otherwise = unsupported
aeval (EApp (Arr oSh _) (EApp _ g@(EApp _ (Builtin _ Outer) op) xs) ys) t a | (Arrow tX (Arrow tY tC)) <- eAnn op, Just zSz <- nSz tC, nind tX && nind tY = do
    szX <- nI; szY <- nI
    (plX, (lX, xR)) <- plA xs; (plY, (lY, yR)) <- plA ys
    contents <- rfill g (AD t (Just a) Nothing Nothing (Just zSz) Nothing) [AI (AD xR lX (Just tXs) Nothing Nothing (Just$Tmp szX)), AI (AD yR lY (Just tYs) Nothing Nothing (Just$Tmp szY))]
    pure (plX$plY$szX =: ev tXs (xR,lX):szY =: ev tYs (yR,lY):Ma () oSh a t 2 (Tmp szX*Tmp szY) zSz:diml (t, Just a) [Tmp szX, Tmp szY]++contents)
  where
    tXs = eAnn xs; tYs=eAnn ys
aeval (EApp (Arr oSh _) (EApp _ (EApp _ (Builtin _ Outer) op) xs) ys) t a
    | (Arrow tX (Arrow tY tC)) <- eAnn op
    , Arr sh tEC <- tC
    , Just [szXT,szYT,szZT] <- traverse nSz [tX,tY,tEC] = do
    td <- nI; szX <- nI; szY <- nI; szZ <- nI; i <- nI; j <- nI; rnkZ <- nI; rnkO <- nI
    (plX, (lX, xR)) <- plA xs; (plY, (lY, yR)) <- plA ys
    (x, wX, pinchX) <- arg tX (iXelem xR 1 lX szXT)
    (y, wY, pinchY) <- arg tY (iXelem yR 1 lY szYT)
    (z0, lZ0, ss0) <- writeA op [ra x, ra y]
    (z, lZ, ss) <- writeA op [ra x, ra y]
    let step=[wX i, wY j]++ss++aiR (td,Just a) (z,lZ,Tmp rnkZ) (Tmp szZ) szZT
        loop=fort tXs i 0 ILt (Tmp szX) [fort tYs j 0 ILt (Tmp szY) step]
    pure (plX$plY$
        i=:0:j=:0:
        sas [pinchX, pinchY] (
        wX i:wY j:ss0
        ++rnkZ=:eRnk sh (z0,lZ0):rnkO=:(Tmp rnkZ+2)
        :SZ () szZ z0 (Tmp rnkZ) lZ0
        :szX=:ev tXs (xR,lX):szY=:ev tYs (yR,lY)
        :Ma () oSh a t (Tmp rnkO) (Tmp szX*Tmp szY*Tmp szZ) szZT
        :diml (t, Just a) [Tmp szX, Tmp szY]
        ++[CpyD () (ADim t 2 (Just a)) (ADim z0 0 lZ0) (Tmp rnkZ), td=:DP t (Tmp rnkO), loop]
        ))
  where
    tXs=eAnn xs; tYs=eAnn ys
aeval (EApp (Arr oSh _) (EApp _ (EApp _ (Builtin _ Outer) op) xs) ys) t a
    | (Arrow tX (Arrow tY tC)) <- eAnn op
    , Arr xESh tEX <- tX, Arr yESh tEY <- tY, Arr xSh _ <- tXs, Arr ySh _ <- tYs
    , Just [xERnk,yERnk,xRnk,yRnk,oRnk] <- traverse staRnk [xESh,yESh,xSh,ySh,oSh]
    , Just [szXT,szYT,szZ] <- traverse nSz [tEX,tEY,tC] = do
    xd <- nI; yd <- nI; slopXd <- nI; slopYd <- nI
    i <- nI; j <- nI; di <- nI; nX <- nI; nY <- nI
    (plX, (lX, xR)) <- plA xs; (plY, (lY, yR)) <- plA ys
    (slopX,nXe,plSlopX,popSlopX) <- plSlop szXT xERnk (idims xERnk xRnk xR lX)
    (slopY,nYe,plSlopY,popSlopY) <- plSlop szYT yERnk (idims yERnk yRnk yR lY)
    (z, wZ, pinch) <- rW tC (iXelem t (KI oRnk) (Just a) szZ)
    (_, ss) <- writeF op [AA slopX Nothing, AA slopY Nothing] z
    let loop = [ aiA slopXd (xd,lX) (Tmp i) (Tmp nXe) szXT
               , aiA slopYd (yd,lY) (Tmp j) (Tmp nYe) szYT
               ] ++ ss ++ [wZ di, di+=1]
    (dtxs,dxss) <- plDim (xRnk-xERnk) (xR,lX)
    (dtys,dyss) <- plDim (yRnk-yERnk) (yR,lY)
    pure (plX$plY$
         dxss++dyss
        ++PlProd () nX (Tmp<$>dtxs):PlProd () nY (Tmp<$>dtys)
        :Ma () oSh a t (KI oRnk) (Tmp nX*Tmp nY) szZ
        :diml (t, Just a) (Tmp<$>(dtxs++dtys))
        ++plSlopX++plSlopY
        ++slopXd=:DP slopX (KI xERnk):slopYd=:DP slopY (KI yERnk)
        :sas [pinch] [xd=:DP xR (KI xRnk), yd=:DP yR (KI yRnk), di=:0, For () 1 i 0 ILt (Tmp nX) [For () 1 j 0 ILt (Tmp nY) loop]]
        ++[popSlopX,popSlopY])
  where
    tXs=eAnn xs; tYs=eAnn ys
aeval (EApp oTy@(Arr sh _) g@(EApp _ (Builtin _ Succ) op) xs) t a | Arrow tX (Arrow _ tZ) <- eAnn op, Just zSz <- nSz tZ, nind tX = do
    szR <- nI; sz'R <- nI
    (plX, (lX, xR)) <- plA xs
    (pinches, loop) <- fill g (AD t (Just a) (Just oTy) Nothing Nothing (Just$Tmp sz'R)) [AI (AD xR lX Nothing Nothing Nothing Nothing)]
    pure (plX$szR =: ev (eAnn xs) (xR,lX):sz'R =: (Tmp szR-1):vSz sh t a (Tmp sz'R) zSz++sas pinches loop)
aeval (EApp oTy@(Arr oSh _) g@(Builtin _ RevE) e) t a | Just sz <- aB oTy = do
    n <- nI
    (plE, (lE, eR)) <- plA e
    contents <- rfill g (AD t (Just a) (Just oTy) Nothing (Just sz) Nothing) [AI$AD eR lE Nothing Nothing Nothing (Just$Tmp n)]
    pure (plE$n =: ev oTy (eR,lE):vSz oSh t a (Tmp n) sz++contents)
aeval (EApp _ (Builtin _ RevE) e) t a | Arr sh ty <- eAnn e, Just rnk <- staRnk sh = do
    n <- nI; szA <- nI
    (plE, (lE, eR)) <- plA e
    let sz=bT ty; rnkE=KI rnk
    (dts, plDs) <- plDim rnk (eR, lE)
    loop <- afor sh 0 ILt (Tmp n) $ \i -> [cpy (AElem t rnkE (Just a) (Tmp i*Tmp szA)) (AElem eR rnkE lE ((Tmp n-Tmp i-1)*Tmp szA)) (Tmp szA) sz]
    pure (plE$n=:ev ty (eR,lE):tail plDs++PlProd () szA (Tmp<$>tail dts):Ma () sh a t rnkE (Tmp n*Tmp szA) sz:CpyD () (ADim t 0 (Just a)) (ADim eR 0 lE) rnkE:[loop])
                                    | otherwise = unsupported
aeval (EApp (Arr sh _) (EApp _ (EApp _ (Builtin _ Gen) seed) op) n) t a | tyS <- eAnn seed, Just sz <- rSz tyS = do
    acc <- rtemp tyS
    plS <- eeval seed acc
    td <- nI
    (plN, nR) <- plEV n
    ss <- writeRF op [acc] acc
    loop <- arof sh (Tmp nR) $ wt (Raw td 0 (Just a) sz) acc:td+=KI sz:ss
    pure (plN$vSz sh t a (Tmp nR) sz++plS++td=:DP t 1:[loop])
aeval (EApp (Arr sh _) (EApp _ (EApp _ (Builtin _ Gen) seed) op) n) t a | isΠR (eAnn seed) = do
    (plN, nE) <- plC n
    td <- nI; acc <- nI; acc0 <- nI
    (szs,mP,_,plS) <- πe seed acc
    let πsz=last szs
    (_, ss) <- writeF op [IPA acc] (IT acc0)
    loop <- arof sh nE $ Mv () (Raw td 0 (Just a) πsz) (TupM acc Nothing) πsz:td+=KI πsz:ss++[Mv () (TupM acc Nothing) (TupM acc0 Nothing) πsz]
    pure (plN$vSz sh t a nE πsz++m'sa acc mP++m'sa acc0 mP++plS++td=:DP t 1:loop:m'pop mP++m'pop mP)
aeval (EApp (Arr oSh _) (EApp _ (EApp _ (Builtin _ Fib) seed) op) n) t a | Just ty <- aN tSeed, sz <- bT ty = do
    (plN, nE) <- plC n
    (plX, (lX, xR)) <- plA seed; kϵ <- nI
    (y, wRet, pinch) <- rW ty (iXelem t 1 (Just a) sz)
    (_, ss) <- writeF op [AA t (Just a)] y
    loop <- arof oSh nE $ Wr () (ADim t 0 (Just a)) (Tmp kϵ):ss++[wRet kϵ, kϵ+=1]
    pure (plX$kϵ=:ev tSeed (xR,lX):plN (vSz oSh t a (nE+Tmp kϵ) sz++cpy (AElem t 1 (Just a) 0) (AElem xR 1 lX 0) (Tmp kϵ) sz:sas [pinch] [loop]))
  where
    tSeed=eAnn seed
aeval (EApp (Arr oSh _) (EApp _ (Builtin _ (Conv as)) f) x) t a
    | (Arrow _ tC) <- eAnn f
    , Just (tX, xRnk) <- tRnk (eAnn x)
    , Just oRnk <- staRnk oSh
    , Just oSz <- nSz tC, Just xSz <- nSz tX, oRnk==xRnk = do
    xRd <- nI; szR <- nI; slopP <- nI
    (plX, (lX, xR)) <- plA x
    (dts, plDs) <- plDim xRnk (xR, lX)
    (tdims, dims) <- unzip <$> zipWithM (\dt (i,d) -> do {odim <- nI; pure (odim, odim =: (Bin Op.IDiv (Tmp dt-fromIntegral i) (maybe 1 fromIntegral d)+1))}) dts as
    (tb,bs) <- unzip <$> zipWithM (\dt i -> do {b <- nI; pure (b, b =: (Tmp dt-fromIntegral(i-1)))}) dts (fst<$>as)
    io <- nIs tdims; iw <- nIs is
    let slopSz=product isi; slopRnk=length isi; slopE=fromIntegral (slopSz*fromIntegral oSz+(slopRnk+1)*8)
        rnk=KI oRnk
    z <- rtemp tC; o <- rtemp tX
    (_, ss) <- writeF f [AA slopP Nothing] z
    (sts, plS) <- offByDim (reverse dts)
    let _:strides = sts; sss=init plS
    extrWindow <- aall1 iw is $ \j ->
                            [mt (At xRd (Tmp<$>strides) (zipWith (\jϵ iϵ -> Tmp jϵ+Tmp iϵ) iw io) lX xSz) o, wt (AElem slopP (KI$fromIntegral slopRnk) Nothing (Tmp j) oSz) o]
    loop <- aall io ds (Tmp<$>tb) $ \k -> extrWindow++ss++[wt (AElem t rnk (Just a) (Tmp k) oSz) z]
    pure (plX$
        plDs
        ++dims
        ++sss
        ++PlProd () szR (Tmp<$>tdims):Ma () oSh a t rnk (Tmp szR) oSz:diml (t, Just a) (Tmp<$>tdims)
        ++sac slopP slopE:Wr () (ARnk slopP Nothing) (KI$fromIntegral slopRnk):diml (slopP, Nothing) is
        ++xRd=:DP xR (KI xRnk):bs++loop
        ++[popc slopE])
    where (isi,dsi)=unzip as; is=fromIntegral<$>isi; ds=maybe 1 fromIntegral<$>dsi
aeval e _ _ = error (show e)

plR :: E (T ()) -> CM ([CS ()] -> [CS ()], RT)
plR e = case eAnn e of
    I -> second IT <$> plEV e
    F -> second FT <$> plF e
    B -> second PT <$> plBV e

plC :: E (T ()) -> CM ([CS ()] -> [CS ()], CE)
plC (ILit _ i) = pure (id, KI$fromIntegral i)
plC e          = second Tmp <$> plEV e

plD2 :: E (T ()) -> CM ([CS ()] -> [CS ()], F2Temp)
plD2 (Var F x) = do {tϵ <- gets (getT2 x); case tϵ of {Right t2 -> pure (id, t2); Left t1 -> do {t <- nF2; pure ((DS () t t1:), t)}}}
plD2 e         = do {t <- nF2; pl <- f2eval e t; pure ((pl++), t)}

plD :: E (T ()) -> CM ([CS ()] -> [CS ()], F1E)
plD (FLit _ x) = pure (id, ConstF x)
plD e          = second FTmp <$> plF e

plP :: E (T ()) -> CM ([CS ()] -> [CS ()], PE)
plP (BLit _ b) = pure (id, BConst b)
plP e          = second Is <$> plBV e

plBV :: E (T ()) -> CM ([CS ()] -> [CS ()], BTemp)
plBV (Var B x) = do {st <- gets pvars; pure (id, getT st x)}
plBV e         = do {t <- nBT; pl <- peval e t; pure ((pl++), t)}

plEV :: E (T ()) -> CM ([CS ()] -> [CS ()], Temp)
plEV (Var I x) = do {st <- gets vars; pure (id, getT st x)}
plEV e         = do {t <- nI; pl <- eval e t; pure ((pl++), t)}

plF :: E (T ()) -> CM ([CS ()] -> [CS ()], FTemp)
plF (Var F x) = do {st <- gets dvars; pure (id, getT st x)}
plF e         = do {t <- nF; pl <- feval e t; pure ((pl++), t)}

plA :: E (T ()) -> CM ([CS ()] -> [CS ()], (Maybe AL, Temp))
plA (Var _ x) = do {st <- gets avars; pure (id, getT st x)}
plA e         = do {(t,lX,plX) <- maa e; pure ((plX++), (lX, t))}

peval :: E (T ()) -> BTemp -> CM [CS ()]
peval (LLet _ b e) t = do
    ss <- llet b
    (ss++) <$> peval e t
peval (BLit _ b) t = pure [MB () t (BConst b)]
peval (EApp _ (EApp _ (Builtin _ A1) e) i) t = do
    (plE, (lE, eR)) <- plA e
    (plI,iE) <- plC i
    pure $ plE $ plI [MB () t (PAt (AElem eR 1 lE iE 8))]
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
peval (EApp _ (EApp _ (Builtin _ Fold) op) e) acc | tXs@(Arr xSh _) <- eAnn e, (Arrow tX _) <- eAnn op, isB tX = do
    x <- nBT; szR <- nI
    (plE, (l, aP)) <- plA e
    ss <- writeRF op [PT acc, PT x] (PT acc)
    loop <- afor1 xSh 1 ILt (Tmp szR) (\i -> MB () x (PAt (AElem aP 1 l (Tmp i) 1)):ss)
    pure $ plE$szR =: ev tXs (aP,l):MB () acc (PAt (AElem aP 1 l 0 1)):[loop]
peval (EApp _ (EApp _ (EApp _ (Builtin _ FoldS) op) seed) e) acc | (Arrow _ (Arrow tY _)) <- eAnn op, Just szY <- nSz tY = do
    szR <- nI
    (plE, (l, aP)) <- plA e
    plAcc <- peval seed acc
    (x, wX, pinch) <- arg tY (iXelem aP 1 l szY)
    ss <- writeRF op [PT acc, x] (PT acc)
    loop <- afort tXs 0 ILt (Tmp szR) (\i -> wX i:ss)
    pure $ plE $ plAcc++szR=:ev (eAnn e) (aP,l):m'p pinch [loop]
  where
    tXs=eAnn e
peval (Id _ (U2 seeds gs c f n)) t | Just e <- traverse (rr.eAnn) seeds = do
    plU <- peval c t
    (plN,nE) <- plC n
    k <- nI
    xs <- traverse (rtemp.fst) e
    plSeeds <- concat <$> zipWithM eeval seeds xs
    usss <- concat <$> zipWithM (\g x -> writeRF g [x] x) gs xs
    fss <- writeRF f (PT t:xs) (PT t)
    pure $ plU ++ plN (plSeeds ++ [For () 1 k 0 ILt nE (fss++usss)])
peval (EApp _ (Builtin _ Head) xs) t = do
    (plX, (l, a)) <- plA xs
    pure $ plX [MB () t (PAt (AElem a 1 l 0 1))]
peval (EApp _ (Builtin _ Last) xs) t = do
    (plX, (l, a)) <- plA xs
    pure $ plX [MB () t (PAt (AElem a 1 l (ev (eAnn xs) (a,l)-1) 1))]
peval (EApp _ (Builtin _ (TAt i)) e) t = do
    k <- nI
    (offs, a, _, plT) <- πe e k
    pure $ m'sa k a++plT ++ MB () t (PAt (Raw k (KI$offs!!(i-1)) Nothing 1)):m'pop a
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
eval (EApp _ (EApp _ (Builtin _ Fold) op) e) acc | (Arr sh _) <- eAnn e, (Arrow I _) <- eAnn op = do
    x <- nI; szR <- nI
    (plE, (l, aP)) <- plA e
    ss <- writeRF op [IT acc, IT x] (IT acc)
    loop <- afor1 sh 1 ILt (Tmp szR) (\i -> x=:EAt (AElem aP 1 l (Tmp i) 8):ss)
    pure $ plE$szR =: ev tXs (aP,l):acc =: EAt (AElem aP 1 l 0 8):[loop]
  where
    tXs=eAnn e
eval (EApp _ (EApp _ (EApp _ (Builtin _ FoldS) op) seed) e) acc | (Arrow _ (Arrow tX _)) <- eAnn op, Just xSz <- nSz tX, tArr <- eAnn e = do
    szR <- nI
    (plE, (l, eR)) <- plA e
    plAcc <- eval seed acc
    (x, wX, pinch) <- arg tX (iXelem eR 1 l xSz)
    ss <- writeRF op [IT acc, x] (IT acc)
    loop <- afort tArr 0 ILt (Tmp szR) (\i -> wX i:ss)
    pure $ plE$plAcc++szR =: ev tArr (eR,l):m'p pinch [loop]
eval (EApp _ (EApp _ (EApp _ (Builtin _ FoldA) op) seed) xs) acc | tXs@(Arr sh _) <- eAnn xs, (Arrow _ (Arrow I _)) <- eAnn op = do
    x <- nI
    rnkR <- nI; szR <- nI; k <- nI
    (plE, (lX, xsR)) <- plA xs
    plAcc <- eval seed acc
    ss <- writeRF op [IT x, IT acc] (IT acc)
    xsRd <- nI
    let step=MT () x (EAt (Raw xsRd (Tmp k) lX 8)):ss
        loop=for sh k 0 ILt (Tmp szR) step
        plSz = case tIx tXs of {Just (_, is) -> szR=:KI (product is); Nothing -> SZ () szR xsR (Tmp rnkR) lX}
    pure $ plE $ plAcc ++ [rnkR =: eRnk sh (xsR, lX), plSz, xsRd=:DP xsR (Tmp rnkR), loop]
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
    pure $ plE $ plI [t =: EAt (AElem eR 1 lE iE 8)]
eval (EApp _ (Builtin _ Head) xs) t = do
    (plX, (l, a)) <- plA xs
    pure $ plX [t =: EAt (AElem a 1 l 0 8)]
eval (EApp _ (Builtin _ Last) xs) t = do
    (plX, (l, a)) <- plA xs
    pure $ plX [t =: EAt (AElem a 1 l (ev (eAnn xs) (a,l)-1) 8)]
eval (EApp _ (Builtin _ Size) xs) t | Just (_, 1) <- tRnk (eAnn xs) = do
    (plE, (l, xsR)) <- plA xs
    pure $ plE [t =: EAt (ADim xsR 0 l)]
eval (EApp _ (Builtin _ Dim) xs) t | Arr (Ix _ i `Cons` _) _ <- eAnn xs = do
    pure [t=:KI (fromIntegral i)]
eval (EApp _ (Builtin _ Dim) xs) t = do
    (plE, (l, xsR)) <- plA xs
    pure $ plE [t =: EAt (ADim xsR 0 l)]
eval (EApp _ (Builtin _ Size) xs) t | Arr sh _ <- eAnn xs = do
    (plE, (l, xsR)) <- plA xs
    rnkR <- nI
    pure $ plE [rnkR =: eRnk sh (xsR,l), SZ () t xsR (Tmp rnkR) l]
eval (EApp _ (Builtin _ Size) xs) t | nind (eAnn xs) = pure [t=:1]
eval (EApp _ (EApp _ (Builtin _ IntExp) (FLit _ (-1))) n) t = do
    (plRϵ,nR) <- plEV n
    pure $ plRϵ [t=:1, Cmov () (IUn IOdd (Tmp nR)) t (KI (-1))]
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
    pure $ m'sa k a++plT ++ t =: EAt (Raw k (KI$offs!!(i-1)) Nothing 1):m'pop a
eval (EApp _ (EApp _ (Builtin _ IOf) p) xs) t | (Arrow tD _) <- eAnn p, Just szX <- nSz tD = do
    pR <- nBT
    szR <- nI; i <- nI; done <- nI
    (plX, (lX, xsR)) <- plA xs
    (x, wX, pinch) <- arg tD (iXelem xsR 1 lX szX)
    ss <- writeRF p [x] (PT pR)
    let loop=While () done INeq 1 (wX i:ss++[If () (Is pR) [t=:Tmp i, done=:1] [], i+=1, Cmov () (IRel IGeq (Tmp i) (Tmp szR)) done 1])
    pure $ plX $ szR=:ev (eAnn xs) (xsR,lX):t=:(-1):done=:0:i=:0:m'p pinch [loop]
eval (EApp _ (EApp _ (EApp _ (Builtin _ Iter) f) n) x) t = do
    (plN,nR) <- plC n
    plX <- eval x t
    ss <- writeRF f [IT t] (IT t)
    i <- nI
    let loop=For () 1 i 0 ILt nR ss
    pure $ plX++plN [loop]
eval (Cond _ p e0 e1) t = cond p e0 e1 (IT t)
eval (Id _ (FoldOfZip zop op [p])) acc | tPs@(Arr sh _) <- eAnn p, Just (tP, pSz) <- aRr (eAnn p) = do
    x <- rtemp tP; szR <- nI
    (plPP, (lP, pR)) <- plA p
    ss <- writeRF op [IT acc, x] (IT acc)
    loop <- afor1 sh 1 ILt (Tmp szR) (\i -> mt (AElem  pR 1 lP (Tmp i) pSz) x:ss)
    sseed <- writeRF zop [x] (IT acc)
    pure $ plPP$szR =:ev tPs (pR,lP):mt (AElem pR 1 lP 0 pSz) x:sseed++[loop]
eval (Id _ (FoldOfZip zop op [p, q])) acc | tPs@(Arr sh _) <- eAnn p, Just (tP, pSz) <- aRr tPs, Just (tQ, qSz) <- aRr (eAnn q) = do
    x <- rtemp tP; y <- rtemp tQ; szR <- nI
    (plPP, (lP, pR)) <- plA p; (plQ, (lQ, qR)) <- plA q
    ss <- writeRF op [IT acc, x, y] (IT acc)
    loop <- afor1 sh 1 ILt (Tmp szR) (\i -> mt (AElem pR 1 lP (Tmp i) pSz) x:mt (AElem qR 1 lQ (Tmp i) qSz) y:ss)
    seed <- writeRF zop [x,y] (IT acc)
    pure $ plPP$plQ$szR =: ev tPs (pR,lP):mt (AElem pR 1 lP 0 pSz) x:mt (AElem qR 1 lQ 0 qSz) y:seed++[loop]
eval (Id _ (U2 seeds gs c f n)) t | Just e <- traverse (rr.eAnn) seeds = do
    plU <- eval c t
    (plN,nE) <- plC n
    k <- nI
    xs <- traverse (rtemp.fst) e
    plSeeds <- concat <$> zipWithM eeval seeds xs
    usss <- concat <$> zipWithM (\g x -> writeRF g [x] x) gs xs
    fss <- writeRF f (IT t:xs) (IT t)
    pure $ plU ++ plN (plSeeds ++ [For () 1 k 0 ILt nE (fss++usss)])
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

cond :: E (T ()) -> E (T ()) -> E (T ()) -> RT -> CM [CS ()]
cond (EApp _ (EApp _ (Builtin (Arrow F _) op) c0) c1) e e1 (FT t) | Just cmp <- frel op, Just cfe <- mFEval e1 = do
    c0R <- nF; c1R <- nF
    plC0 <- feval c0 c0R; plC1 <- feval c1 c1R
    eR <- nF; fe <- cfe
    plE <- feval e eR
    pure (plC0 ++ plC1 ++ [MX () t fe] ++ plE ++ [Fcmov () (FRel cmp (FTmp c0R) (FTmp c1R)) t (FTmp eR)])
cond (EApp _ (EApp _ (Builtin (Arrow F _) o) c0) c1) e0 e1 t | Just f <- frel o, isIF (eAnn e0) = do
    c0R <- nF; c1R <- nF
    plC0 <- feval c0 c0R; plC1 <- feval c1 c1R
    plE0 <- eeval e0 t; plE1 <- eeval e1 t
    pure (plC0 ++ plC1 ++ [If () (FRel f (FTmp c0R) (FTmp c1R)) plE0 plE1])
cond (EApp _ (EApp _ (Builtin (Arrow I _) op) c0) c1) e e1 (FT t) | Just cmp <- rel op, Just cfe <- mFEval e1 = do
    c0R <- nI
    plC0 <- eval c0 c0R
    (plC1,c1e) <- plC c1
    eR <- nF; fe <- cfe
    plE <- feval e eR
    pure (plC0 ++ plC1 ([MX () t fe] ++ plE ++ [Fcmov () (IRel cmp (Tmp c0R) c1e) t (FTmp eR)]))
cond (EApp _ (EApp _ (Builtin (Arrow I _) op) c0) c1) e0 e1 t | Just cmp <- rel op, isIF (eAnn e0) = do
    c0R <- nI; c1R <- nI
    plC0 <- eval c0 c0R; plC1 <- eval c1 c1R
    plE0 <- eeval e0 t; plE1 <- eeval e1 t
    pure (plC0 ++ plC1 ++ [If () (IRel cmp (Tmp c0R) (Tmp c1R)) plE0 plE1])
cond p e0 e1 t | isIF (eAnn e0) = do
    pR <- nBT
    plPP <- peval p pR; plE0 <- eeval e0 t; plE1 <- eeval e1 t
    pure (plPP ++ [If () (Is pR) plE0 plE1])

f2eval :: E (T ()) -> F2Temp -> CM [CS ()]
f2eval (LLet _ b e) t = do
    ss <- llet b
    (ss++) <$> f2eval e t
f2eval (Var _ x) t = do {tϵ <- gets (getT2 x); pure $ case tϵ of Right t2 -> [MX2 () t (FTmp t2)]; Left t1 -> [DS () t t1]}
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
feval (EApp _ (EApp _ (Builtin _ Plus) (EApp _ (EApp _ (Builtin _ Times) e0) e1)) e2) t = do
    (pl0,t0) <- plF e0; (pl1,t1) <- plF e1; (pl2,t2) <- plF e2
    pure $ pl0 $ pl1 $ pl2 [MX () t (FTmp t0*FTmp t1+FTmp t2)]
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
    (plRϵ,nR) <- plEV n
    pure $ plRϵ [MX () t 1, Fcmov () (IUn IOdd (Tmp nR)) t (ConstF (-1))]
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
feval (Cond _ p e0 e1) t = cond p e0 e1 (FT t)
feval (EApp _ (Builtin _ Head) xs) t = do
    (plX, (l, a)) <- plA xs
    pure $ plX [MX () t (FAt (AElem a 1 l 0 8))]
feval (EApp _ (Builtin _ T) x) t = feval x t
feval (EApp _ (Builtin _ Flat) x) t = feval x t
feval (EApp _ (EApp _ (Builtin _ A1) e) i) t = do
    (plE, (lE, eR)) <- plA e; (plI, iR) <- plC i
    pure $ plE $ plI [MX () t (FAt (AElem eR 1 lE iR 8))]
feval (EApp _ (Builtin _ Last) xs) t = do
    (plX, (l, a)) <- plA xs
    pure $ plX [MX () t (FAt (AElem a 1 l (ev (eAnn xs) (a,l)-1) 8))]
feval (Id _ (FoldOfZip zop op [EApp _ (EApp _ (EApp _ (Builtin _ Gen) seed) g) n, ys])) acc
    | (Arr ySh tY) <- eAnn ys, Just (tQ, qSz) <- rr tY, isR (eAnn seed) = do
    (plN,nE) <- plC n; (plU,x) <- plR seed
    (plYs, (lY, yR)) <- plA ys
    (plY,y) <- plR (EApp tQ (Builtin undefined Head) ys)
    yRd <- nI
    plSeed <- writeRF zop [x, y] (FT acc)
    ss <- writeRF op [FT acc, x, y] (FT acc)
    gs <- writeRF g [x] x
    ll <- arof1 ySh nE $ yRd+=KI qSz:mt (Raw yRd 0 lY qSz) y:gs++ss
    pure $ plYs $ plY $ plU plSeed ++ plN [yRd=:DP yR 1, ll]
feval (Id _ (FoldOfZip zop op [p, q])) acc | tyP@(Arr _ F) <- eAnn p, Arr _ F <- eAnn q, Just (c0,_) <- fz op, hasS op, Just vseed <- fc c0 = do
    acc0 <- nF; acc2 <- nF2; x <- nF2; y <- nF2; x0 <- nF; y0 <- nF
    i <- nI; szR <- nI
    (plPP, (lP, pR)) <- plA p; (plQ, (lQ, qR)) <- plA q
    pD <- nI; qD <- nI
    ss1 <- writeRF op (FT<$>[acc0,x0,y0]) (FT acc0)
    ss <- write2 op [acc2, x, y] acc2
    seed <- writeRF zop (FT<$>[x0,y0]) (FT acc0)
    let step1 = MX () x0 (FAt (Raw pD 0 lP 8)):pD=:(Tmp pD+8):MX () y0 (FAt (Raw qD 0 lQ 8)):qD=:(Tmp qD+8):ss1
        step = MX2 () x (FAt (Raw pD 0 lP 8)):pD=:(Tmp pD+16):MX2 () y (FAt (Raw qD 0 lQ 8)):qD=:(Tmp qD+16):ss
        loop = r21 tyP i (Tmp szR) step step1
    pure $ plPP$plQ$szR=:ev tyP (pR,lP):pD=:DP pR 1:MX () x0 (FAt (Raw pD 0 lP 8)):pD=:(Tmp pD+8):qD=:DP qR 1:MX () y0 (FAt (Raw qD 0 lQ 8)):qD=:(Tmp qD+8):seed++[szR=:(Tmp szR-1), vseed acc acc2, loop, Comb () c0 acc acc2, MX () acc (FTmp acc+FTmp acc0)]
  where
    fz (Lam _ _ (Lam _ _ (Lam _ _ (EApp _ (EApp _ (Builtin _ b0) _) (EApp _ (EApp _ (Builtin _ b1) _) _))))) | fS b0, fS b1 = (,) <$> mFop b0 <*> mFop b1
    fz _ = Nothing
feval (Id _ (FoldOfZip zop op (p:qs))) acc
    | tPs@(Arr pSh _) <- eAnn p
    , Just (tP, pSz) <- aRr tPs
    , Just (tQs, qSzs) <- unzip<$>traverse (aRr.eAnn) qs = do
    x <- rtemp tP; ys <- traverse rtemp tQs; nR <- nI
    (plPP, (lP, pR)) <- plA p; (plQs, aQs) <- first thread.unzip <$> traverse plA qs
    ss <- writeRF op (FT acc:x:ys) (FT acc)
    let mQs at = [mt (AElem qR 1 lQ at qSz) y | (y, (lQ, qR), qSz) <- zip3 ys aQs qSzs]
    loop <- afor1 pSh 1 ILt (Tmp nR) (\i -> mt (AElem pR 1 lP (Tmp i) pSz) x:mQs (Tmp i)++ss)
    seed <- writeRF zop (x:ys) (FT acc)
    pure $plPP$plQs$nR =: ev tPs (pR,lP):mt (AElem pR 1 lP 0 pSz) x:mQs 0++seed++[loop]
feval (Id _ (FoldSOfZip seed op (p:qs))) acc
    | tPs@(Arr pSh _) <- eAnn p
    , Just (tP, pSz) <- aRr tPs
    , Just (tQs, qSzs) <- unzip<$>traverse (aRr.eAnn) qs = do
    x <- rtemp tP; ys <- traverse rtemp tQs; nR <- nI
    plSeed <- feval seed acc
    (plPP, (lP, pR)) <- plA p; (plQs, aQs) <- first thread.unzip <$>traverse plA qs
    ss <- writeRF op (FT acc:x:ys) (FT acc)
    loop <- afor pSh 0 ILt (Tmp nR) (\i -> mt (AElem pR 1 lP (Tmp i) pSz) x:[mt (AElem qR 1 lQ (Tmp i) qSz) y | (y, (lQ, qR), qSz) <- zip3 ys aQs qSzs]++ss)
    pure $ plPP$plQs$nR=:ev tPs (pR,lP):plSeed++[loop]
feval (EApp _ (EApp _ (Builtin _ Fold) op) e) acc | tXs <- eAnn e, Just c <- fca op, Just vseed <- fc c = do
    x0 <- nF; acc0 <- nF; acc2 <- nF2; x <- nF2
    i <- nI; szR <- nI
    (plX, (lX, xR)) <- plA e
    ss1 <- writeRF op [FT acc, FT x0] (FT acc)
    ss <- write2 op [acc2, x] acc2
    let loop = f21o tXs i 1 ILt (Tmp szR) (MX2 () x (FAt (AElem xR 1 lX (Tmp i) 8)):ss) (MX () x0 (FAt (AElem xR 1 lX (Tmp i) 8)):ss1)
    pure $ plX$szR=:ev tXs (xR,lX):MX () acc (FAt (AElem xR 1 lX 0 8)):vseed acc acc2:[loop, Comb () c acc0 acc2, MX () acc (FBin c (FTmp acc) (FTmp acc0))]
  where
    fca (Lam _ _ (Lam _ _ (EApp _ (EApp _ (Builtin _ b) _) _))) | fS b = mFop b; fca _ = Nothing
feval (EApp _ (EApp _ (Builtin _ Fold) op) e) acc | tXs@(Arr xSh _) <- eAnn e = do
    x <- nF; szR <- nI
    (plE, (l, aP)) <- plA e
    ss <- writeRF op [FT acc, FT x] (FT acc)
    loop <- afor1 xSh 1 ILt (Tmp szR) (\i -> MX () x (FAt (AElem aP 1 l (Tmp i) 8)):ss)
    pure $ plE$szR =: ev tXs (aP,l):MX () acc (FAt (AElem aP 1 l 0 8)):[loop]
feval (EApp _ (EApp _ (EApp _ (Builtin _ Foldl) op) seed) e) acc | (Arrow _ (Arrow tX _)) <- eAnn op, isIF tX = do
    x <- rtemp tX
    i <- nI
    (plE, (l, eR)) <- plA e
    plAcc <- feval seed acc
    ss <- writeRF op [x, FT acc] (FT acc)
    let loopBody=mt (AElem eR 1 l (Tmp i) 8) x:ss++[i =: (Tmp i-1)]
        loop=While () i IGeq 0 loopBody
    pure $ plE $ plAcc++i =: (ev (eAnn e) (eR,l)-1):[loop]
feval (EApp _ (EApp _ (EApp _ (Builtin _ FoldA) op) seed) xs) acc | tXs@(Arr sh _) <- eAnn xs, (Arrow _ (Arrow F _)) <- eAnn op = do
    x <- nF
    rnkR <- nI; szR <- nI; k <- nI
    (plE, (lX, xsR)) <- plA xs
    plAcc <- feval seed acc
    ss <- writeRF op [FT x, FT acc] (FT acc)
    xsRd <- nI
    let step=MX () x (FAt (Raw xsRd (Tmp k) lX 8)):ss
        loop=for sh k 0 ILt (Tmp szR) step
        plSz = case tIx tXs of {Just (_, is) -> szR=:KI (product is); Nothing -> SZ () szR xsR (Tmp rnkR) lX}
    pure $ plE $ plAcc ++ [rnkR =: eRnk sh (xsR, lX), plSz, xsRd=:DP xsR (Tmp rnkR), loop]
feval (EApp _ (EApp _ (EApp _ (Builtin _ FoldS) op) seed) e) acc | (Arrow _ (Arrow tX _)) <- eAnn op, Just xSz <- nSz tX = do
    szR <- nI
    (plE, (l, eR)) <- plA e
    plAcc <- feval seed acc
    (x, wX, pinch) <- arg tX (iXelem eR 1 l xSz)
    ss <- writeRF op [FT acc, x] (FT acc)
    loop <- afort tArr 0 ILt (Tmp szR) (\i -> wX i:ss)
    pure $ plE $ plAcc++szR =: ev tArr (eR,l):m'p pinch [loop]
  where
    tArr=eAnn e
feval (EApp _ (EApp _ (EApp _ (Builtin _ Iter) f) n) x) t = do
    (plN,nR) <- plC n
    plX <- feval x t
    ss <- writeRF f [FT t] (FT t)
    i <- nI
    let loop=For () 1 i 0 ILt nR ss
    pure $ plX ++ plN [loop]
feval (EApp _ (Builtin _ (TAt i)) e) t = do
    k <- nI
    (offs, a, _, plT) <- πe e k
    pure $ m'sa k a++plT ++ MX () t (FAt (Raw k (KI$offs!!(i-1)) Nothing 1)):m'pop a
feval (EApp _ (Var _ f) x) t | isR (eAnn x) = do
    st <- gets fvars
    let (l, [a], FT r) = getT st f
    plX <- eeval x (art a)
    retL <- neL
    pure $ plX ++ [G () l retL, MX () t (FTmp r)]
feval (Id _ (U2 seeds gs c f n)) t | Just e <- traverse (rr.eAnn) seeds = do
    plU <- feval c t
    (plN,nE) <- plC n
    k <- nI
    xs <- traverse (rtemp.fst) e
    plSeeds <- concat <$> zipWithM eeval seeds xs
    usss <- concat <$> zipWithM (\g x -> writeRF g [x] x) gs xs
    fss <- writeRF f (FT t:xs) (FT t)
    pure (plU ++ plSeeds ++ plN [Rof () k nE (fss++usss)])
feval (Id _ (FoldGen seed g f n)) t = do
    x <- nF; acc <- nF
    k <- nI
    (plSeed,seedR) <- plF seed
    (plN,nE) <- plC n
    uss <- writeRF g [FT x] (FT x)
    fss <- writeRF f [FT acc, FT x] (FT acc)
    pure $ plSeed $ plN [MX () acc (FTmp seedR), MX () x (FTmp seedR), Rof () k nE (fss++uss), MX () t (FTmp acc)]
feval e _ = error (show e)

sac t = Sa8 () t.KI
popc = Pop8().KI

sa sz | sz `rem` 8 == 0 = Sa8 () | otherwise = Sa ()
pop sz | sz `rem` 8 == 0 = Pop8 () | otherwise = Pop ()

m'pop = maybe [] ((:[]).popc)
m'sa t = maybe [] ((:[]).sac t)

πe :: E (T ()) -> Temp -> CM ([Int64], Maybe Int64, [AL], [CS ()]) -- element offsets, size to be popped off the stack, array labels kept live
πe (EApp (P tys) (Builtin _ Head) xs) t | offs <- szT tys, sz <- last offs = do
    (plX, (lX, xR)) <- plA xs
    pure (offs, Just sz, [], plX [Mv () (TupM t Nothing) (AElem xR 1 lX 0 sz) sz])
πe (EApp (P tys) (Builtin _ Last) xs) t | offs <- szT tys, sz <- last offs = do
    (plX, (lX, xR)) <- plA xs
    pure (offs, Just sz, [], plX [Mv () (TupM t Nothing) (AElem xR 1 lX (ev (eAnn xs) (xR,lX)-1) sz) sz])
πe (Tup (P tys) es) t | offs <- szT tys, sz <- last offs = do
    (ls, ss) <- unzip <$>
        zipWithM (\e off ->
            case eAnn e of
                F     -> do {(plX, f) <- plD e; pure (Nothing, plX [WrF () (Raw t (KI off) Nothing 1) f])}
                I     -> do {(plX, i) <- plC e; pure (Nothing, plX [Wr () (Raw t (KI off) Nothing 1) i])}
                B     -> do {(plX, r) <- plP e; pure (Nothing, plX [WrP () (Raw t (KI off) Nothing 1) r])}
                Arr{} -> do {(pl, (l, r)) <- plA e; pure (l, pl [Wr () (Raw t (KI off) Nothing 1) (Tmp r)])}) es offs
    pure (offs, Just sz, catMaybes ls, concat ss)
πe (EApp (P tys) (EApp _ (Builtin _ A1) e) i) t | offs <- szT tys, sz <- last offs = do
    (plI, iR) <- plEV i; (plX, (lX, xR)) <- plA e
    pure (offs, Just sz, mempty, plX $ plI [Mv () (TupM t Nothing) (AElem xR 1 lX (Tmp iR) sz) sz])
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
    let loop=For () 1 i 0 ILt nR (ss++[Mv () (TupM ttemp Nothing) (TupM t Nothing) sz, Mv () (TupM pre Nothing) (TupM ttemp Nothing) sz])
    pure (offs, Just sz, [], m'sa pre mSz++plX++plN [sac ttemp sz, loop, popc sz]++m'pop mSz)
πe e _ = error (show e)

unsupported = error "Requires statically known rank."

fourth f ~(x,y,z,w) = (x,y,z,f w)

qmap f g h k ~(x,y,z,w) = (f x, g y, h z, k w)
