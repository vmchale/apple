module C.Trans ( writeC ) where

import           A
import           A.S
import           B
import           C
import           CF.AL                            (AL (..))
import qualified CF.AL                            as AL
import           Control.Composition              (thread, (-$), (.*))
import           Control.Monad                    (zipWithM)
import           Control.Monad.Trans.State.Strict (State, gets, modify, runState, state)
import           Data.Bifunctor                   (bimap, first, second)
import           Data.Functor                     (($>))
import           Data.Int                         (Int64)
import qualified Data.IntMap                      as IM
import qualified Data.IntSet                      as IS
import           Data.List                        (find, genericLength)
import           Data.Maybe                       (mapMaybe)
import           Data.Word                        (Word8)
import           E
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
               , πvars       :: IM.IntMap TStore
               , avars       :: IM.IntMap (Maybe AL, Temp)
               , fvars       :: IM.IntMap (Label, [TT], RT)
               , _aa         :: AsmData
               , mts         :: IM.IntMap Temp
               }

nextI = state (\(CSt tϵ ar as l v b d d2 π a f aas ts) -> (tϵ, CSt (tϵ+1) ar as l v b d d2 π a f aas ts))
nextAA = state (\(CSt t ar as l v b d d2 π a f aas ts) -> (as, CSt t ar (as+1) l v b d d2 π a f aas ts))
nextArr r = state (\(CSt t a@(AL i) as l v b d d2 π aϵ f aas ts) -> (a, CSt t (AL$i+1) as l v b d d2 π aϵ f aas (AL.insert a r ts)))
neL = state (\(CSt t ar as l v b d d2 π a f aas ts) -> (l, CSt t ar as (l+1) v b d d2 π a f aas ts))

nI = ITemp <$> nextI; nBT = BTemp <$> nextI
nF = FTemp <$> nextI; nF2 = F2Temp <$> nextI

nIs = traverse (\_ -> nI); nFs = traverse (\_ -> nF); nF2s = traverse (\_ -> nF2)
frts = traverse frt where frt TI{}=TI<$>nI; frt TF{}=TF<$>nF; frt TB{}=TB<$>nBT; frt (TΠ ts)=TΠ<$>traverse frt ts

addAA i aa = modify (\(CSt t ar as l v b d d2 π a f aas ts) -> CSt t ar as l v b d d2 π a f (IM.insert i aa aas) ts)
addVar n r = modify (\(CSt t ar as l v b d d2 π a f aas ts) -> CSt t ar as l (insert n r v) b d d2 π a f aas ts)
addD n r = modify (\(CSt t ar as l v b d d2 π a f aas ts) -> CSt t ar as l v b (insert n r d) d2 π a f aas ts)
addD2 n r = modify (\(CSt t ar as l v b d d2 π a f aas ts) -> CSt t ar as l v b d (insert n r d2) π a f aas ts)
addB n r = modify (\(CSt t ar as l v b d d2 π a f aas ts) -> CSt t ar as l v (insert n r b) d d2 π a f aas ts)
addΠ n r = modify (\(CSt t ar as l v b d d2 π a f aas ts) -> CSt t ar as l v b d d2 (insert n r π) a f aas ts)
addAVar n r = modify (\(CSt t ar as l v b d d2 π a f aas ts) -> CSt t ar as l v b d d2 π (insert n r a) f aas ts)
addF n f = modify (\(CSt t ar as l v b d d2 π a fs aas ts) -> CSt t ar as l v b d d2 π a (insert n f fs) aas ts)

bI n = state (\(CSt t ar as l v b d d2 π a f aas ts) -> let r=ITemp t in (r, CSt (t+1) ar as l (insert n r v) b d d2 π a f aas ts))
bD n = state (\(CSt t ar as l v b d d2 π a f aas ts) -> let r=FTemp t in (r, CSt (t+1) ar as l v b (insert n r d) d2 π a f aas ts))
bD2 n = state (\(CSt t ar as l v b d d2 π a f aas ts) -> let r=F2Temp t in (r, CSt (t+1) ar as l v b d (insert n r d2) π a f aas ts))
bB n = state (\(CSt t ar as l v b d d2 π a f aas ts) -> let r=BTemp t in (r, CSt (t+1) ar as l v (insert n r b) d d2 π a f aas ts))
bp n e = do {r <- πts e; addΠ n r $> r}

getT2 :: Nm a -> CSt -> Either FTemp F2Temp
getT2 n (CSt _ _ _ _ _ _ d d2 _ _ _ _ _) = case Nm.lookup n d2 of {Just f2 -> Right f2; Nothing -> Left$getT d n}

getT :: IM.IntMap b -> Nm a -> b
getT st n = findWithDefault (error ("Internal error: variable " ++ show n ++ " not assigned to a temp.")) n st

type CM = State CSt

infix 9 +=
(+=) t i = t =: (Tmp t+i)

rel :: Builtin -> Maybe IRel
rel Eq=Just IEq; rel Neq=Just INeq; rel Lt=Just ILt; rel Gt=Just IGt; rel Lte=Just ILeq; rel Gte=Just IGeq; rel _=Nothing

eRnk :: Sh a -> (Temp, Maybe AL) -> CE
eRnk sh (xR, lX) | Just i <- staRnk sh = KI i
                 | otherwise = EAt (ARnk xR lX)

ev, ec :: T a -> (Temp, Maybe AL) -> CE
ev (Arr (Ix _ i `Cons` _) _) _ = KI$fromIntegral i; ev _ (xR, lX) = EAt (ADim xR 0 lX)
ec (Arr (_ `Cons` Ix _ j `Cons` _) _) _ = KI$fromIntegral j; ec _ (xR, lX) = EAt (ADim xR 1 lX)

for (i `Cons` _) = For () (nz i) 1; for _ = For () Z 1

rof sh = Rof () (nzSh sh); rof1 sh = Rof () (n1 sh)
fort (Arr sh _) = for sh; fort _ = For () E.Z 1
forc (Arr sh _) = For () (nec sh) 1; forc _ = For () E.Z 1

f2or sh = F2or () (pr sh); f2orc sh = F2or () (pc sh)
f2ors sh = F2or () (psh sh); r2of sh = R2of () (psh sh)

mIFs :: [E a] -> Maybe [Word8]
mIFs = fmap concat.traverse b where b (BLit _ True)=Just [1]; b (BLit _ False)=Just [0]; b (FLit _ d)=Just (le$castDoubleToWord64 d); b (ILit _ n)=Just$le(fromIntegral n::Int64); b (Tup _ xs)=mIFs xs; b _=Nothing

writeC :: E (T ()) -> ([CS ()], LSt, AsmData, IM.IntMap Temp)
writeC = π.flip runState (CSt 0 (AL 0) 0 0 IM.empty IM.empty IM.empty IM.empty IM.empty IM.empty IM.empty IM.empty IM.empty) . writeCM . fmap rLi where π (s, CSt t _ _ l _ _ _ _ _ _ _ aa a) = (s, LSt l t, aa, a)

writeCM :: E (T ()) -> CM [CS ()]
writeCM eϵ = do
    cs <- nIs [(0::Int)..5]; fs <- nFs [(0::Int)..5]
    (zipWith (\xr xr' -> MX () xr' (FTmp xr)) [F0,F1,F2,F3,F4,F5] fs ++) . (zipWith (\r r' -> r' =: Tmp r) [C0,C1,C2,C3,C4,C5] cs ++) <$> go eϵ fs cs where
    go (Lam _ x@(Nm _ _ F) e) (xr:frs) rs = addD x xr *> go e frs rs
    go (Lam _ x@(Nm _ _ B) e) frs (r:rs) = addB x (bt r) *> go e frs rs where bt (ITemp i)=BTemp i
    go (Lam _ (Nm _ _ F) _) [] _ = error "Not enough floating-point registers."
    go (Lam _ x@(Nm _ _ I) e) frs (r:rs) = addVar x r *> go e frs rs
    go (Lam _ x@(Nm _ _ Arr{}) e) frs (r:rs) = addAVar x (Nothing, r) *> go e frs rs
    go Lam{} _ [] = error "Not enough registers."
    go e _ _ | isF (eAnn e) = do {f <- nF ; (++[MX () FRet0 (FTmp f)]) <$> feval e f} -- avoid clash with xmm0 (arg + ret)
             | isI (eAnn e) = do {t <- nI; (++[CRet =: Tmp t]) <$> eval e t} -- avoid clash when calling functions
             | isB (eAnn e) = do {t <- nBT; (++[MB () CBRet (Is t)]) <$> peval e t}
             | isArr (eAnn e) = do {(i,l,r) <- maa e; pure$r++[CRet =: Tmp i]++case l of {Just m -> [RA () m]; Nothing -> []}}
             | P [F,F] <- eAnn e = do {f0 <- nF; f1 <- nF; (++[MX () FRet0 (FTmp f0), MX () FRet1 (FTmp f1)]) <$> πr e [TF f0, TF f1]}
             | ty@P{} <- eAnn e, b64 <- bT ty, (n,0) <- b64 `quotRem` 8 = do {t <- nI; a <- nextArr CRet; (_,_,ls,pl) <- πe e t; pure (sac t b64:pl++MaB () a CRet (KI b64):CpyE () (TupM CRet (Just a)) (TupM t Nothing) (KI n) 8:popc b64:RA () a:(RA ()<$>ls))}

rtemp :: T a -> CM RT
rtemp F=FT<$>nF; rtemp I=IT<$>nI; rtemp B=PT<$>nBT; rtemp (P ts)=ΠT<$>traverse rtemp ts

πts :: E (T ()) -> CM [TT]
πts e₀ | P tys <- eAnn e₀ = traverse g tys where g I=TI<$>nI; g F=TF<$>nF; g B=TB<$>nBT; g Arr{} = do {t <- nI; a <- nextArr t; pure $ TA t (Just a)}

fc :: FBin -> Maybe (FTemp -> F2Temp -> CS ())
fc FPlus = Just (\_ x -> MX2 () x (ConstF (0,0))); fc FTimes = Just (\_ x -> MX2 () x (ConstF (1,1)))
fc FMax = Just (\x₀ x -> DS () x x₀); fc FMin = Just (\x₀ x -> DS () x x₀)
fc _ = Nothing

fS :: Builtin -> Bool
fS Times = True; fS Plus = True; fS Max = True; fS Min = True; fS _ = False

hasS :: E a -> Bool
hasS (EApp _ e0 e1)    = hasS e0&&hasS e1
hasS (Lam _ _ e)       = hasS e
hasS Var{}             = True
hasS FLit{}            = True
hasS Cond{}            = False
hasS Id{}              = False
hasS (LLet _ (_,e) e') = hasS e&&hasS e'
hasS (Builtin _ b)     = bS b
  where
    bS Times = True; bS Plus = True; bS Minus = True; bS Div  = True
    bS Neg   = True; bS Max  = True; bS Min   = True; bS Sqrt = True
    bS Abs   = True; bS _    = False

write2 :: E (T ()) -> [F2Temp] -> F2Temp -> CM [CS ()]
write2 (Lam _ x e) (v:vs) vret = addD2 x v *> write2 e vs vret
write2 e [] r                  = f2eval e r

writeA :: E (T ()) -> [TT]
       -> CM (Temp, Maybe AL, [CS ()])
writeA e as | isArr (codT$eAnn e) = do {r <- nI; (\(x,y) -> (r,x,y)) <$> writeF e as (IT r)}
            | otherwise = error "Internal error. writeA called on a function not returning an array."

writeF :: E (T ()) -> [TT] -> RT -> CM (Maybe AL, [CS ()])
writeF (Lam _ x e) (TA r l:rs) ret = addAVar x (l,r) *> writeF e rs ret
writeF (Lam _ x e) (TI r:rs) ret = addVar x r *> writeF e rs ret
writeF (Lam _ x e) (TF xr:rs) ret = addD x xr *> writeF e rs ret
writeF (Lam _ x e) (TB r:rs) ret = addB x r *> writeF e rs ret
writeF (Lam _ x e) (TΠ r:rs) ret = addΠ x r *> writeF e rs ret
writeF (Var ty x) [] (IT r) | isArr ty = do
    st <- gets avars
    let (l,t) = {-# SCC "getA" #-} getT st x
    pure (l,[r=:Tmp t])
writeF e [] (IT r) | isArr (eAnn e) = do {l <- nextArr r; (Just l,)<$>aeval e r l}
writeF e [] (ΠT rs) = (Nothing,)<$>πr e (rt<$>rs)
writeF e [] r = (Nothing,)<$>eeval e r

aSD :: E (T ()) -> [(T (), ArrAcc, Temp)] -> T () -> ArrAcc -> Temp -> CM [CS ()]
aSD f as rT rAt td = do
    (args, rArgs) <- unzip <$> traverse (\(t,r,xd) -> second ((:[xd+=KI (bT t)]).($undefined)) <$> arg t (\_ -> r)) as
    (r, wR) <- rW rT (\_ -> rAt)
    ss <- writeRF f args r
    pure (concat rArgs++ss++[wR undefined, td+=KI (bT rT)])

aS :: E (T ()) -> [(T (), Temp -> Int64 -> ArrAcc)] -> T () -> (Temp -> Int64 -> ArrAcc) -> CM ([Temp] -> Temp -> [CS ()])
aS f as rT rAt = do
    (args, rArgs) <- unzip <$> traverse (\(t,r) -> arg t (r-$bT t)) as
    (r, wR) <- rW rT (rAt-$bT rT)
    ss <- writeRF f args r
    pure (\is j -> zipWith ($) rArgs is++ss++[wR j])

type Ix'd = Temp -> ArrAcc

ve t l sz ix = AElem t 1 l (Tmp ix) sz
iXelem t rnk l sz ix = AElem t rnk l (Tmp ix) sz
ixarg t rnk l = AElem t rnk l.Tmp

infixr 8 .%
(.%) :: (a -> b -> c) -> (d -> a) -> b -> d -> c
(.%) f g x y = f (g y) x

arg :: T () -> Ix'd -> CM (RT, Temp -> CS ())
arg ty at | nind ty = do
    t <- rtemp ty
    pure (t, (mt.%at) t)

rW :: T () -> Ix'd -> CM (RT, Temp -> CS ())
rW ty at | nind ty = do
    t <- rtemp ty
    pure (t, (wt.%at) t)

aiA slopD (xd,lX) i = cpy (Raw slopD 0 Nothing) (Raw xd i lX)
aiR (td,l) (yR,lY,yRnk) n sz = [cpy (Raw td 0 l) (AElem yR yRnk lY 0) n sz, td+=(n*KI sz)]

writeRF :: E (T ()) -> [RT] -> RT -> CM [CS ()]
writeRF e args = fmap snd.writeF e (rt<$>args)

data RT = IT !Temp | FT !FTemp | PT !BTemp | ΠT [RT]

mt :: ArrAcc -> RT -> CS ()
mt p (FT t) = MX () t (FAt p); mt p (PT t) = MB () t (PAt p)
mt p (IT t) = t =: EAt p; mt p (ΠT rs) = ATT () (rt<$>rs) p

mvts = concat.*zipWith mvt where mvt (TI t0) (TI t1)=[t0=:Tmp t1]; mvt (TF x0) (TF x1)=[MX () x0 (FTmp x1)]; mvt (TB t0) (TB t1)=[MB () t0 (Is t1)]; mvt (TΠ tt0) (TΠ tt1)=mvts tt0 tt1

wt :: ArrAcc -> RT -> CS ()
wt p (IT t) = Wr () p (Tmp t); wt p (FT t) = WrF () p (FTmp t)
wt p (PT t) = WrP () p (Is t); wt p (ΠT rs) = WrT () p (rt<$>rs)

cpy dest asrc n sz = CpyE () (dest sz) (asrc sz) n sz
mv dest asrc sz = Mv () (dest sz) (asrc sz) sz

tr (TF x)=FT x; tr (TI r)=IT r; tr (TB r)=PT r; tr (TΠ rs)=ΠT (tr<$>rs)
rt (FT x)=TF x; rt (IT r)=TI r; rt (PT r)=TB r; rt (ΠT rs)=TΠ (rt<$>rs)

unFA (TF x)=FTmp x; unIA (TI r)=Tmp r; unBA (TB p)=Is p

eeval :: E (T ()) -> RT -> CM [CS ()]
eeval e (IT t) = eval e t; eeval e (FT t) = feval e t
eeval e (PT t) = peval e t; eeval e (ΠT t) = πr e (rt<$>t)

data RI a b = Cell a | Index b

part :: [RI a b] -> ([a], [b])
part []           = ([], [])
part (Cell i:is)  = first (i:) $ part is
part (Index i:is) = second (i:) $ part is

iterDims allIx dts = part $ zipWith (\ixϵ dt -> case ixϵ of {Cell{} -> Cell dt; Index{} -> Index dt}) allIx dts

md sh t a rnk n dt sz = Ma () sh a t rnk n sz:diml (t, Just a) dt
mdn sh t a rnk dt sz = let ds=Tmp<$>dt in do {n <- nI; pure (PlProd () n ds:md sh t a (KI rnk) (Tmp n) ds sz)}

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

off :: Temp -> Maybe AL -> [CE] -> CM ([CS ()], CE)
off _ _ [i] = pure ([], i)
off xR lX ixs = do {s <- nI; b <- nI; pure (b=:0 : s=:1 : init (concat [[b+=(Tmp s*n), s=:(Tmp s*EAt (ADim xR (KI i) lX))] | (n,i) <- zip ixϵ [0..]]), Tmp b) }
  where ixϵ=reverse ixs

data Cell a b = Fixed -- set by the larger procedure
              | Bound b -- to be iterated over

aall is ds bs cs = do {i <- nI; pure (i=:0:forAll is ds bs (cs i++[i+=1]))}

forAll is ds bs = thread (zipWith3 g is ds bs) where
    g t d b@(KI i) | i > 0 = (:[]) . For () E.S d t 0 ILt b
    g t d b                = (:[]) . For () E.Z d t 0 ILt b

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

idims rnk oRnk xR lX = [EAt (ADim xR (KI l) lX) | l <- reverse (take (fromIntegral rnk) [oRnk-1,oRnk-2..]) ]

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
llet (n,e') | Arr{} <- eAnn e' = do
    (eR,l,ss) <- maa e'
    addAVar n (l,eR) $> ss
llet (n,e') | I <- eAnn e' = do {eR <- bI n; eval e' eR}
llet (n,e') | F <- eAnn e' = do {eR <- bD n; feval e' eR}
llet (n,e') | B <- eAnn e' = do {eR <- bB n; peval e' eR}
llet (n,e') | P{} <- eAnn e' = do {eR <- bp n e'; πr e' eR}
llet (n,e') | (tArgs, tC) <- ur (eAnn e'), all nind (tC:tArgs) = do
    l <- neL
    xs <- traverse rtemp tArgs; y <- rtemp tC
    let rrs=rt<$>xs
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

data RA = AI !AD | NA !RT

fill :: E (T ()) -> AD -> [RA] -> CM (CS ())
fill (EApp _ (Builtin _ Zip) op) (AD t lA (Just (Arr sh _)) _ _ _) [AI (AD aPX lX _ _ _ (Just n)), AI (AD aPY lY _ _ _ _)]
    | (Arrow tX (Arrow tY tC)) <- eAnn op, nind tX && nind tY && nind tC = do
    step <- aS op [(tX, ixarg aPX 1 lX), (tY, ixarg aPY 1 lY)] tC (ixarg t 1 lA)
    afor sh 0 ILt n (\i -> step (repeat i) i)
    -- TODO: parallel/step-2?
fill (EApp _ (Builtin _ Succ) op) (AD t lA (Just (Arr sh _)) _ _ (Just n')) [AI (AD xR lX _ _ _ _)]
    | Arrow tX (Arrow _ tZ) <- eAnn op = do
    step <- aS op [(tX, \iϵ -> AElem xR 1 lX (Tmp iϵ+1)), (tX, ixarg xR 1 lX)] tZ (ixarg t 1 lA)
    afor sh 0 ILt n' (\i -> step (repeat i) i)
fill (EApp _ (Builtin _ ScanS) op) (AD t lA _ _ _ (Just n)) [NA acc, AI (AD aP l (Just tXs) _ _ _)]
    | Arrow tX (Arrow tY _) <- eAnn op, Just xSz <- nSz tX, Just ySz <- nSz tY = do
    (x, wX) <- arg tY (ve aP l ySz)
    ss <- writeRF op [acc, x] acc
    afort tXs 0 ILt n (\i -> wt (AElem t 1 lA (Tmp i) xSz) acc:wX i:ss)

fv (AD xR lX _ _ (Just sz) (Just n)) (AD yR lY _ _ _ _) i j = [cpy (AElem xR 1 lX i) (AElem yR 1 lY j) n sz]

rfill :: E (T ()) -> AD -> [RA] -> CM [CS ()]
rfill (Builtin _ Init) d [AI s] = pure (fv d s 0 0)
rfill (Builtin _ InitM) d [AI s] = pure (fv d s 0 0)
rfill (Builtin _ Tail) d [AI s] = pure (fv d s 0 1)
rfill (Builtin _ TailM) d [AI s] = pure (fv d s 0 1)
rfill (Builtin _ Take) d [AI s] = pure (fv d s 0 0)
rfill (Builtin _ Drop) d [AI s, NA (IT n)] = pure (fv d s 0 (Tmp n))
rfill (Builtin _ Del) (AD t lA _ _ (Just sz) (Just n)) [AI (AD xR lX _ _ _ _), NA (IT j)] =
    pure [cpy (AElem t 1 lA 0) (AElem xR 1 lX 0) (Tmp j) sz, cpy (AElem t 1 lA (Tmp j)) (AElem xR 1 lX (Tmp j+1)) (n-Tmp j) sz]
rfill (EApp _ (Builtin _ Map) f) (AD t lA _ _ _ (Just n)) [AI (AD xR lX (Just (Arr xSh _)) _ _ _)] | Arrow F F <- eAnn f, hasS f = do
    td <- nI; xRd <- nI; i <- nI
    x <- nF2; y <- nF2; x₀ <- nF; y₀ <- nF
    ss <- write2 f [x] y
    s1 <- writeRF f [FT x₀] (FT y₀)
    let step=MX2 () x (FAt (Raw xRd 0 lX 8)):xRd+=16:ss++[Wr2F () (Raw td 0 lA 8) (FTmp y), td+=16]
        step1=MX () x₀ (FAt (Raw xRd 0 lX 8)):xRd+=8:s1++[WrF () (Raw td 0 lA 8) (FTmp y₀), td+=8]
        loop=r2of xSh i n step step1
    pure [xRd=:DP xR 1,td=:DP t 1, loop]
rfill (EApp _ (Builtin _ Map) op) (AD t lA (Just (Arr sh _)) _ _ (Just n)) [AI (AD xR l _ _ _ _)] | Arrow tD tC <- eAnn op, nind tD = do
    xRd <- nI; td <- nI;
    step <- aSD op [(tD, Raw xRd 0 l undefined, xRd)] tC (Raw td 0 lA undefined) td
    loop <- arof sh n step
    pure (xRd=:DP xR 1:td=:DP t 1:[loop])
rfill (Builtin _ CatE) (AD t lA _ _ (Just sz) _) [AI (AD xR lX _ _ _ (Just xn)), AI (AD yR lY _ _ _ (Just yn))] =
    pure [cpy (AElem t 1 lA 0) (AElem xR 1 lX 0) xn sz, cpy (AElem t 1 lA xn) (AElem yR 1 lY 0) yn sz]
rfill (Builtin _ ConsE) (AD t lA _ _ (Just sz) _) [NA xR, AI (AD xsR lX _ _ _ (Just n))] =
    pure [wt (AElem t 1 lA 0 sz) xR, cpy (AElem t 1 lA 1) (AElem xsR 1 lX 0) n sz]
rfill (Builtin _ ConsE) (AD t lA _ _ _ _) [NA (ΠT ps), AI (AD xsR lX _ _ (Just sz) (Just n))] = do
    pure [WrT () (AElem t 1 lA 0 sz) (rt<$>ps), cpy (AElem t 1 lA 1) (AElem xsR 1 lX 0) n sz]
rfill (Builtin _ Snoc) (AD t lA _ _ (Just sz) _) [NA xR, AI (AD xsR lX _ _ _ (Just n))] =
    pure [wt (AElem t 1 lA n sz) xR, cpy (AElem t 1 lA 0) (AElem xsR 1 lX 0) n sz]
rfill (Builtin _ Snoc) (AD t lA _ _ _ _) [NA (ΠT ps), AI (AD xsR lX _ _ (Just sz) (Just n))] = do
    pure [WrT () (AElem t 1 lA n sz) (rt<$>ps), cpy (AElem t 1 lA 0) (AElem xsR 1 lX 0) n sz]
rfill (Builtin _ Cyc) (AD t lA (Just (Arr oSh _)) _ (Just sz) _) [AI (AD xR lX _ _ _ (Just nx)), NA (IT nR)] = do
    ix <- nI
    loop <- arof oSh (Tmp nR) [cpy (AElem t 1 lA (Tmp ix)) (AElem xR 1 lX 0) nx sz, ix+=nx]
    pure [ix=:0, loop]
rfill (EApp _ (Builtin _ Scan) op) (AD t lA (Just (Arr oSh _)) _ (Just accSz) (Just n)) [AI (AD xR lX _ _ (Just xSz) _), NA acc, NA x] = do
    ss <- writeRF op [acc, x] acc
    loop <- afor1 oSh 1 ILeq n (\i -> wt (AElem t 1 lA (Tmp i-1) accSz) acc:mt (AElem xR 1 lX (Tmp i) xSz) x:ss)
    pure [mt (AElem xR 1 lX 0 xSz) acc, loop]
rfill (EApp _ (Builtin _ Outer) op) (AD t lA _ _ _ _) [AI (AD xR lX (Just tXs) _ _ (Just nx)), AI (AD yR lY (Just tYs) _ _ (Just ny))]
    | Arrow tX (Arrow tY tC) <- eAnn op = do
    i <- nI; j <- nI; k <- nI
    step <- aS op [(tX, ixarg xR 1 lX), (tY, ixarg yR 1 lY)] tC (ixarg t 2 lA)
    let loop=fort tXs i 0 ILt nx [fort tYs j 0 ILt ny (step [i,j] k++[k+=1])]
    pure (k=:0:[loop])
rfill (Builtin _ Rot) (AD t lA _ _ _ _) [AI (AD xsR lX _ _ (Just sz) (Just nx)), NA (IT nR)] = do
    c <- nI
    pure [Ifn't () (IRel IGeq (Tmp nR) 0) [nR+=nx], c =: (nx-Tmp nR), cpy (AElem t 1 lA 0) (AElem xsR 1 lX (Tmp nR)) (Tmp c) sz, cpy (AElem t 1 lA (Tmp c)) (AElem xsR 1 lX 0) (Tmp nR) sz]
rfill (Builtin _ RevE) (AD t lA (Just (Arr oSh _)) _ (Just sz) _) [AI (AD xR lX _ _ _ (Just n))] =
    (:[]) <$> afor oSh 0 ILt n (\i -> [mv (AElem t 1 lA (Tmp i)) (AElem xR 1 lX (n-Tmp i-1)) sz])
rfill (Builtin _ AddDim) (AD t lA _ (Just rnk) (Just sz) _) [AI (AD xR lX _ (Just xRnk) _ (Just n))] = do
    td <- nI; xRd <- nI
    pure [td=:DP t rnk, xRd=:DP xR xRnk, cpy (Raw td 0 lA) (Raw xRd 0 lX) n sz]

afor sh el c eu ss = do {i <- nI; pure (for sh i el c eu (ss i))}
afor1 sh el c eu ss = do {i <- nI; pure (ff 1 i el c eu (ss i))} where
    ff = For() (n1 sh)
afort (Arr sh _) el c eu ss = do {i <- nI; pure (for sh i el c eu (ss i))}
afors sh el c eu ss = do {i <- nI; pure (ff 1 i el c eu (ss i))} where
    ff = For() (nzSh sh)
arof sh n ss = do {i <- nI; pure (rof sh i n ss)}; arof1 sh n ss = do {k <- nI; pure (rof1 sh k n ss)}

maa :: E (T ()) -> CM (Temp, Maybe AL, [CS ()])
maa (Var _ x) = do
    st <- gets avars
    let (l,t) = {-# SCC "getA" #-} getT st x
    pure (t,l,[])
maa (Id _ (AShLit ns es)) | Just ws <- mIFs es = do
    t <- nI; n <- nextAA
    addAA n (concatMap le8 (rnk:ns)++ws)
    pure (t, Nothing, [t =: LA n])
  where
    rnk=genericLength ns
    le8=le.(fromIntegral::Int->Int64)
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
    pure (plE0 $ plE1 (md sh t a rnk (KI n) (KI<$>ixs) 8++MX () scaleR (e1e-e0e):[loop]))
                                                           | otherwise = usi
aeval (EApp (Arr sh I) (EApp _ (Builtin _ A.R) e0) e1) t a | Just ixs <- staIx sh = do
    scaleR <- nI; iR <- nI
    (plE0,e0e) <- plC e0; (plE1,e1e) <- plC e1
    let rnk=genericLength ixs; n=product ixs
    loop <- afors sh 0 ILt (KI n) $ \k ->
              [Rnd () iR, iR =: (Bin IRem (Tmp iR) (Tmp scaleR) + e0e), Wr () (AElem t rnk (Just a) (Tmp k) 8) (Tmp iR)]
    pure (plE0$plE1$md sh t a rnk (KI n) (KI<$>ixs) 8++scaleR=:(e1e-e0e+1):[loop])
                                                           | otherwise = usi
aeval (EApp _ (EApp _ (Builtin _ I1) i1) e) t a | iT@(Arr iSh _) <- eAnn i1, Just eSz <- aB (eAnn e) = do
    n <- nI
    (plX, (lX, xR)) <- plA e
    (plI, (lI, iR)) <- plA i1
    loop <- afors iSh 0 ILt (Tmp n) $ \k -> [mv (AElem t 1 (Just a) (Tmp k)) (AElem xR 1 lX (EAt$AElem iR 1 lI (Tmp k) 8)) eSz]
    pure $ plX$plI$n=:ev iT (iR,lI):vSz iSh t a (Tmp n) eSz++[loop]
aeval (EApp (Arr oSh _) (EApp _ (Builtin _ I1) i1) e) t a | iT@(Arr iSh _) <- eAnn i1, Just (tX, rnk) <- tRnk (eAnn e), Just sz <- nSz tX = do
    n <- nI; nA <- nI
    (plX, (lX, xR)) <- plA e
    (plI, (lI, iR)) <- plA i1
    (dts, dss) <- plDim rnk (xR, lX)
    let ts=tail dts
    loop <- afors iSh 0 ILt (Tmp n) $ \k -> [cpy (AElem t 1 (Just a) (Tmp k*Tmp nA)) (AElem xR 1 lX (Tmp nA*EAt (AElem iR 1 lI (Tmp k) 8))) (Tmp nA) sz]
    pure $ plX$plI$n=:ev iT (iR,lI):tail dss++PlProd () nA (Tmp<$>ts):md oSh t a (KI rnk) (Tmp n*Tmp nA) (Tmp<$>n:ts) sz++[loop]
aeval (EApp (Arr oSh ty) (Builtin _ Di) e) t a | Just sz <- nSz ty = do
    (plX, (lX, xR)) <- plA e
    td <- nI; xRd <- nI; n <- nI
    ll <- afor oSh 0 ILt (Tmp n) $ \i ->
            [mv (Raw td 0 (Just a)) (At xRd [Tmp n, 1] [Tmp i, Tmp i] lX) sz, td+=KI sz]
    pure $ plX$n=:ev (eAnn e) (xR,lX):vSz oSh t a (Tmp n) sz++[xRd=:DP xR 2, td=:DP t 1, ll]
aeval (EApp (Arr sh _) (Builtin _ AddDim) x) t a | Just (ty,sz) <- rr (eAnn x) = do
    xR <- rtemp ty
    plX <- eeval x xR
    pure (plX++vSz sh t a 1 sz++[wt (AElem t 1 (Just a) 0 8) xR])
aeval (EApp (Arr oSh _) g@(Builtin _ AddDim) xs) t a | Arr sh ty <- eAnn xs, Just sz <- nSz ty = do
    (plX, (lX, xR)) <- plA xs
    xRnk <- nI; szR <- nI; rnk <- nI
    contents <- rfill g (AD t (Just a) Nothing (Just$Tmp rnk) (Just sz) Nothing) [AI (AD xR lX Nothing (Just$Tmp xRnk) Nothing (Just$Tmp szR))]
    pure (plX$xRnk=:eRnk sh (xR,lX):SZ () szR xR (Tmp xRnk) lX:rnk =: (Tmp xRnk+1):Ma () oSh a t (Tmp rnk) (Tmp szR) sz:
           [Wr () (ADim t 0 (Just a)) 1, CpyD () (ADim t 1 (Just a)) (ADim xR 0 lX) (Tmp xRnk)]++contents)
aeval (EApp oTy@(Arr oSh tX) (Builtin _ Sort) x) t a | Just lt <- cr tX = do
    (plX, (lX, xR)) <- plA x
    ph <- nI; bl <- nI; nB <- nI; i₀ <- nI; i₁ <- nI; iₒ <- nI; bl₀ <- nI; bl₁ <- nI
    e₀ <- rtemp tX; e₁ <- rtemp tX
    n <- nI; np <- nI; pad <- nI; blSz <- nI; blOSz <- nI; steps <- nI; i <- nI
    slop <- nI; lS <- nextArr slop
    inP <- nI; oP <- nI
    let iAt0=Raw inP (Tmp bl₀*Tmp blSz+Tmp i₀) (Just lS) 8; iAt1=Raw inP (Tmp bl₁*Tmp blSz+Tmp i₁) (Just lS) 8
        iAtO=Raw oP (Tmp bl*Tmp blOSz+Tmp iₒ) (Just lS) 8
    pure (plX$
         n=:ev oTy (xR,lX)
        :steps=:(63-IU Clz (Tmp n)):np=:Bin IAsl 2 (Tmp steps):Cmov () (IRel INeq (Tmp n) (Tmp np)) steps (Tmp steps+1)
        :pad=:(Tmp np-Tmp n)
        -- pad it to a power of 2
        :MaB () lS slop (Tmp np*(Tmp steps+1)*8)
        :For () E.Z 1 i 0 ILt (Tmp pad) [ε (Raw slop (Tmp i) (Just lS) 8)]
        :cpy (Raw slop (Tmp pad) (Just lS)) (AElem xR 1 lX 0) (Tmp n) 8
        :i₀=:0:i₁=:0:inP=:Tmp slop:oP=:(Tmp slop+Tmp np*8):blSz=:1:blOSz=:2:nB=:Bin IAsr (Tmp np) 1
        :For () E.Z 1 ph 1 ILeq (Tmp steps)
            [ For () E.Z 1 bl 0 ILt (Tmp nB) [
                i₀=:0, i₁=:0, bl₀=:(Tmp bl*2), bl₁=:(Tmp bl₀+1),
                -- fill out-block/next slab
                For () E.Z 1 iₒ 0 ILt (Tmp blOSz)
                    [ If () (IRel IGeq (Tmp i₀) (Tmp blSz))
                        [ Wr () iAtO (EAt iAt1), i₁+=1 ]
                        [ If () (IRel IGeq (Tmp i₁) (Tmp blSz))
                          [ Wr () iAtO (EAt iAt0), i₀+=1 ]
                          [ mt iAt0 e₀, mt iAt1 e₁
                          , If () (lt e₀ e₁) [wt iAtO e₀, i₀+=1] [wt iAtO e₁, i₁+=1]
                          ]
                        ]
                    ]
                ]
            , inP=:Tmp oP, oP+=(Tmp np*8)
            , blSz=:(Tmp blSz*2), blOSz=:(Tmp blOSz*2)
            , nB=:Bin IAsr (Tmp nB) 1
            ]
        :v8 oSh t a (Tmp n)
        ++[cpy (AElem t 1 (Just a) 0) (Raw slop (Tmp np*Tmp steps+Tmp pad) (Just lS)) (Tmp n) 8])
  where
    cr I=Just (\(IT r0) (IT r1) -> IRel ILt (Tmp r0) (Tmp r1)); cr F=Just (\(FT x0) (FT x1) -> FRel FLt (FTmp x0) (FTmp x1)); cr _=Nothing
    ε at=case tX of F -> WrF () at (let (_,ub)=floatRange (undefined::Double) in ConstF (- (encodeFloat (2^(12::Int)-1) ub))); I -> Wr () at (KI minBound)
aeval (EApp oTy@(Arr oSh _) e@(Builtin _ Init) x) t a | Just sz <- aB oTy = do
    nR <- nI
    (plX, (lX, xR)) <- plA x
    contents <- rfill e (AD t (Just a) Nothing Nothing (Just sz) (Just$Tmp nR)) [AI (AD xR lX Nothing Nothing Nothing Nothing)]
    pure (plX$nR =: (ev (eAnn x) (xR,lX)-1):vSz oSh t a (Tmp nR) sz++contents)
aeval (EApp oTy@(Arr oSh _) (EApp _ e@(Builtin _ Take) n) x) t a | Just sz <- aB oTy = do
    (plX, (lX, xR)) <- plA x; (plN,nE) <- plC n
    contents <- rfill e (AD t (Just a) Nothing Nothing (Just sz) (Just nE)) [AI (AD xR lX Nothing Nothing Nothing Nothing)]
    pure (plX$plN$vSz oSh t a nE sz++contents)
aeval (EApp oTy@(Arr oSh _) (EApp _ e@(Builtin _ Drop) n) x) t a | Just sz <- aB oTy = do
    (plX, (lX, xR)) <- plA x; (plN,nR) <- plEV n
    nO <- nI
    contents <- rfill e (AD t (Just a) Nothing Nothing (Just sz) (Just$Tmp nO)) [AI (AD xR lX Nothing Nothing Nothing Nothing), NA (IT nR)]
    pure (plX$plN$nO=:(ev (eAnn x) (xR,lX)-Tmp nR):vSz oSh t a (Tmp nO) sz++contents)
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
aeval (EApp oTy@(Arr oSh _) e@(Builtin _ TailM) x) t a | Just sz <- aB oTy = do
    nR <- nI
    (plX, (lX, xR)) <- plA x
    contents <- rfill e (AD t (Just a) Nothing Nothing (Just sz) (Just$Tmp nR)) [AI (AD xR lX Nothing Nothing Nothing Nothing)]
    pure (plX$nR =: Bin IMax (ev (eAnn x) (xR,lX)-1) 0:vSz oSh t a (Tmp nR) sz++contents)
aeval (EApp oTy@(Arr oSh _) (EApp _ e@(Builtin _ Del) x) j) t a | Just sz <- aB oTy = do
    nR <- nI
    (plX, (lX, xR)) <- plA x; (plN,jR) <- plEV j
    contents <- rfill e (AD t (Just a) Nothing Nothing (Just sz) (Just$Tmp nR)) [AI (AD xR lX Nothing Nothing Nothing Nothing), NA (IT jR)]
    pure (plX$nR=:(ev (eAnn x) (xR,lX)-1):vSz oSh t a (Tmp nR) sz++plN contents)
aeval (EApp oTy@(Arr oSh _) (EApp _ (Builtin _ DelM) x) j) t a | Just sz <- aB oTy = do
    nR <- nI
    (plX, (lX, xR)) <- plA x; (plN,jR) <- plEV j
    pure (plX$nR=:ev (eAnn x) (xR,lX):Ma () oSh a t 1 (Tmp nR) sz:plN
        [ If () (IRel ILt (Tmp jR) (Tmp nR))
            [ Wr () (ADim t 0 (Just a)) (Tmp nR-1), cpy (AElem t 1 (Just a) 0) (AElem xR 1 lX 0) (Tmp jR) sz, cpy (AElem t 1 (Just a) (Tmp jR)) (AElem xR 1 lX (Tmp jR+1)) (Tmp nR-Tmp jR) sz]
            [ Wr () (ADim t 0 (Just a)) (Tmp nR), cpy (AElem t 1 (Just a) 0) (AElem xR 1 lX 0) (Tmp nR) sz]
        ])
aeval (EApp (Arr oSh _) (EApp _ (Builtin _ Del) x) j) t a | Just (tX, rnk) <- tRnk (eAnn x), Just sz <- nSz tX = do
    nO <- nI; nE <- nI
    (plX, (lX, xR)) <- plA x; (plJ, jR) <- plEV j
    (dts, dss) <- plDim rnk (xR, lX)
    let n:ds=dts; rnkE=KI rnk
    pure (plX$dss++nO=:(Tmp n-1):PlProd () nE (Tmp<$>ds):md oSh t a rnkE (Tmp nO*Tmp nE) (Tmp<$>nO:ds) sz
          ++plJ [ cpy (AElem t rnkE (Just a) 0) (AElem xR rnkE lX 0) (Tmp jR*Tmp nE) sz
                , cpy (AElem t rnkE (Just a) (Tmp jR*Tmp nE)) (AElem xR rnkE lX ((Tmp jR+1)*Tmp nE)) ((Tmp n-Tmp jR-1)*Tmp nE) sz])
aeval (EApp (Arr oSh _) (EApp _ (Builtin _ DelM) x) j) t a | Just (tX, rnk) <- tRnk (eAnn x), Just sz <- nSz tX = do
    nX <- nI; nO <- nI; nE <- nI
    (plX, (lX, xR)) <- plA x; (plJ, jR) <- plEV j
    (dts, dss) <- plDim rnk (xR, lX)
    let n:ds=dts; rnkE=KI rnk
    pure (plX$dss++nO=:(Tmp n-1):PlProd () nE (Tmp<$>ds):nX=:(Tmp nO*Tmp nE):md oSh t a rnkE (Tmp nX) (Tmp<$>nO:ds) sz
          ++plJ [ If () (IRel ILt (Tmp jR) (Tmp nO))
                    [ cpy (AElem t rnkE (Just a) 0) (AElem xR rnkE lX 0) (Tmp jR*Tmp nE) sz
                    , cpy (AElem t rnkE (Just a) (Tmp jR*Tmp nE)) (AElem xR rnkE lX ((Tmp jR+1)*Tmp nE)) ((Tmp n-Tmp jR-1)*Tmp nE) sz]
                    [ cpy (AElem t rnkE (Just a) 0) (AElem xR rnkE lX 0) (Tmp nX+Tmp nE) sz ] ])
aeval (Id (Arr oSh _) (Aɴ xs ns)) t a | Just (tX, xRnk) <- tRnk (eAnn xs), Just sz <- nSz tX = do
    (plNs, nEs) <- first thread.unzip <$> traverse plC ns
    (plX, (lX, xR)) <- plA xs
    xRd <- nI; szA <- nI
    (dts, plDs) <- plDim xRnk (xR, lX)
    let ots = drop k dts
        oRnk=KI$xRnk-k
    (plB, b) <- off xR lX nEs
    pure (plX$drop k plDs++PlProd () szA (Tmp<$>ots):Ma () oSh a t oRnk (Tmp szA) sz:CpyD () (ADim t 0 (Just a)) (ADim xR 1 lX) oRnk:plNs (plB ++ [xRd=:DP xR (KI xRnk), cpy (AElem t oRnk (Just a) 0) (Raw xRd (b*Tmp szA) lX) (Tmp szA) sz]))
  where
    k :: Integral a => a
    k=genericLength ns
aeval (EApp (Arr oSh _) (Builtin _ Init) xs) t a | Just (tX, rnk) <- tRnk (eAnn xs), Just sz <- nSz tX = do
    d0 <- nI; n <- nI
    (plX, (lX, xR)) <- plA xs
    (dtx, plDs) <- plDim rnk (xR, lX)
    let dx0=head dtx; dts=Tmp<$>(d0:tail dtx)
    pure (plX$plDs++d0=:(Tmp dx0-1):PlProd () n dts:md oSh t a (KI rnk) (Tmp n) dts sz++[cpy (AElem t (KI rnk) (Just a) 0) (AElem xR (KI rnk) lX 0) (Tmp n) sz])
                                                 | otherwise = unsupported
aeval (EApp (Arr oSh _) (Builtin _ Tail) x) t a | Just (tX, rnk) <- tRnk (eAnn x), Just sz <- nSz tX = do
    d0 <- nI; n <- nI
    (plX, (lX, xR)) <- plA x
    (dtx,ss) <- plDim rnk (xR,lX)
    let dx0=head dtx; dts=Tmp<$>(d0:tail dtx)
    pure (plX$ss++d0=:(Tmp dx0-1):PlProd () n dts:md oSh t a (KI rnk) (Tmp n) dts sz++[cpy (AElem t (KI rnk) (Just a) 0) (AElem xR (KI rnk) lX (Tmp dx0)) (Tmp n) sz])
                                                | otherwise = unsupported
aeval (EApp (Arr oSh _) (Builtin _ InitM) xs) t a | Just (tX, rnk) <- tRnk (eAnn xs), Just sz <- nSz tX = do
    d0 <- nI; n <- nI
    (plX, (lX, xR)) <- plA xs
    (dtx, plDs) <- plDim rnk (xR, lX)
    let dx0=head dtx; dts=Tmp<$>(d0:tail dtx)
    pure (plX$plDs++d0=:Bin IMax (Tmp dx0-1) 0:PlProd () n dts:md oSh t a (KI rnk) (Tmp n) dts sz++[cpy (AElem t (KI rnk) (Just a) 0) (AElem xR (KI rnk) lX 0) (Tmp n) sz])
                                                 | otherwise = unsupported
aeval (EApp (Arr oSh _) (Builtin _ TailM) x) t a | Just (tX, rnk) <- tRnk (eAnn x), Just sz <- nSz tX = do
    d0 <- nI; n <- nI
    (plX, (lX, xR)) <- plA x
    (dtx,ss) <- plDim rnk (xR,lX)
    let dx0=head dtx; dts=Tmp<$>(d0:tail dtx)
    pure (plX$ss++d0=:Bin IMax (Tmp dx0-1) 0:PlProd () n dts:md oSh t a (KI rnk) (Tmp n) dts sz++[cpy (AElem t (KI rnk) (Just a) 0) (AElem xR (KI rnk) lX (Tmp dx0)) (Tmp n) sz])
                                                | otherwise = unsupported
aeval (EApp (Arr oSh _) (Builtin _ Flat) xs) t a | (Arr sh ty) <- eAnn xs, Just sz <- nSz ty = do
    (plX, (lX, xR)) <- plA xs
    xRnk <- nI; szR <- nI
    pure (plX$xRnk=:eRnk sh (xR,lX):SZ () szR xR (Tmp xRnk) lX:vSz oSh t a (Tmp szR) sz++[cpy (AElem t 1 (Just a) 0) (AElem xR (Tmp xRnk) lX 0) (Tmp szR) sz])
aeval (EApp _ (EApp _ (Builtin _ Filt) p) xs) t a | Arrow tX _ <- eAnn p, tXs@(Arr sh _) <- eAnn xs, Just sz <- nSz tX = do
    szR <- nI; nR <- nI; b <- nBT
    (plX, (lX, xsR)) <- plA xs
    (xR, rX) <- arg tX (\k -> AElem xsR 1 lX (Tmp k) sz)
    ss <- writeRF p [xR] (PT b)
    loop <- afor sh 0 ILt (Tmp szR) $ \k -> rX k:ss++[If () (Is b) [wt (AElem t 1 (Just a) (Tmp nR) sz) xR, nR+=1] []]
    pure (plX$szR =: ev tXs (xsR,lX)
        :Ma () sh a t 1 (Tmp szR) sz
        :[nR=:0, loop, Wr () (ADim t 0 (Just a)) (Tmp nR)])
aeval (EApp _ (EApp _ (Builtin _ Ices) p) xs) t a | Arrow tX _ <- eAnn p, tXs@(Arr sh _) <- eAnn xs, Just sz <- nSz tX = do
    szR <- nI; nR <- nI; b <- nBT
    (plX, (lX, xsR)) <- plA xs
    (xR, rX) <- arg tX (ve xsR lX sz)
    ss <- writeRF p [xR] (PT b)
    loop <- afor sh 0 ILt (Tmp szR) $ \k -> rX k:ss++[If () (Is b) [Wr () (AElem t 1 (Just a) (Tmp nR) 8) (Tmp k), nR+=1] []]
    pure (plX$szR=:ev tXs (xsR,lX)
        :Ma () sh a t 1 (Tmp szR) 8
        :[nR=:0, loop, Wr () (ADim t 0 (Just a)) (Tmp nR)])
aeval (EApp _ f@(EApp _ (Builtin _ Map) op) e) t a
    | Arr sh _ <- tX, Arrow tD tC <- eAnn op
    , Just sz <- nSz tC, nind tD = do
    (plE, (l, xR)) <- plA e
    nR <- nI
    contents <- rfill f (AD t (Just a) (Just tX) Nothing Nothing (Just$Tmp nR)) [AI (AD xR l (Just tX) Nothing Nothing Nothing)]
    pure (plE$nR=:ev tX (xR,l):vSz sh t a (Tmp nR) sz++contents)
  where tX=eAnn e
aeval (EApp (Arr oSh _) (EApp _ (Builtin _ Map) f) xs) t a
    | Arrow tD tC <- eAnn f
    , Arr xSh _ <- eAnn xs
    , Just xRnk <- staRnk xSh
    , Just (ta, rnk) <- tRnk tD
    , Just szD <- nSz ta, Just sz <- nSz tC = do
    szR <- nI; xd <- nI; i <- nI
    (plX, (lX, xR)) <- plA xs
    (slopP, slopSz, aSlop, pops) <- plSlop szD rnk (idims rnk xRnk xR lX)
    (y, wRet) <- rW tC (ve t (Just a) sz)
    (_, ss) <- writeF f [TA slopP Nothing] y
    let xDims=[EAt (ADim xR (KI l) lX) | l <- [0..(rnk-1)]]
        dimsFromIn=KI$xRnk-rnk
        oRnk=xRnk-rnk
    loop <- afors xSh 0 ILt (Tmp szR) $ \k -> cpy (AElem slopP (KI rnk) Nothing 0) (Raw xd (Tmp i) lX) (Tmp slopSz) szD:ss++[wRet k, i+=Tmp slopSz]
    pure (plX$aSlop++PlProd () szR xDims
        :Ma () oSh a t (KI oRnk) (Tmp szR) sz
            :CpyD () (ADim t 0 (Just a)) (ADim xR 0 lX) dimsFromIn
        :xd=:DP xR (KI xRnk):i=:0
        :(loop:[pops]))
aeval (EApp (Arr oSh _) (EApp _ (Builtin _ Map) f) xs) t a
    | (Arrow tD tC) <- eAnn f
    , (Arr xSh _) <- eAnn xs
    , Just xRnk <- staRnk xSh
    , Just (ta, rnk) <- tRnk tC
    , Just szO <- nSz ta, Just dSz <- nSz tD = do
    y <- nI; y0 <- nI; szX <- nI; szY <- nI
    td <- nI
    (plX, (lX, xR)) <- plA xs
    (x0, wX0) <- arg tD (const $ AElem xR (KI xRnk) lX 0 dSz)
    (x, wX) <- arg tD (\kϵ -> AElem xR (KI xRnk) lX (Tmp kϵ) dSz)
    (lY0, ss0) <- writeF f [rt x0] (IT y0)
    (lY, ss) <- writeF f [rt x] (IT y)
    let xDims=[EAt (ADim xR (KI l) lX) | l <- [0..(xRnk-1)]]
        yDims=[EAt (ADim y0 (KI l) lY0) | l <- [0..(rnk-1)]]
        oRnk=xRnk+rnk
    loop <- afors xSh 0 ILt (Tmp szX) $ \k ->
                wX k:ss++aiR (td,Just a) (y,lY,KI rnk) (Tmp szY) szO
    pure (plX$wX0 undefined:ss0
        ++PlProd () szY yDims
        :PlProd () szX xDims
        :Ma () oSh a t (KI oRnk) (Tmp szX*Tmp szY) szO
            :CpyD () (ADim t 0 (Just a)) (ADim xR 0 lX) (KI xRnk)
            :CpyD () (ADim t (KI xRnk) (Just a)) (ADim y0 0 lY0) (KI rnk)
        :td=:DP t (KI$xRnk+rnk)
        :[loop])
aeval (EApp (Arr oSh _) (EApp _ (Builtin _ Map) f) xs) t a
    | (Arr xSh _) <- eAnn xs
    , Just xRnk <- staRnk xSh
    , Just ((ta0, rnk0), (ta1, rnk1)) <- mAA (eAnn f)
    , Just sz0 <- nSz ta0, Just sz1 <- nSz ta1 = do
    szR <- nI; szY <- nI
    i <- nI; j <- nI; kL <- nI; xd <- nI; td <- nI
    (plX, (lX, xR)) <- plA xs
    (slopP, slopSz, aSlop, pops) <- plSlop sz1 rnk0 (idims rnk0 xRnk xR lX)
    (y0, lY0, ss0) <- writeA f [TA slopP Nothing]
    (y, lY, ss) <- writeA f [TA slopP Nothing]
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
        :PlProd () szY yDims:PlProd () kL xDims
        :i=:0:j=:0:loop
        :[pops])
   | otherwise = unsupported
aeval e t a | (Arr oSh _) <- eAnn e, Just (f, xss) <- r00 e, all isF (unroll$eAnn f), (Arr sh _) <- eAnn (head xss), hasS f = do
    xRds <- nIs xss; tD <- nI
    rnkR <- nI; szR <- nI; i <- nI
    (plXs, (lXs, xRs)) <- second unzip <$> plAs xss
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
    pure (plXs$rnkR=:eRnk sh (xR,lX):SZ () szR xR (Tmp rnkR) lX:Ma () oSh a t (Tmp rnkR) (Tmp szR) 8:CpyD () (ADim t 0 (Just a)) (ADim xR 0 lX) (Tmp rnkR):zipWith (\xRϵ xRd -> xRd=:DP xRϵ (Tmp rnkR)) xRs xRds++tD=:DP t (Tmp rnkR):[loop])
-- TODO: transp-rank
aeval e t a
    | Just (f, xss) <- r00 e
    , Just xsTys <- traverse (fmap fst.aN.eAnn) xss
    , Arr sh _ <- eAnn (head xss)
    , tC <- codT (eAnn f)
    , Just szC <- nSz tC
    , Arr oSh _ <- eAnn e = do
    xRds <- nIs xss; tD <- nI
    rnkR <- nI; szR <- nI
    (plXs, (lXs, xRs)) <- second unzip <$> plAs xss
    let xR=head xRs; lX=head lXs
    step <- aS f (reverse$zipWith3 (\tXϵ xRd lXϵ -> (tXϵ, \iϵ -> Raw xRd (Tmp iϵ) lXϵ)) xsTys xRds lXs) tC (\iϵ -> Raw tD (Tmp iϵ) (Just a))
    loop <- afor sh 0 ILt (Tmp szR) (\i -> step (repeat i) i)
    pure (plXs$rnkR=:eRnk sh (xR,lX):SZ () szR xR (Tmp rnkR) lX:Ma () oSh a t (Tmp rnkR) (Tmp szR) szC:CpyD () (ADim t 0 (Just a)) (ADim xR 0 lX) (Tmp rnkR):zipWith (\xRϵ xRd -> xRd=:DP xRϵ (Tmp rnkR)) xRs xRds++tD=:DP t (Tmp rnkR):[loop])
aeval (EApp (Arr oSh _) (EApp _ (EApp _ (Builtin _ (Rank [(0, _), (cr, Just ixs)])) op) xs) ys) t a
    | Just (yT, yRnk) <- tRnk (eAnn ys), Just (_, xRnk) <- tRnk (eAnn xs)
    , Arrow tX (Arrow _ tC) <- eAnn op, Just cSz <- nSz tC
    , Just xSz <- nSz tX, Just ySz <- nSz yT = do
    (plX, (lX, xR)) <- plA xs; (plY, (lY, yR)) <- plA ys
    zR <- rtemp tC
    let oRnk=yRnk-fromIntegral cr
    (x, pAX) <- arg tX (\ixϵ -> AElem xR (KI xRnk) lX (Tmp ixϵ) xSz)
    (oDims, complts, dps, pinchC, slopP, copyCell) <- loopCell cr ixs (yR, lY) yRnk ySz
    (_, ss) <- writeF op [rt x, TA slopP Nothing] zR
    loop <- aall1 complts (Tmp<$>oDims) $ \ix -> pAX ix:copyCell ++ ss ++ [wt (AElem t (KI oRnk) (Just a) (Tmp ix) cSz) zR]
    m <- mdn oSh t a oRnk oDims cSz
    pure (plX$plY$pinchC$
        [tϵ=:0 | tϵ <- complts]
        ++mt (AElem xR (KI xRnk) lX 0 xSz) x
        :dps++m++loop)
aeval (EApp (Arr oSh _) (EApp _ (EApp _ (Builtin _ (Rank [(0, _), (cr, Just ixs)])) op) xs) ys) t a
    | Just (yT, yRnk) <- tRnk (eAnn ys), Just (_, xRnk) <- tRnk (eAnn xs)
    , (Arrow tX (Arrow _ tCod)) <- eAnn op, Just (tC, opRnk) <- tRnk tCod
    , Just xSz <- nSz tX, Just cSz <- nSz tC, Just ySz <- nSz yT = do
    (plX, (lX, xR)) <- plA xs; (plY, (lY, yR)) <- plA ys
    oSz <- nI; zSz <- nI
    td <- nI
    let oRnk=KI$yRnk+opRnk-fromIntegral cr
    (x, pAX) <- arg tX (\ixϵ -> AElem xR (KI xRnk) lX (Tmp ixϵ) xSz)
    (oDims, complts, ds, pinchC, slopP, copyCell) <- loopCell cr ixs (yR, lY) yRnk ySz
    (zR, lZ, ss) <- writeA op [rt x, TA slopP Nothing]
    loop <- aall1 complts (Tmp<$>oDims) $ \ix -> pAX ix:copyCell++ss++aiR (td,Just a) (zR,lZ,KI opRnk) (Tmp zSz) cSz
    (dots, doss) <- plDim opRnk (zR, lZ)
    pure (plX$plY$pinchC$
        [tϵ=:0 | tϵ <- complts]
        ++mt (AElem xR (KI xRnk) lX 0 xSz) x
        :ds++copyCell
        ++ss++doss
        ++PlProd () zSz (Tmp<$>dots)
        :PlProd () oSz (Tmp<$>(zSz:oDims))
            :md oSh t a oRnk (Tmp oSz) (Tmp<$>(oDims++dots)) cSz
        ++td=:DP t oRnk:loop)
aeval (EApp (Arr oSh _) (EApp _ (Builtin _ (Rank [(cr, Just ixs)])) f) xs) t a
    | Just (tA, rnk) <- tRnk (eAnn xs)
    , (Arrow _ tC) <- eAnn f
    , Just ySz <- nSz tC, Just aSz <- nSz tA = do
    (plX, (lX, xR)) <- plA xs
    let oRnk=rnk-fromIntegral cr
    (oDims, complts, ds, pinchC, slopP, copyCell) <- loopCell cr ixs (xR, lX) rnk aSz
    (y, wY) <- rW tC (iXelem t (KI oRnk) (Just a) ySz)
    (_, ss) <- writeF f [TA slopP Nothing] y
    loop <- aall1 complts (Tmp<$>oDims) $ \di -> copyCell ++ ss ++ [wY di]
    m <- mdn oSh t a oRnk oDims ySz
    pure (plX$pinchC$m++ds++loop)
aeval (EApp (Arr oSh _) (EApp _ (Builtin _ (Rank [(cr, Just ixs)])) f) xs) t a
    | Just (tA, xRnk) <- tRnk (eAnn xs)
    , (Arrow _ tCod) <- eAnn f
    , Just (tC, opRnk) <- tRnk tCod, Just cSz <- nSz tC, Just aSz <- nSz tA = do
    (plX, (lX, xR)) <- plA xs
    ySz <- nI; td <- nI; oSz <- nI
    let oRnk=KI$xRnk+opRnk-fromIntegral cr
    (oDims, complts, ds, pinchC, slopP, copyCell) <- loopCell cr ixs (xR, lX) xRnk aSz
    (yR, lY, ss) <- writeA f [TA slopP Nothing]
    let loop=forAll1 complts (Tmp<$>oDims)
                $ copyCell ++ ss ++ aiR (td,Just a) (yR,lY,KI opRnk) (Tmp ySz) cSz
    (dots, doss) <- plDim opRnk (yR, lY)
    pure (plX$pinchC$
        [tϵ=:0 | tϵ <- complts]
        ++ds++copyCell
        ++ss++doss
        ++PlProd () ySz (Tmp<$>dots)
        :PlProd () oSz (Tmp<$>(ySz:oDims))
            :md oSh t a oRnk (Tmp oSz) (Tmp<$>(oDims++dots)) cSz
        ++td=:DP t oRnk:loop)
aeval (EApp oTy@(Arr oSh _) (EApp _ g@(Builtin _ CatE) x) y) t a | Just (ty, 1) <- tRnk oTy = do
    xnR <- nI; ynR <- nI; tn <- nI
    let sz=bT ty
    (plX, (lX, xR)) <- plA x; (plY, (lY, yR)) <- plA y
    contents <- rfill g (AD t (Just a) Nothing Nothing (Just sz) (Just$Tmp tn)) [AI (AD xR lX Nothing Nothing Nothing (Just$Tmp xnR)), AI (AD yR lY Nothing Nothing Nothing (Just$Tmp ynR))]
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
        loop = For () E.S ɴc j₀ 0 ILt (Tmp n) [
                  fort tA i 0 ILt (Tmp m) $
                      let zr=Raw td (Tmp i) (Just aL) 8 in
                      [ aid=:(Tmp aRd+(Tmp n*Tmp i+Tmp j₀)*8)
                      , xid=:(Tmp xRd+Tmp j₀*8)
                      , MX () z₀ (FAt zr)
                      , Ins () z z₀
                      , For () E.S 2 j 0 ILt ɴc
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
    (prologue, et, ~(Just zs)) <- case pr xSh of E{} -> pure (id, FTmp z0, Nothing); _ -> do {zs <- nF; pure ((MX () zs 0:), FTmp zs+FTmp z0, Just zs)}
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
        loop=For () E.S ᴍ i₀ 0 ILt mE [
                For () E.S ᴏE j₀ 0 ILt oE [
                    For () E.S ɴ k₀ 0 ILt nE [
                      For () E.S 1 i 0 ILt ᴍ
                            [ tid=:(Tmp td+((Tmp i+Tmp i₀)*oE+Tmp j₀)*8)
                            , For () E.S ᴏE j 0 ILt ᴏE $
                                  zipWith (\z₀ toffs -> MX () z₀ (FAt (Raw tid (KI toffs) (Just aL) 8))) z₀s oᴋ
                                ++zipWith (Ins ()) zs z₀s
                                ++[ aid=:(Tmp aRd+((Tmp i₀+Tmp i)*nE+Tmp k₀)*8)
                                  , bid=:(Tmp bRd+((Tmp j₀+Tmp j)*nE+Tmp k₀)*8)
                                  , For () E.S 2 k 0 ILt ɴ $
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
    pure (plAA$plB$md oSh t aL 2 (KI$m*o) [mE,oE] 8
        ++aRd=:DP aR 2:bRd=:DP bR 2:td=:DP t 2
        :[zero,loop])
  where
    tA=eAnn a; tB=eAnn b
    mT n | n `rem` 8==0 && n > 0 = Just 8 | n `rem` 4==0 && n>0 = Just 4 | otherwise = Nothing
    rot1 xs = take (length xs) $ drop 1 $ cycle xs
aeval (EApp (Arr oSh _) (EApp _ (Builtin _ Mul) a) (EApp _ (Builtin _ T) b)) t aL | Arr bSh F <- tB = do
    i <- nI; j <- nI; k <- nI; m <- nI; l <- nI; n <- nI; o <- nI
    z <- nF2; z0 <- nF; za <- nF2; zb <- nF2; za1 <- nF; zb1 <- nF
    aRd <- nI; bRd <- nI; td <- nI
    tid <- nI; bid <- nI; aid <- nI
    (plAA, (lA, aR)) <- plA a; (plB, (lB, bR)) <- plA b
    (prologue, et, ~(Just zs)) <- case pc bSh of E{} -> pure (id, FTmp z0, Nothing); _ -> do {zs <- nF; pure ((MX () zs 0:), FTmp zs+FTmp z0, Just zs)}
    let zero=f2ors oSh l 0 ILt (Tmp m*Tmp o)
                [Wr2F () (Raw td (Tmp l) (Just aL) 8) (ConstF (0,0))]
                [WrF () (Raw td (Tmp l) (Just aL) 8) 0]
        loop=fort tA i 0 ILt (Tmp m)
                [ MT () tid (Tmp td+(Tmp i*Tmp o)*8)
                , forc tB j 0 ILt (Tmp o) $ prologue
                    [ MX2 () z (ConstF (0,0))
                    , MT () aid (Tmp aRd+(Tmp n*Tmp i)*8)
                    , MT () bid (Tmp bRd+(Tmp n*Tmp j)*8)
                    , f2orc bSh k 0 ILt (Tmp n)
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
    al <- mdn oSh t aL 2 [m,o] 8
    pure (plAA$plB$
         m=:ev tA (aR,lA):o=:ev tB (bR,lB):al++n=:ec tA (aR,lA)
        :aRd=:DP aR 2:bRd=:DP bR 2:td=:DP t 2
        :[zero,loop])
  where
    tA=eAnn a; tB=eAnn b
aeval (EApp (Arr oSh _) (EApp _ (Builtin _ Mul) a) b) t aL | Arr bSh F <- tB = do
    m <- nI; n <- nI; o <- nI; i <- nI; j <- nI; k <- nI; l <- nI; zr <- nF2; zr₀ <- nF; z₀ <- nF2; z₁ <- nF2; z₀₀ <- nF; z₁₀ <- nF
    aRd <- nI; bRd <- nI; td <- nI; bid <- nI; bidϵ <- nI
    (plAA, (lA, aR)) <- plA a; (plB, (lB, bR)) <- plA b
    let zero=f2ors oSh l 0 ILt (Tmp m*Tmp o)
                [Wr2F () (Raw td (Tmp l) (Just aL) 8) (ConstF (0,0))]
                [WrF () (Raw td (Tmp l) (Just aL) 8) 0]
        kjloop = f2or bSh k 0 ILt (Tmp n)
                    [ MX () z₀₀ (FAt (Raw aRd (Tmp k) lA 8))
                    , MX () z₁₀ (FAt (Raw aRd (Tmp k+1) lA 8))
                    -- thabove could be a single fetch (dup works on indexed SIMD registers)
                    , DS () z₀ z₀₀, DS () z₁ z₁₀
                    , let za=Raw td (Tmp j) (Just aL) 8 in
                        f2orc bSh j 0 ILt (Tmp o)
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
                        f2orc bSh j 0 ILt (Tmp o)
                          [ Wr2F () za (FBin FPlus (FAt za) (FBin FTimes (FTmp z₀) (FAt (Raw bid (Tmp j) lB 8)))) ]
                          [ WrF () za (FAt za+FTmp z₀₀*FAt (Raw bid (Tmp j) lB 8))]
                    , bid+=(Tmp o*8)
                    , bidϵ+=(Tmp o*8)
                    ]
        loop=fort tA i 0 ILt (Tmp m) [bid=:Tmp bRd, bidϵ=:(Tmp bid+Tmp o*8), kjloop, aRd+=(Tmp n*8), td+=(Tmp o*8)]
    al <- mdn oSh t aL 2 [m,o] 8
    pure (plAA$plB$
         m=:ev tA (aR,lA):o=:ec tB (bR,lB):al++n=:ec tA (aR,lA)
        :aRd=:DP aR 2:bRd=:DP bR 2:td=:DP t 2
        :[zero,loop])
  where
    tA=eAnn a; tB=eAnn b
aeval (EApp (Arr oSh _) (EApp _ (Builtin _ Mul) a) b) t aL | Just (I, _) <- tRnk tA = do
    i <- nI; j <- nI; k <- nI; m <- nI; n <- nI; o <- nI; z <- nI
    aRd <- nI; bRd <- nI; td <- nI
    (plAA, (lA, aR)) <- plA a
    (plB, (lB, bR)) <- plA b
    let loop=fort tA i 0 ILt (Tmp m)
                [forc tB j 0 ILt (Tmp o)
                    [ z=:0, fort tB k 0 ILt (Tmp n)
                          [z=:(Tmp z+EAt (Raw aRd (Tmp n*Tmp i+Tmp k) lA 8)*EAt (Raw bRd (Tmp k*Tmp o+Tmp j) lB 8))]
                    , Wr () (Raw td (Tmp i*Tmp o+Tmp j) (Just aL) 8) (Tmp z)]
                    ]
    al <- mdn oSh t aL 2 [m,o] 8
    pure (plAA$plB$
         m=:ev tA (aR,lA):o=:ec tB (bR,lB):al++n=:ev tB (bR,lB)
        :aRd=:DP aR 2:bRd=:DP bR 2:td=:DP t 2
        :[loop])
  where
    tA=eAnn a; tB=eAnn b
aeval (EApp (Arr oSh _) (EApp _ (Builtin _ VMul) a) x) t aL | Just (I, _) <- tRnk tA = do
    i <- nI; j <- nI; m <- nI; n <- nI; z <- nI
    aRd <- nI; xRd <- nI; td <- nI
    (plAA, (lA, aR)) <- plA a; (plX, (lX, xR)) <- plA x
    let loop = fort tA i 0 ILt (Tmp m)
                  [ z=:0,
                    fort tX j 0 ILt (Tmp n)
                        [ z+=(EAt (Raw aRd (Tmp n*Tmp i+Tmp j) lA 8)*EAt (Raw xRd (Tmp j) lX 8)) ]
                  , Wr () (Raw td (Tmp i) (Just aL) 8) (Tmp z)
                  ]
    pure (plAA$plX$
        n=:ev tX (xR,lX):v8 oSh t aL (Tmp n)
        ++aRd=:DP aR 2:xRd=:DP xR 1:td=:DP t 1
        :m=:ev tA (aR,lA):[loop])
  where
    tA=eAnn a; tX=eAnn x
aeval (EApp (Arr oSh _) (EApp _ g@(Builtin _ ConsE) x) xs) t a | tX <- eAnn x, Just sz <- nSz tX = do
    xR <- rtemp tX; nR <- nI; nϵR <- nI
    plX <- eeval x xR
    (plXs, (l, xsR)) <- plA xs
    contents <- rfill g (AD t (Just a) Nothing Nothing (Just sz) Nothing) [NA xR, AI (AD xsR l Nothing Nothing Nothing (Just$Tmp nϵR))]
    pure (plX++plXs (nϵR =: ev (eAnn xs) (xsR,l):nR =: (Tmp nϵR+1):vSz oSh t a (Tmp nR) sz++contents))
aeval (EApp (Arr oSh _) (EApp _ (Builtin _ ConsE) x) xs) t a | Just (tX, xRnk) <- tRnk (eAnn x), tXs <- eAnn xs, Just (_, xsRnk) <- tRnk tXs = do
    (plX, (lX, xR)) <- plA x; (plXs, (lXs, xsR)) <- plA xs
    (dts,dss) <- plDim xRnk (xR, lX)
    d1R <- nI; d1'R <- nI; szR <- nI; nX <- nI
    let rnkE=KI xsRnk; szX=bT tX
    pure (plXs$plX$d1R=:ev tXs (xsR,lXs):dss++d1'R=:(Tmp d1R+1):PlProd () nX (Tmp<$>dts):szR=:(Tmp d1'R*Tmp nX):Ma () oSh a t rnkE (Tmp szR) szX:Wr () (ADim t 0 (Just a)) (Tmp d1'R):CpyD () (ADim t 1 (Just a)) (ADim xsR 1 lXs) (KI$xsRnk-1):[cpy (AElem t rnkE (Just a) 0) (AElem xR (KI xRnk) lX 0) (Tmp nX) szX, cpy (AElem t rnkE (Just a) (Tmp nX)) (AElem xsR (KI xsRnk) lXs 0) (Tmp d1R*Tmp nX) szX])
                                                           | otherwise = unsupported
aeval (EApp (Arr oSh _) (EApp _ g@(Builtin _ Snoc) x) xs) t a | tX <- eAnn x, Just sz <- nSz tX = do
    xR <- rtemp tX; nR <- nI; nϵR <- nI
    plX <- eeval x xR
    (plXs, (l, xsR)) <- plA xs
    contents <- rfill g (AD t (Just a) Nothing Nothing (Just sz) Nothing) [NA xR, AI (AD xsR l Nothing Nothing Nothing (Just$Tmp nϵR))]
    pure (plXs$plX++nϵR =: ev (eAnn xs) (xsR,l):nR =: (Tmp nϵR+1):vSz oSh t a (Tmp nR) sz++contents)
aeval (EApp (Arr oSh _) (EApp _ (Builtin _ Snoc) x) xs) t a | Just (tX, xRnk) <- tRnk (eAnn x), tXs <- eAnn xs, Just (_, xsRnk) <- tRnk tXs = do
    (plX, (lX, xR)) <- plA x; (plXs, (lXs, xsR)) <- plA xs
    (dts,dss) <- plDim xRnk (xR, lX)
    d1R <- nI; d1'R <- nI; szR <- nI; nX <- nI
    let rnkE=KI xsRnk; szX=bT tX
    pure (plXs$plX$d1R=:ev tXs (xsR,lXs):dss++d1'R=:(Tmp d1R+1):PlProd () nX (Tmp<$>dts):szR=:(Tmp d1'R*Tmp nX):Ma () oSh a t rnkE (Tmp szR) szX:Wr () (ADim t 0 (Just a)) (Tmp d1'R):CpyD () (ADim t 1 (Just a)) (ADim xsR 1 lXs) (KI$xsRnk-1):[cpy (AElem t rnkE (Just a) (Tmp d1R*Tmp nX)) (AElem xR (KI xRnk) lX 0) (Tmp nX) szX, cpy (AElem t rnkE (Just a) 0) (AElem xsR (KI xsRnk) lXs 0) (Tmp d1R*Tmp nX) szX])
                                                          | otherwise = unsupported
aeval (EApp (Arr oSh _) (EApp _ (Builtin _ Re) n) x) t a | (Arr sh tO) <- eAnn x, sz <- bT tO = do
    (plN, nR) <- plEV n; (plX, (lX, xR)) <- plA x
    xRnk <- nI; oRnk <- nI; td <- nI; xRd <- nI; szX <- nI
    loop <- afor oSh 0 ILt (Tmp nR) $ \k -> [cpy (Raw td (Tmp k*Tmp szX) (Just a)) (Raw xRd 0 lX) (Tmp szX) sz]
    pure (plX$xRnk=:eRnk sh (xR,lX):oRnk=:(Tmp xRnk+1):SZ () szX xR (Tmp xRnk) lX
        :plN (Ma () oSh a t (Tmp oRnk) (Tmp szX*Tmp nR) sz:Wr () (ADim t 0 (Just a)) (Tmp nR):CpyD () (ADim t 1 (Just a)) (ADim xR 0 lX) (Tmp xRnk)
        :td=:DP t (Tmp oRnk):xRd=:DP xR (Tmp xRnk):[loop]))
aeval (EApp (Arr oSh _) (EApp _ (EApp _ (Builtin _ Zip) op) xs) ys) t a | Arrow F (Arrow F F) <- eAnn op, tXs@(Arr xSh _) <- eAnn xs, hasS op = do
    nR <- nI; i <- nI
    (plEX, (lX, xR)) <- plA xs; (plEY, (lY, yR)) <- plA ys
    xRd <- nI; yRd <- nI; td <- nI
    x <- nF2; y <- nF2; z <- nF2; x0 <- nF; y0 <- nF; z0 <- nF
    ss <- write2 op [x,y] z
    s1 <- writeRF op (FT<$>[x0,y0]) (FT z0)
    let step=MX2 () x (FAt (Raw xRd 0 lX 8)):xRd+=16:MX2 () y (FAt (Raw yRd 0 lY 8)):yRd+=16:ss++[Wr2F () (Raw td 0 (Just a) 8) (FTmp z), td+=16]
        step1=MX () x0 (FAt (Raw xRd 0 lX 8)):xRd+=8:MX () y0 (FAt (Raw yRd 0 lY 8)):yRd+=8:s1++[WrF () (Raw td 0 (Just a) 8) (FTmp z0), td+=8]
        loop=r2of xSh i (Tmp nR) step step1
    pure (plEX$plEY$nR=:ev tXs (xR,lX):v8 oSh t a (Tmp nR)++xRd=:DP xR 1:yRd=:DP yR 1:td=:DP t 1:[loop])
aeval (EApp oTy@(Arr sh _) (EApp _ g@(EApp _ (Builtin _ Zip) op) xs) ys) t a | (Arrow tX (Arrow tY tC)) <- eAnn op, Just zSz <- nSz tC, nind tX && nind tY = do
    nR <- nI
    (plEX, (lX, aPX)) <- plA xs; (plEY, (lY, aPY)) <- plA ys
    contents <- fill g (AD t (Just a) (Just oTy) Nothing Nothing Nothing) [AI (AD aPX lX Nothing Nothing Nothing (Just$Tmp nR)), AI (AD aPY lY Nothing Nothing Nothing Nothing)]
    pure (plEX$plEY$nR =: ev (eAnn xs) (aPX,lX):vSz sh t a (Tmp nR) zSz++[contents])
aeval (EApp (Arr sh _) (EApp _ (EApp _ (Builtin _ Zip) op) xs) ys) t a
    | Arrow tX (Arrow tY tC) <- eAnn op
    , (Arr ySh _) <- eAnn ys, tXs@(Arr xSh _) <- eAnn xs
    , Just (tXE, slopRnk) <- tRnk tX
    , Just szX <- nSz tXE, Just szY <- nSz tY, Just szC <- nSz tC
    , Just rnk <- staRnk sh, Just yRnk <- staRnk ySh, Just xRnk <- staRnk xSh = do
    nR <- nI
    y <- rtemp tY; z <- rtemp tC
    (plX, (lX, xsR)) <- plA xs; (plY, (lY, ysR)) <- plA ys
    (slopP, slopN, aSlop, pops) <- plSlop szX slopRnk (idims slopRnk xRnk xsR lX)
    (dts,dss) <- plDim yRnk (ysR, lY)
    m <- mdn sh t a rnk dts szC
    (_, ss) <- writeF op [TA slopP Nothing, rt y] z
    loop <- afor sh 0 ILt (Tmp nR) $ \k -> cpy (AElem slopP (KI slopRnk) Nothing 0) (AElem xsR (KI xRnk) lX (Tmp k*Tmp slopN)) (Tmp slopN) szX:mt (AElem ysR (KI yRnk) lY (Tmp k) szY) y:ss++[wt (AElem t (KI rnk) (Just a) (Tmp k) szC) z]
    pure (plX$plY$nR=:ev tXs (xsR, lX):dss++m++aSlop++[loop, pops])
    | otherwise = unsupported
aeval (EApp (Arr oSh _) (EApp _ g@(EApp _ (Builtin _ ScanS) op) seed) e) t a | (Arrow tX (Arrow tY _)) <- eAnn op, Just xSz <- rSz tX, nind tY = do
    acc <- rtemp tX; n <- nI
    plS <- eeval seed acc
    (plE, (l, aP)) <- plA e
    loop <- fill g (AD t (Just a) Nothing Nothing Nothing (Just$Tmp n)) [NA acc, AI (AD aP l (Just tXs) Nothing Nothing Nothing)]
    pure (plE$n =: (ev tXs (aP,l)+1):vSz oSh t a (Tmp n) xSz++plS++[loop])
  where
    tXs=eAnn e
aeval (EApp (Arr oSh _) (EApp _ g@(EApp _ (Builtin _ ScanS) op) seed) e) t a | (Arrow tX (Arrow tY _)) <- eAnn op, isΠ tX, xSz <- bT tX, nind tY = do
    n <- nI
    (plS,as) <- plΠ seed
    (plE, (l, aP)) <- plA e
    -- TODO: tup-of-arrays would get discarded hm
    loop <- fill g (AD t (Just a) Nothing Nothing Nothing (Just$Tmp n)) [NA (ΠT (tr<$>as)), AI (AD aP l (Just tXs) Nothing (Just xSz) Nothing)]
    pure (plE$n =: (ev tXs (aP,l)+1):vSz oSh t a (Tmp n) xSz++plS++[loop])
  where
    tXs=eAnn e
aeval (EApp oTy@(Arr sh _) g@(EApp _ (Builtin _ Scan) op) xs) t a | (Arrow tAcc (Arrow tX _)) <- eAnn op, Just accSz <- rSz tAcc, Just xSz <- rSz tX = do
    acc <- rtemp tAcc; x <- rtemp tX; n <- nI
    (plE, (l, aP)) <- plA xs
    contents <- rfill g (AD t (Just a) (Just oTy) Nothing (Just accSz) (Just$Tmp n)) [AI (AD aP l Nothing Nothing (Just xSz) Nothing), NA acc, NA x]
    pure (plE$n =: ev (eAnn xs) (aP,l):vSz sh t a (Tmp n) accSz++contents)
aeval (EApp oTy@(Arr oSh _) (EApp _ (Builtin _ (DI n)) op) xs) t a | Just (ot, oSz) <- aBs oTy, tXs <- eAnn xs, Just xSz <- aB tXs = do
    szR <- nI; sz'R <- nI; fR <- rtemp ot
    (slopP, aSlop, pops) <- vslop xSz n
    td <- nI
    (_, ss) <- writeF op [TA slopP Nothing] fR
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
    (z0R, lZ0, ss0) <- writeA op [TA slopP Nothing]
    (zR, lZ, ss) <- writeA op [TA slopP Nothing]
    (dots, plOds) <- plDim cRnk (z0R, lZ0)
    loop <- afor oSh 0 ILt (Tmp d1) $ \i ->
                aiA slopPd (xRd,lX) (Tmp i) neϵ szX
                :ss++aiR (td,Just a) (zR,lZ,KI cRnk) (Tmp nC) szO
    pure (plX$
        d1x=:ev tXs (xR,lX)
        :d1=:(Tmp d1x-fromIntegral(n-1))
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
aeval (Id (Arr sh at) (AShLit ns es)) t a | Just (ty,sz) <- nr at, Just{} <- traverse (nr.eAnn) es = do
    let rnk=genericLength ns; n=fromIntegral$product ns
    tt <- rtemp ty
    plEs <- zipWithM (\e i -> do {pl <- eeval e tt; pure $ pl ++ [wt (AElem t rnk (Just a) (KI i) sz) tt]}) es [0..]
    pure (md sh t a rnk n (fromIntegral<$>ns) sz++concat plEs)
aeval (Id (Arr sh at) (AShLit ns es@(e0:_))) t a | Just sz <- rSz at, Arr sh0 _ <- t0, Just irnk <- staRnk sh0 = do
    (r0,l0,pl0) <- maa e0
    (ts,ss) <- plDim irnk (r0,l0); n <- nI
    let rnk=KI$genericLength ns+irnk; on=fromIntegral$product ns; nn=Tmp n; tt=Tmp<$>ts
    plEs <- zipWithM (\e i -> do {(r,l,pl) <- maa e; pure $ pl ++ [cpy (AElem t rnk (Just a) (KI i*nn)) (AElem r (KI irnk) l 0) nn sz]}) es [0..]
    pure (pl0++ss++PlProd () n tt:md sh t a rnk (on*nn) (map fromIntegral ns++tt) sz++concat plEs)
  where t0=eAnn e0
aeval (EApp _ (Builtin _ T) x) t a | Arr sh ty <- eAnn x, Just rnk <- staRnk sh = do
    let sze=bT ty; dO=KI$8+8*rnk
    xd <- nI; td <- nI
    (plX, (l, xR)) <- plA x
    (dts, plDs) <- plDim rnk (xR, l)
    (sts, plSs) <- offByDim (reverse dts)
    (std, plSd) <- offByDim dts
    let _:sstrides = sts; (_:dstrides) = std
    is <- nIs [1..rnk]
    let loop=thread (zipWith (\i tt -> (:[]) . For () E.Z 1 i 0 ILt (Tmp tt)) is dts) [mv (At td (Tmp<$>dstrides) (Tmp<$>reverse is) (Just a)) (At xd (Tmp<$>sstrides) (Tmp<$>is) l) sze]
    pure (plX$plDs++init plSs++Ma () sh a t (KI rnk) (Tmp (head dts)*Tmp (head sstrides)) sze:diml (t, Just a) (Tmp<$>reverse dts)++init plSd++xd =: (Tmp xR+dO):td =: (Tmp t+dO):loop)
                                 | otherwise = unsupported
aeval (EApp (Arr oSh _) (EApp _ g@(EApp _ (Builtin _ Outer) op) xs) ys) t a
    | (Arrow tX (Arrow tY tC)) <- eAnn op, Just zSz <- nSz tC
    , nind tX && nind tY = do
    szX <- nI; szY <- nI
    (plX, (lX, xR)) <- plA xs; (plY, (lY, yR)) <- plA ys
    contents <- rfill g (AD t (Just a) Nothing Nothing (Just zSz) Nothing) [AI (AD xR lX (Just tXs) Nothing Nothing (Just$Tmp szX)), AI (AD yR lY (Just tYs) Nothing Nothing (Just$Tmp szY))]
    m <- mdn oSh t a 2 [szX, szY] zSz
    pure (plX$plY$szX =: ev tXs (xR,lX):szY =: ev tYs (yR,lY):m++contents)
  where
    tXs = eAnn xs; tYs=eAnn ys
aeval (EApp (Arr oSh _) (EApp _ (EApp _ (Builtin _ Outer) op) xs) ys) t a
    | (Arrow tX (Arrow tY tC)) <- eAnn op
    , Arr sh tEC <- tC
    , Just [szXT,szYT,szZT] <- traverse nSz [tX,tY,tEC] = do
    td <- nI; szX <- nI; szY <- nI; szZ <- nI; i <- nI; j <- nI; rnkZ <- nI; rnkO <- nI
    (plX, (lX, xR)) <- plA xs; (plY, (lY, yR)) <- plA ys
    (x, wX) <- arg tX (ve xR lX szXT)
    (y, wY) <- arg tY (ve yR lY szYT)
    (z0, lZ0, ss0) <- writeA op [rt x, rt y]
    (z, lZ, ss) <- writeA op [rt x, rt y]
    let step=[wX i, wY j]++ss++aiR (td,Just a) (z,lZ,Tmp rnkZ) (Tmp szZ) szZT
        loop=fort tXs i 0 ILt (Tmp szX) [fort tYs j 0 ILt (Tmp szY) step]
    pure (plX$plY$
        i=:0:j=:0:
          wX i:wY j:ss0
        ++rnkZ=:eRnk sh (z0,lZ0):rnkO=:(Tmp rnkZ+2)
        :SZ () szZ z0 (Tmp rnkZ) lZ0
        :szX=:ev tXs (xR,lX):szY=:ev tYs (yR,lY)
        :Ma () oSh a t (Tmp rnkO) (Tmp szX*Tmp szY*Tmp szZ) szZT
        :diml (t, Just a) [Tmp szX, Tmp szY]
        ++[CpyD () (ADim t 2 (Just a)) (ADim z0 0 lZ0) (Tmp rnkZ), td=:DP t (Tmp rnkO), loop]
        )
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
    (z, wZ) <- rW tC (iXelem t (KI oRnk) (Just a) szZ)
    (_, ss) <- writeF op [TA slopX Nothing, TA slopY Nothing] z
    let loop = [ aiA slopXd (xd,lX) (Tmp i*Tmp nXe) (Tmp nXe) szXT
               , aiA slopYd (yd,lY) (Tmp j*Tmp nYe) (Tmp nYe) szYT
               ] ++ ss ++ [wZ di, di+=1]
    (dtxs,dxss) <- plDim (xRnk-xERnk) (xR,lX)
    (dtys,dyss) <- plDim (yRnk-yERnk) (yR,lY)
    pure (plX$plY$dxss++dyss
        ++PlProd () nX (Tmp<$>dtxs):PlProd () nY (Tmp<$>dtys)
        :md oSh t a (KI oRnk) (Tmp nX*Tmp nY) (Tmp<$>(dtxs++dtys)) szZ
        ++plSlopX++plSlopY
          ++[ slopXd=:DP slopX (KI xERnk), slopYd=:DP slopY (KI yERnk)
            , xd=:DP xR (KI xRnk), yd=:DP yR (KI yRnk)
            , di=:0, For () E.Z 1 i 0 ILt (Tmp nX) [For () E.Z 1 j 0 ILt (Tmp nY) loop]
            , popSlopX, popSlopY])
  where
    tXs=eAnn xs; tYs=eAnn ys
aeval (EApp oTy@(Arr sh _) g@(EApp _ (Builtin _ Succ) op) xs) t a | Arrow tX (Arrow _ tZ) <- eAnn op, Just zSz <- nSz tZ, nind tX = do
    szR <- nI; sz'R <- nI
    (plX, (lX, xR)) <- plA xs
    loop <- fill g (AD t (Just a) (Just oTy) Nothing Nothing (Just$Tmp sz'R)) [AI (AD xR lX Nothing Nothing Nothing Nothing)]
    pure (plX$szR =: ev (eAnn xs) (xR,lX):sz'R =: (Tmp szR-1):vSz sh t a (Tmp sz'R) zSz++[loop])
aeval (EApp oTy@(Arr oSh _) g@(Builtin _ RevE) e) t a | Just sz <- aB oTy = do
    n <- nI
    (plE, (lE, eR)) <- plA e
    contents <- rfill g (AD t (Just a) (Just oTy) Nothing (Just sz) Nothing) [AI$AD eR lE Nothing Nothing Nothing (Just$Tmp n)]
    pure (plE$n =: ev oTy (eR,lE):vSz oSh t a (Tmp n) sz++contents)
aeval (EApp _ (Builtin _ RevE) e) t a
    | tyXs@(Arr sh ty) <- eAnn e
    , Just sz <- nSz ty, Just rnk <- staRnk sh = do
    n <- nI; szA <- nI
    (plE, (lE, eR)) <- plA e
    let rnkE=KI rnk
    (dts, plDs) <- plDim rnk (eR, lE)
    loop <- afor sh 0 ILt (Tmp n) $ \i -> [cpy (AElem t rnkE (Just a) (Tmp i*Tmp szA)) (AElem eR rnkE lE ((Tmp n-Tmp i-1)*Tmp szA)) (Tmp szA) sz]
    pure (plE$n=:ev tyXs (eR,lE):tail plDs++PlProd () szA (Tmp<$>tail dts):Ma () sh a t rnkE (Tmp n*Tmp szA) sz:CpyD () (ADim t 0 (Just a)) (ADim eR 0 lE) rnkE:[loop])
                                    | otherwise = unsupported
aeval (EApp (Arr sh tX) (EApp _ (EApp _ (Builtin _ Ug) g) seed) n) t a
    | tyS <- eAnn seed
    , Just sz <- rSz tX
    , Arrow _ (P tys@[_, tC1]) <- eAnn g
    , Just sze <- rSz tC1 = do
    (plN,nR) <- plEV n
    acc <- rtemp tyS; tts <- traverse rtemp tys
    plSeed <- eeval seed acc
    ss <- writeRF g [acc] (ΠT tts)
    let [next,x]=tts
    loop <- afor sh 0 ILt (Tmp nR) $ \i -> ss++[mvrt acc next, wt (AElem t 1 (Just a) (Tmp i) sze) x]
    pure $ plN (vSz sh t a (Tmp nR) sz++plSeed++[loop])
  where
    mvrt (IT i0) (IT i1) = i0=:Tmp i1; mvrt (FT x0) (FT x1) = MX () x0 (FTmp x1); mvrt (PT b0) (PT b1) = MB () b0 (Is b1)
aeval (EApp (Arr sh _) (EApp _ (EApp _ (Builtin _ Gen) seed) op) n) t a | tyS <- eAnn seed, Just sz <- rSz tyS = do
    acc <- rtemp tyS
    plS <- eeval seed acc
    td <- nI
    (plN, nR) <- plEV n
    ss <- writeRF op [acc] acc
    loop <- arof sh (Tmp nR) $ wt (Raw td 0 (Just a) sz) acc:td+=KI sz:ss
    pure (plN$vSz sh t a (Tmp nR) sz++plS++td=:DP t 1:[loop])
aeval (EApp (Arr sh _) (EApp _ (EApp _ (Builtin _ Gen) seed) op) n) t a | seedTy@P{} <- eAnn seed, Just πsz <- nSz seedTy = do
    (plN, nE) <- plC n
    (plS,as) <- plΠ seed
    td <- nI; as0 <- frts as
    -- TODO: discards arrays
    (_, ss) <- writeF op [TΠ as] (ΠT (tr<$>as0))
    loop <- arof sh nE $ WrT () (Raw td 0 (Just a) πsz) as:td+=KI πsz:ss++mvts as as0
    pure (plN$vSz sh t a nE πsz++plS++td=:DP t 1:[loop])
aeval (EApp (Arr oSh _) (EApp _ (EApp _ (Builtin _ Gen) seed) op) n) t a | Arr xSh tX <- eAnn seed, Just xSz <- nSz tX = do
    (plN, nE) <- plC n
    (seedR, lSeed, plSeed) <- maa seed
    x <- nI; lX <- nextArr x
    (y, lY, ss) <- writeA op [TA x (Just lX)]
    rnk <- nI; rnkX <- nI; nX <- nI
    let xRnkE=Tmp rnkX; rnkE=Tmp rnk; nXe=Tmp nX
        l1=ss++[cpy (AElem y xRnkE lY 0) (AElem x xRnkE (Just lX) 0) nXe xSz, cpy (AElem t rnkE (Just a) 0) (AElem y xRnkE lY 0) nXe xSz]
    loop <- afor oSh 1 ILt nE $ \k -> cpy (AElem x xRnkE (Just lX) 0) (AElem y xRnkE lY 0) (Tmp nX) xSz:ss++[cpy (AElem t rnkE (Just a) (Tmp k*nXe)) (AElem y xRnkE lY 0) nXe xSz]
    pure $plN$plSeed
        ++rnkX=:eRnk xSh (seedR,lSeed):SZ () nX seedR xRnkE lSeed:rnk=:(xRnkE+1)
        :Ma () oSh a t rnkE (nXe*nE) xSz:Wr () (ADim t 0 (Just a)) nE:CpyD () (ADim t 1 (Just a)) (ADim seedR 0 lSeed) xRnkE
        :Ma () xSh lX x xRnkE nXe xSz:CpyD () (ADim x 0 (Just lX)) (ADim seedR 0 lSeed) xRnkE:cpy (AElem x xRnkE (Just lX) 0) (AElem seedR xRnkE lSeed 0) nXe xSz
        :l1++[loop]
-- also (%.)/(re: 5 ⟨⟨1,1⟩,⟨1,0::int⟩⟩) would be nice
aeval (EApp (Arr oSh _) (EApp _ (EApp _ (Builtin _ Fib) seed) op) n) t a | Just (ty,sz) <- aN tSeed = do
    (plN, nE) <- plC n
    (plX, (lX, xR)) <- plA seed; kϵ <- nI
    (y, wRet) <- rW ty (ve t (Just a) sz)
    (_, ss) <- writeF op [TA t (Just a)] y
    loop <- arof oSh nE $ Wr () (ADim t 0 (Just a)) (Tmp kϵ):ss++[wRet kϵ, kϵ+=1]
    pure (plX$kϵ=:ev tSeed (xR,lX):plN (vSz oSh t a (nE+Tmp kϵ) sz++cpy (AElem t 1 (Just a) 0) (AElem xR 1 lX 0) (Tmp kϵ) sz:[loop]))
  where
    tSeed=eAnn seed
aeval (EApp (Arr oSh _) (EApp _ (Builtin _ (Conv as)) f) x) t a
    | (Arrow _ tC) <- eAnn f
    , Just (tX, xRnk) <- tRnk (eAnn x)
    , Just oRnk <- staRnk oSh
    , Just oSz <- nSz tC, Just xSz <- nSz tX, oRnk==xRnk = do
    xRd <- nI; slopP <- nI
    (plX, (lX, xR)) <- plA x
    (dts, plDs) <- plDim xRnk (xR, lX)
    (sts, plS) <- offByDim (reverse dts)
    let _:strides = sts; sss=init plS
    (tdims, dims) <- unzip <$> zipWithM (\dt (i,d) -> do {odim <- nI; pure (odim, odim =: (Bin Op.IDiv (Tmp dt-fromIntegral i) (maybe 1 fromIntegral d)+1))}) dts as
    (tb,bs) <- unzip <$> zipWithM (\dt i -> do {b <- nI; pure (b, b =: (Tmp dt-fromIntegral(i-1)))}) dts (fst<$>as)
    m <- mdn oSh t a oRnk tdims xSz
    io <- nIs tdims; iw <- nIs is
    let slopSz=fromIntegral$product isi; slopRnk=genericLength isi; slopB=slopSz*xSz+(slopRnk+1)*8
        rnk=KI oRnk
    z <- rtemp tC; o <- rtemp tX
    (_, ss) <- writeF f [TA slopP Nothing] z
    extrWindow <- aall1 iw is $ \j ->
                            [ mt (At xRd (Tmp<$>strides) (zipWith (+) (Tmp<$>iw) (Tmp<$>io)) lX xSz) o
                            , wt (AElem slopP (KI slopRnk) Nothing (Tmp j) xSz) o
                            ]
    loop <- aall io ds (Tmp<$>tb) $ \k -> extrWindow++ss++[wt (AElem t rnk (Just a) (Tmp k) oSz) z]
    pure (plX$plDs++dims++sss
        ++sac slopP slopB:Wr () (ARnk slopP Nothing) (KI slopRnk):diml (slopP, Nothing) is
        ++m++xRd=:DP xR (KI xRnk):bs++loop
        ++[popc slopB])
  where (isi,dsi)=unzip as; is=fromIntegral<$>isi; ds=maybe 1 fromIntegral<$>dsi
aeval (EApp (Arr oSh _) (EApp _ (Builtin _ (Conv as)) f) x) t a
    | Just (_, (tC, cRnk)) <- mAA (eAnn f)
    , Just (tX, xRnk) <- tRnk (eAnn x)
    , Just oRnk <- staRnk oSh
    , Just zSz <- nSz tC, Just xSz <- nSz tX = do
    xRd <- nI; td <- nI; nO <- nI; nZ <- nI; slopP <- nI
    (plX, (lX, xR)) <- plA x
    (dts, plDs) <- plDim xRnk (xR, lX)
    (tdims, dims) <- unzip <$> zipWithM (\dt (i,d) -> do {odim <- nI; pure (odim, odim =: (Bin Op.IDiv (Tmp dt-fromIntegral i) (maybe 1 fromIntegral d)+1))}) dts as
    (sts, plS) <- offByDim (reverse dts)
    let _:strides = sts; sss=init plS
    (tb,bs) <- unzip <$> zipWithM (\dt i -> do {b <- nI; pure (b, b =: (Tmp dt-fromIntegral(i-1)))}) dts (fst<$>as)
    io <- nIs tdims; iw <- nIs is
    let slopSz=fromIntegral$product isi; slopRnk=genericLength isi; slopRnkE=KI slopRnk; slopB=slopSz*xSz+(1+slopRnk)*8; rnk=KI oRnk; nE=Tmp nZ
    o <- rtemp tX
    (z0, lZ0, ss0) <- writeA f [TA slopP Nothing]
    (z, lZ, ss) <- writeA f [TA slopP Nothing]
    (dots, plOds) <- plDim cRnk (z0, lZ0)
    extrWindow <- aall1 iw is $ \j ->
                            [ mt (At xRd (Tmp<$>strides) (zipWith (+) (Tmp<$>iw) (Tmp<$>io)) lX xSz) o
                            , wt (AElem slopP slopRnkE Nothing (Tmp j) xSz) o
                            ]
    loop <- aall io ds (Tmp<$>tb) $ \k -> extrWindow++ss++[cpy (AElem t rnk (Just a) (Tmp k*Tmp nO)) (AElem z (KI cRnk) lZ 0) nE zSz]
    pure (plX$
        plDs++dims++sss
        ++sac slopP slopB:Wr () (ARnk slopP Nothing) slopRnkE:diml (slopP, Nothing) is
        ++xRd=:DP xR (KI xRnk)
        :[ioϵ=:0 | ioϵ <- io]++extrWindow++ss0
        ++plOds
        ++PlProd () nO (Tmp<$>dots)
        :PlProd () nZ (Tmp<$>nO:tdims):md oSh t a rnk nE (Tmp<$>(tdims++dots)) zSz
        ++td=:DP t rnk:bs++loop
        ++[popc slopB])
  where (isi,dsi)=unzip as; is=fromIntegral<$>isi; ds=maybe 1 fromIntegral<$>dsi
aeval e _ _ = nyi e

plR :: E (T ()) -> CM ([CS ()] -> [CS ()], RT)
plR e = case eAnn e of
    I   -> second IT <$> plEV e
    F   -> second FT <$> plF e
    B   -> second PT <$> plBV e
    P{} -> bimap (\cs -> (cs++)) (ΠT . map tr) <$> plΠ e

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

plΠ :: E (T ()) -> CM ([CS ()], TStore)
plΠ e = do {as <- πts e; ss <- πr e as; pure (ss,as)}

plAs :: [E (T ())] -> CM ([CS ()] -> [CS ()], [(Maybe AL, Temp)])
plAs = fmap (first thread.unzip).traverse plA

peval :: E (T ()) -> BTemp -> CM [CS ()]
peval (LLet _ b e) t = do
    ss <- llet b
    (ss++) <$> peval e t
peval (BLit _ b) t = pure [MB () t (BConst b)]
peval (Var _ x) t = do
    st <- gets pvars
    pure [MB () t (Is $ getT st x)]
peval (Id _ (Aɴ xs ns)) t | Arr sh _ <- eAnn xs, Just rnk <- staRnk sh = do
    (plX, (lX, xR)) <- plA xs
    (plNs, nEs) <- first thread.unzip <$> traverse plC ns
    xRd <- nI
    (plB, b) <- off xR lX nEs
    pure $ plX $ plNs (plB++[xRd=:DP xR (KI rnk), MB () t (PAt (Raw xRd b lX 1))])
                          | otherwise = unsupported
peval (EApp _ (Builtin _ T) e) t = peval e t; peval (EApp _ (Builtin _ Flat) e) t = peval e t
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
peval (EApp _ (EApp _ (Builtin (Arrow (Arr _ ty) _) Eq) e0) e1) t | Arr sh _ <- eAnn e0, nind ty =do
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
peval (EApp _ (EApp _ (Builtin (Arrow B _) Neq) e0) e1) t = do
    (pl0,e0R) <- plP e0; (pl1,e1R) <- plP e1
    pure $ pl0 $ pl1 [MB () t (Boo XorB e0R e1R)]
peval (EApp _ (EApp _ (Builtin (Arrow B _) Gt) e0) e1) t = do
    (pl0,e0R) <- plP e0; (pl1,e1R) <- plP (EApp B (Builtin (B~>B) N) e1)
    pure $ pl0 $ pl1 [MB () t (Boo AndB e0R e1R)]
peval (EApp _ (EApp _ (Builtin (Arrow B _) Gte) e0) e1) t = do
    (pl0,e0R) <- plP e0; (pl1,e1R) <- plP (EApp B (Builtin (B~>B) N) e1)
    pure $ pl0 $ pl1 [MB () t (Boo OrB e0R e1R)]
peval (EApp _ (EApp _ (Builtin (Arrow B _) Lt) e0) e1) t = do
    (pl0,e0R) <- plP (EApp B (Builtin (B~>B) N) e0); (pl1,e1R) <- plP e1
    pure $ pl0 $ pl1 [MB () t (Boo AndB e0R e1R)]
peval (EApp _ (EApp _ (Builtin (Arrow B _) Lte) e0) e1) t = do
    (pl0,e0R) <- plP (EApp B (Builtin (B~>B) N) e0); (pl1,e1R) <- plP e1
    pure $ pl0 $ pl1 [MB () t (Boo OrB e0R e1R)]
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
    (x, wX) <- arg tY (ve aP l szY)
    ss <- writeRF op [PT acc, x] (PT acc)
    loop <- afort tXs 0 ILt (Tmp szR) (\i -> wX i:ss)
    pure $ plE $ plAcc++szR=:ev (eAnn e) (aP,l):[loop]
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
    pure $ plU ++ plN (plSeeds ++ [For () E.Z 1 k 0 ILt nE (fss++usss)])
peval e@(EApp _ (Builtin _ TAt{}) Var{}) t = do
    aa <- tat e
    pure [MB () t (unBA aa)]
peval (EApp _ (Builtin _ (TAt i)) (Tup _ es)) t = peval (es!!(i-1)) t
peval (EApp _ (Builtin _ (TAt i)) e) t = do
    (ss, as) <- plΠ e
    pure (ss++[MB () t (unBA (as!!(i-1)))])
peval (Id _ (FoldGen seed g f n)) t = do
    x <- nBT; acc <- nBT; k <- nI
    (plSeed,seedR) <- plBV seed; (plN,nE) <- plC n
    uss <- writeRF g [PT x] (PT x)
    fss <- writeRF f [PT acc, PT x] (PT acc)
    pure $ plSeed $ plN ([MB () acc (Is seedR), MB () x (Is seedR)] ++ uss ++ [Rof () E.Z k (nE-1) (fss++uss), MB () t (Is acc)])
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
    (x, wX) <- arg tY (ve aP l szY)
    ss <- writeRF op [PT acc, x] (PT acc)
    loop <- afort tXs 0 ILt (Tmp szR) (\i -> wX i:ss)
    pure $ plE $ plAcc++szR=:ev (eAnn e) (aP,l):[loop]
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
    pure $ plU ++ plN (plSeeds ++ [For () E.Z 1 k 0 ILt nE (fss++usss)])
peval e _ = nyi e

eval :: E (T ()) -> Temp -> CM [CS ()]
eval (LLet _ b e) t = do
    ss <- llet b
    (ss++) <$> eval e t
eval (ILit _ n) t = pure [t =: fromInteger n]
eval (Var _ x) t = do
    st <- gets vars
    pure [t =: Tmp (getT st x)]
eval (Cond _ (EApp _ (EApp _ (Builtin (Arrow I _) op) c0) c1) (ILit _ 0) (ILit _ 1)) t | Just cmp <- rel op = do
    (plC0,c0e) <- plC c0; (plC1,c1e) <- plC c1
    pure $ plC0 $ plC1 [CsetI () (IRel cmp c0e c1e) t]
eval (Cond _ b (ILit _ 0) (ILit _ 1)) t = do
    (plB,eB) <- plP b
    pure $ plB [CsetI () eB t]
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
    (x, wX) <- arg tX (ve eR l xSz)
    ss <- writeRF op [IT acc, x] (IT acc)
    loop <- afort tArr 0 ILt (Tmp szR) (\i -> wX i:ss)
    pure $ plE$plAcc++szR =: ev tArr (eR,l):[loop]
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
eval (EApp _ (Builtin _ Neg) e) t = do
    (plE,i) <- plC e
    pure $ plE [t=:negate i]
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
eval (Id _ (Aɴ xs ns)) t | Arr sh _ <- eAnn xs, Just rnk <- staRnk sh = do
    (plX, (lX, xR)) <- plA xs
    (plNs, nEs) <- first thread.unzip <$> traverse plC ns
    xRd <- nI
    (plB, b) <- off xR lX nEs
    pure $ plX $ plNs (plB++[xRd=:DP xR (KI rnk), t =: EAt (Raw xRd b lX 8)])
                         | otherwise = unsupported
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
eval (EApp _ (EApp _ (Builtin _ IntExp) (ILit _ (-1))) n) t = do
    (plRϵ,nR) <- plEV n
    pure $ plRϵ [t=:1, Cmov () (IUn IOdd (Tmp nR)) t (IU INeg (Tmp t))]
eval (EApp _ (EApp _ (Builtin _ IntExp) x) n) t = do
    xR <- nI; nR <- nI
    plX <- eval x xR; plN <- eval n nR
    pure $ plX ++ plN ++ [t=:1, While () nR IGt 0 [Ifn't () (IUn IEven (Tmp nR)) [t=:(Tmp t*Tmp xR)], nR =: Bin IAsr (Tmp nR) 1, MT () xR (Tmp xR*Tmp xR)]]
eval (EApp _ (Builtin _ T) x) t = eval x t; eval (EApp _ (Builtin _ Flat) x) t = eval x t
eval (EApp _ (Builtin _ Abs) x) t = do {(plX,e) <- plC x; pure (plX [t=:abs e])}
eval (EApp _ (Builtin _ Floor) x) t = do {(plX,e) <- plD x; pure (plX [t =: CFloor e])}
eval (EApp _ (Builtin _ Ceil) x) t = do {(plX, e) <- plD x; pure (plX [t =: CCeil e])}
eval e@(EApp _ (Builtin _ TAt{}) Var{}) t = do {aa <- tat e; pure [t=:unIA aa]}
eval (EApp _ (Builtin _ (TAt i)) (Tup _ es)) t = eval (es!!(i-1)) t
eval (EApp _ (Builtin _ (TAt i)) e) t = do {(ss, as) <- plΠ e; pure (ss++[t=:unIA (as!!(i-1))])}
eval (EApp _ (EApp _ (Builtin _ IOf) p) xs) t | (Arrow tD _) <- eAnn p, Just szX <- nSz tD = do
    pR <- nBT
    szR <- nI; i <- nI; done <- nI
    (plX, (lX, xsR)) <- plA xs
    (x, wX) <- arg tD (ve xsR lX szX)
    ss <- writeRF p [x] (PT pR)
    let loop=While () done INeq 1 (wX i:ss++[If () (Is pR) [t=:Tmp i, done=:1] [], i+=1, Cmov () (IRel IGeq (Tmp i) (Tmp szR)) done 1])
    pure $ plX $ szR=:ev (eAnn xs) (xsR,lX):t=:(-1):done=:0:i=:0:[loop]
eval (EApp _ (EApp _ (Builtin _ IOf) p) x) t
    | Arrow tC _ <- eAnn p
    , Just (tEX, slopRnk) <- tRnk tC
    , Just szX <- nSz tEX, Just xRnk <- staRnk xSh = do
    pR <- nBT
    oR <- nI; i <- nI; done <- nI
    (plX, (lX, xR)) <- plA x
    (slop, nR, plSlopP, popSlop) <- plSlop szX slopRnk (idims slopRnk xRnk xR lX)
    (_, ss) <- writeF p [TA slop Nothing] (PT pR)
    let loop=While () done INeq 1 (cpy (AElem slop (KI slopRnk) Nothing 0) (AElem xR (KI xRnk) lX (Tmp i*Tmp nR)) (Tmp nR) szX:ss++[If () (Is pR) [t=:Tmp i, done=:1] [], i+=1, Cmov () (IRel IGeq (Tmp i) (Tmp oR)) done 1])
    pure $ plX $ oR=:ev tX (xR,lX):t=:(-1):done=:0:i=:0:plSlopP ++ [loop, popSlop]
  where tX@(Arr xSh _)=eAnn x
eval (Id _ (Iter f x n)) t = do
    (plN,nR) <- plC n
    plX <- eval x t
    ss <- writeRF f [IT t] (IT t)
    i <- nI
    let loop=For () E.Z 1 i 1 ILt nR ss
    pure $ plX++plN [loop]
eval (Cond _ p e0 e1) t = cond p e0 e1 (IT t)
eval (Id _ (FoldOfZip zop op (p:qs))) acc
    | tPs@(Arr sh _) <- eAnn p
    , Just (tP, pSz) <- aBs tPs
    , Just (tQs, qSzs) <- unzip<$>traverse (aBs.eAnn) qs = do
    x <- rtemp tP; ys <- traverse rtemp tQs; szR <- nI
    (plPP, (lP, pR)) <- plA p; (plQs, aQs) <- plAs qs
    ss <- writeRF op (IT acc:x:ys) (IT acc)
    let mQs at = [mt (AElem qR 1 lQ at qSz) y | (y, (lQ, qR), qSz) <- zip3 ys aQs qSzs]
    loop <- afor1 sh 1 ILt (Tmp szR) (\i -> mt (AElem pR 1 lP (Tmp i) pSz) x:mQs (Tmp i)++ss)
    seed <- writeRF zop (x:ys) (IT acc)
    pure $ plPP$plQs$szR =: ev tPs (pR,lP):mt (AElem pR 1 lP 0 pSz) x:mQs 0++seed++[loop]
eval (Id _ (U2 seeds gs c f n)) t | Just e <- traverse (rr.eAnn) seeds = do
    plU <- eval c t
    (plN,nE) <- plC n
    k <- nI
    xs <- traverse (rtemp.fst) e
    plSeeds <- concat <$> zipWithM eeval seeds xs
    usss <- concat <$> zipWithM (\g x -> writeRF g [x] x) gs xs
    fss <- writeRF f (IT t:xs) (IT t)
    pure $ plU ++ plN (plSeeds ++ [For () E.Z 1 k 0 ILt nE (fss++usss)])
eval (Id _ (FoldGen seed g f n)) t = do
    x <- nI; acc <- nI
    k <- nI
    (plSeed,seedR) <- plEV seed
    (plN,nE) <- plC n
    uss <- writeRF g [IT x] (IT x)
    fss <- writeRF f [IT acc, IT x] (IT acc)
    pure $ plSeed $ plN ([acc=:Tmp seedR, x=:Tmp seedR] ++ uss ++ [Rof () E.Z k (nE-1) (fss++uss), t=:Tmp acc])
eval e _          = nyi e

frel :: Builtin -> Maybe FRel
frel Gte=Just FGeq; frel Lte=Just FLeq; frel Eq=Just FEq; frel Neq=Just FNeq; frel Lt=Just FLt; frel Gt=Just FGt; frel _=Nothing

mFop :: Builtin -> Maybe FBin
mFop Plus=Just FPlus; mFop Times=Just FTimes; mFop Minus=Just FMinus; mFop Div=Just FDiv; mFop Exp=Just FExp
mFop Max=Just FMax; mFop Min=Just FMin; mFop CS=Just CpySgn;mFop _=Nothing

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
cond (EApp _ (EApp _ (Builtin (Arrow F _) o) c0) c1) e0 e1 t | Just f <- frel o, nind (eAnn e0) = do
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
cond (EApp _ (EApp _ (Builtin (Arrow I _) op) c0) c1) e0 e1 t | Just cmp <- rel op, nind (eAnn e0) = do
    c0R <- nI; c1R <- nI
    plC0 <- eval c0 c0R; plC1 <- eval c1 c1R
    plE0 <- eeval e0 t; plE1 <- eeval e1 t
    pure (plC0 ++ plC1 ++ [If () (IRel cmp (Tmp c0R) (Tmp c1R)) plE0 plE1])
cond p e0 e1 t | nind (eAnn e0) = do
    pR <- nBT
    plPP <- peval p pR; plE0 <- eeval e0 t; plE1 <- eeval e1 t
    pure (plPP ++ [If () (Is pR) plE0 plE1])

f2eval :: E (T ()) -> F2Temp -> CM [CS ()]
f2eval (LLet _ (n,e') e) t = do {eR <- bD2 n; (++) <$> f2eval e' eR <*> f2eval e t}
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
f2eval e _ = nyi e

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
feval (EApp _ (Builtin _ Tan) e) t = do
    (plE,r) <- plF e; t₀ <- nF; t₁ <- nF
    pure $ plE [MX () t₀ (FUn FSin (FTmp r)), MX () t₁ (FUn FCos (FTmp r)), MX () t (FTmp t₀/FTmp t₁)]
feval (EApp _ (Builtin _ ItoF) e) t = do
    (pl,iE) <- plC e
    pure $ pl [MX () t (IE iE)]
feval (Cond _ p e0 e1) t = cond p e0 e1 (FT t)
feval (EApp _ (Builtin _ T) x) t = feval x t; feval (EApp _ (Builtin _ Flat) x) t = feval x t
feval (Id _ (Aɴ xs ns)) t | Arr sh _ <- eAnn xs, Just rnk <- staRnk sh = do
    (plX, (lX, xR)) <- plA xs
    (plNs, nEs) <- first thread.unzip <$> traverse plC ns
    xRd <- nI
    (plB, b) <- off xR lX nEs
    pure $ plX $ plNs (plB++[xRd=:DP xR (KI rnk), MX () t (FAt (Raw xRd b lX 8))])
feval (Id _ (FoldOfZip zop op [EApp _ (EApp _ (EApp _ (Builtin _ Gen) seed) g) n, ys])) acc
    | (Arr ySh tY) <- eAnn ys, Just (tQ, qSz) <- rr tY, nind (eAnn seed) = do
    (plN,nE) <- plC n; (plU,x) <- plR seed
    (plYs, (lY, yR)) <- plA ys
    (plY,y) <- plR (Id tQ (Aɴ ys [ILit I 0]))
    yRd <- nI
    plSeed <- writeRF zop [x, y] (FT acc)
    ss <- writeRF op [FT acc, x, y] (FT acc)
    gs <- writeRF g [x] x
    ll <- arof1 ySh nE $ yRd+=KI qSz:mt (Raw yRd 0 lY qSz) y:gs++ss
    pure $ plYs $ plY $ plU plSeed ++ plN [yRd=:DP yR 1, ll]
feval (Id _ (FoldOfZip zop op [p])) acc | tyP@(Arr pSh F) <- eAnn p, Just c <- fca op, Just vseed <- fc c, hasS op = do
    acc₂ <- nF2; acc₁ <- nF; x <- nF2; x₀ <- nF
    i <- nI; szR <- nI; pD <- nI
    (plPP, (lP, pR)) <- plA p
    ss₁ <- writeRF op (FT<$>[acc,x₀]) (FT acc)
    ss <- write2 op [acc₂,x] acc₂
    seed <- writeRF zop [FT x₀] (FT acc)
    let step = MX2 () x (FAt (Raw pD 0 lP 8)):pD+=16:ss
        step₁ = MX () x₀ (FAt (Raw pD 0 lP 8)):pD+=8:ss₁
        loop = R2of () (pr1 pSh) i (Tmp szR) step step₁
    pure
        $plPP
        $szR=:ev tyP (pR,lP)
        :pD=:DP pR 1:MX () x₀ (FAt (Raw pD 0 lP 8)):pD+=8
        :seed
        ++[szR=:(Tmp szR-1), vseed acc acc₂, loop, Comb () c acc₁ acc₂, MX () acc (FTmp acc+FTmp acc₁)]
  where
    fca (Lam _ _ (Lam _ _ (EApp _ (EApp _ (Builtin _ b) _) _))) | fS b = mFop b; fca _ = Nothing
feval (Id _ (FoldOfZip zop op [p, q])) acc | tyP@(Arr pSh _) <- eAnn p, Arr _ F <- eAnn q, Just (c0,_) <- fz op, Just vseed <- fc c0, hasS op = do
    acc0 <- nF; acc2 <- nF2; x <- nF2; y <- nF2; x0 <- nF; y0 <- nF
    i <- nI; szR <- nI
    (plPP, (lP, pR)) <- plA p; (plQ, (lQ, qR)) <- plA q
    pD <- nI; qD <- nI
    ss1 <- writeRF op (FT<$>[acc,x0,y0]) (FT acc)
    ss <- write2 op [acc2, x, y] acc2
    seed <- writeRF zop (FT<$>[x0,y0]) (FT acc)
    let step1 = MX () x0 (FAt (Raw pD 0 lP 8)):pD+=8:MX () y0 (FAt (Raw qD 0 lQ 8)):qD+=8:ss1
        step = MX2 () x (FAt (Raw pD 0 lP 8)):pD+=16:MX2 () y (FAt (Raw qD 0 lQ 8)):qD+=16:ss
        loop = R2of () (pr1 pSh) i (Tmp szR) step step1
    pure
        $plPP$plQ
        $szR=:ev tyP (pR,lP)
        :pD=:DP pR 1:MX () x0 (FAt (Raw pD 0 lP 8)):pD+=8
        :qD=:DP qR 1:MX () y0 (FAt (Raw qD 0 lQ 8)):qD+=8
        :seed
        ++[szR=:(Tmp szR-1), vseed acc acc2, loop, Comb () c0 acc0 acc2, MX () acc (FTmp acc+FTmp acc0)]
  where
    fz (Lam _ _ (Lam _ _ (Lam _ _ (EApp _ (EApp _ (Builtin _ b0) _) (EApp _ (EApp _ (Builtin _ b1) _) _))))) | fS b0, fS b1 = (,) <$> mFop b0 <*> mFop b1
    fz _ = Nothing
feval (Id _ (FoldOfZip zop op (p:qs))) acc
    | tPs@(Arr pSh _) <- eAnn p
    , Just (tP, pSz) <- aBs tPs
    , Just (tQs, qSzs) <- unzip<$>traverse (aBs.eAnn) qs = do
    x <- rtemp tP; ys <- traverse rtemp tQs; nR <- nI
    (plPP, (lP, pR)) <- plA p; (plQs, aQs) <- plAs qs
    ss <- writeRF op (FT acc:x:ys) (FT acc)
    let mQs at = [mt (AElem qR 1 lQ at qSz) y | (y, (lQ, qR), qSz) <- zip3 ys aQs qSzs]
    loop <- afor1 pSh 1 ILt (Tmp nR) (\i -> mt (AElem pR 1 lP (Tmp i) pSz) x:mQs (Tmp i)++ss)
    seed <- writeRF zop (x:ys) (FT acc)
    pure $plPP$plQs$nR =: ev tPs (pR,lP):mt (AElem pR 1 lP 0 pSz) x:mQs 0++seed++[loop]
feval (Id _ (FoldSOfZip seed op (p:qs))) acc
    | tPs@(Arr pSh _) <- eAnn p
    , Just (tP, pSz) <- aBs tPs
    , Just (tQs, qSzs) <- unzip<$>traverse (aBs.eAnn) qs = do
    x <- rtemp tP; ys <- traverse rtemp tQs; nR <- nI
    plSeed <- feval seed acc
    (plPP, (lP, pR)) <- plA p; (plQs, aQs) <- plAs qs
    ss <- writeRF op (FT acc:x:ys) (FT acc)
    loop <- afor pSh 0 ILt (Tmp nR) (\i -> mt (AElem pR 1 lP (Tmp i) pSz) x:[mt (AElem qR 1 lQ (Tmp i) qSz) y | (y, (lQ, qR), qSz) <- zip3 ys aQs qSzs]++ss)
    pure $ plPP$plQs$nR=:ev tPs (pR,lP):plSeed++[loop]
feval (EApp _ (EApp _ (Builtin _ Fold) op) e) acc | tXs@(Arr xSh _) <- eAnn e, Just c <- fca op, Just vseed <- fc c = do
    x0 <- nF; acc0 <- nF; acc2 <- nF2; x <- nF2
    i <- nI; szR <- nI; xRd <- nI
    (plX, (lX, xR)) <- plA e
    ss1 <- writeRF op [FT acc, FT x0] (FT acc)
    ss <- write2 op [acc2, x] acc2
    let loop = F2or () (pr1 xSh) i 1 ILt (Tmp szR) (MX2 () x (FAt (Raw xRd 0 lX 16)):xRd+=16:ss) (MX () x0 (FAt (Raw xRd 0 lX 8)):xRd+=8:ss1)
    pure $ plX$szR=:ev tXs (xR,lX):xRd=:DP xR 1:MX () acc (FAt (Raw xRd 0 lX 8)):xRd+=8:vseed acc acc2:[loop, Comb () c acc0 acc2, MX () acc (FBin c (FTmp acc) (FTmp acc0))]
    -- TODO: read two elements to initialize?
  where
    fca (Lam _ _ (Lam _ _ (EApp _ (EApp _ (Builtin _ b) _) _))) | fS b = mFop b; fca _ = Nothing
feval (EApp _ (EApp _ (EApp _ (Builtin _ FoldS) op) seed) e) acc | tXs@(Arr xSh _) <- eAnn e, Just c <- fca op, Just vseed <- fc c = do
    x₀ <- nF; acc₀ <- nF; acc2 <- nF2; x <- nF2
    i <- nI; szR <- nI
    (plX, (lX, xR)) <- plA e
    (plSeed, seedR) <- plF seed
    ss0 <- writeRF op [FT acc, FT x₀] (FT acc)
    ss <- write2 op [acc2, x] acc2
    let loop = F2or () (pr xSh) i 0 ILt (Tmp szR) (MX2 () x (FAt (AElem xR 1 lX (Tmp i) 8)):ss) (MX () x₀ (FAt (AElem xR 1 lX (Tmp i) 8)):ss0)
    pure $ plX$szR=:ev tXs (xR,lX):plSeed (vseed seedR acc2:[loop, Comb () c acc₀ acc2, MX () acc (FBin c (FTmp seedR) (FTmp acc₀))])
  where
    fca (Lam _ _ (Lam _ _ (EApp _ (EApp _ (Builtin _ b) _) _))) | fS b = mFop b; fca _ = Nothing
feval (EApp _ (EApp _ (Builtin _ Fold) op) e) acc | tXs@(Arr xSh _) <- eAnn e = do
    x <- nF; szR <- nI
    (plE, (l, aP)) <- plA e
    ss <- writeRF op [FT acc, FT x] (FT acc)
    loop <- afor1 xSh 1 ILt (Tmp szR) (\i -> MX () x (FAt (AElem aP 1 l (Tmp i) 8)):ss)
    pure $ plE$szR =: ev tXs (aP,l):MX () acc (FAt (AElem aP 1 l 0 8)):[loop]
feval (EApp _ (EApp _ (EApp _ (Builtin _ Foldl) op) seed) e) acc | (Arrow _ (Arrow tX _)) <- eAnn op, nind tX = do
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
    (x, wX) <- arg tX (ve eR l xSz)
    ss <- writeRF op [FT acc, x] (FT acc)
    loop <- afort tArr 0 ILt (Tmp szR) (\i -> wX i:ss)
    pure $ plE $ plAcc++szR =: ev tArr (eR,l):[loop]
  where
    tArr=eAnn e
feval (Id _ (Iter f x n)) t = do
    (plN,nR) <- plC n
    plX <- feval x t
    ss <- writeRF f [FT t] (FT t)
    i <- nI
    let loop=For () E.Z 1 i 1 ILt nR ss
    pure $ plX ++ plN [loop]
feval e@(EApp _ (Builtin _ TAt{}) Var{}) t = do
    aa <- tat e
    pure [MX () t (unFA aa)]
feval (EApp _ (Builtin _ (TAt i)) (Tup _ es)) t = feval (es!!(i-1)) t
feval (EApp _ (Builtin _ (TAt i)) e) t = do
    (ss, as) <- plΠ e
    pure (ss++[MX () t (unFA (as!!(i-1)))])
feval (EApp _ (Var _ f) x) t | nind (eAnn x) = do
    st <- gets fvars
    let (l, [a], FT r) = getT st f
    plX <- eeval x (tr a)
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
    pure (plU ++ plSeeds ++ plN [Rof () E.Z k nE (fss++usss)])
feval (Id _ (FoldGen seed g f n)) t = do
    x <- nF; acc <- nF
    k <- nI
    (plSeed,seedR) <- plF seed
    (plN,nE) <- plC n
    uss <- writeRF g [FT x] (FT x)
    fss <- writeRF f [FT acc, FT x] (FT acc)
    pure $ plSeed $ plN ([MX () acc (FTmp seedR), MX () x (FTmp seedR)] ++ uss ++ [Rof () E.Z k (nE-1) (fss++uss), MX () t (FTmp acc)])
feval e _ = nyi e

sac t = Sa8 () t.KI
popc = Pop8().KI

sa sz | sz `rem` 8 == 0 = Sa8 () | otherwise = Sa ()
pop sz | sz `rem` 8 == 0 = Pop8 () | otherwise = Pop ()

tat (EApp _ (Builtin _ (TAt i)) (Var _ n)) = do
    st <- gets πvars
    pure(getT st n!!(i-1))

-- update IPA/how we pass args! (multiple returns...)
πr :: E (T ()) -> TStore -> CM [CS ()]
πr (EApp _ (EApp _ (Builtin _ Part) p) xs) [TA t0 l0@(Just a0), TA t1 l1@(Just a1)] | Arrow tX _ <- eAnn p, tXs@(Arr sh _) <- eAnn xs, Just sz <- nSz tX = do
    szR <- nI; n₀ <- nI; n₁ <- nI; b <- nBT
    (plX, (lX, xsR)) <- plA xs
    (xR, rX) <- arg tX (\k -> AElem xsR 1 lX (Tmp k) sz)
    ss <- writeRF p [xR] (PT b)
    loop <- afor sh 0 ILt (Tmp szR) $ \k -> rX k:ss++[If () (Is b) [wt (AElem t0 1 l0 (Tmp n₀) sz) xR, n₀+=1] [wt (AElem t1 1 l1 (Tmp n₁) sz) xR, n₁+=1]]
    pure (plX$szR=:ev tXs (xsR,lX)
        :Ma () sh a0 t0 1 (Tmp szR) sz
        :Ma () sh a1 t1 1 (Tmp szR) sz
        :[n₀=:0,n₁=:0, loop, Wr () (ADim t0 0 l0) (Tmp n₀), Wr () (ADim t1 0 l1) (Tmp n₁)])
πr (Tup _ es) ts = do
    concat <$> zipWithM (\e a ->
            case (eAnn e, a) of
                (I, TI t)              -> do {(plX,i) <- plC e; pure (plX [t=:i])}
                (Arr{}, TA t (Just l)) -> aeval e t l
                (B, TB t)              -> do {(plX,v) <- plP e; pure (plX [MB () t v])}
                (F, TF x)              -> do {(plX,v) <- plD e; pure (plX [MX () x v])}
                (P{}, TΠ td)           -> do {(plX,tt) <- plΠ e; pure (plX++mvts td tt)}) es ts
πr (Id _ (Iter f x n)) ts = do
    (plN,nR) <- plC n
    ats <- frts ts
    plS <- πr x ats
    -- TODO: array labels would be lost here, is that a problem?
    (_, ss) <- writeF f [TΠ ats] (ΠT (tr<$>ts))
    i <- nI
    let loop=For () E.Z 1 i 1 ILt nR (ss++mvts ats ts)
    pure $ plN (plS ++ [loop])
πr (LLet _ b e) ts = do
    ss <- llet b
    (ss++) <$> πr e ts
πr (EApp _ (Builtin _ T) e) ts = πr e ts; πr (EApp _ (Builtin _ Flat) e) ts = πr e ts
πr (Id ty (Aɴ xs ns)) ts | Arr sh _ <- eAnn xs, Just rnk <- staRnk sh, Just sz <- nSz ty = do
    (plX, (lX, xR)) <- plA xs
    (plNs, nEs) <- first thread.unzip <$> traverse plC ns
    xRd <- nI
    (plB, b) <- off xR lX nEs
    pure $ plX $ plNs (plB++[xRd=:DP xR (KI rnk), ATT () ts (Raw xRd b lX sz)])
πr (EApp _ (EApp _ (Builtin _ Fold) op) e) acc | tXs@(Arr xSh tX) <- eAnn e, Just xSz <- nSz tX = do
    x <- frts acc; szR <- nI; acc0 <- frts acc
    (plE, (l, aP)) <- plA e
    let xa=ΠT (tr<$>x); rts=ΠT (tr<$>acc); rts0=ΠT (tr<$>acc0)
    ss <- writeRF op [rts, xa] rts0
    loop <- afor1 xSh 1 ILt (Tmp szR) (\i -> ATT () x (AElem aP 1 l (Tmp i) xSz):ss++mvts acc acc0)
    pure $ plE$szR=:ev tXs (aP,l):ATT () acc (AElem aP 1 l 0 xSz):[loop]
πr (Id _ (FoldOfZip zop op (p:qs))) acc
    | tPs@(Arr pSh _) <- eAnn p
    , Just (tP, pSz) <- aBs tPs
    , Just (tQs, qSzs) <- unzip<$>traverse (aBs.eAnn) qs = do
    x <- rtemp tP; ys <- traverse rtemp tQs; nR <- nI; acc0 <- frts acc
    let rts=ΠT (tr<$>acc); rts0=ΠT (tr<$>acc0)
    (plPP, (lP, pR)) <- plA p; (plQs, aQs) <- plAs qs
    ss <- writeRF op (rts:x:ys) rts0
    let mQs at = [mt (AElem qR 1 lQ at qSz) y | (y, (lQ, qR), qSz) <- zip3 ys aQs qSzs]
    loop <- afor1 pSh 1 ILt (Tmp nR) (\i -> mt (AElem pR 1 lP (Tmp i) pSz) x:mQs (Tmp i)++ss++mvts acc acc0)
    seed <- writeRF zop (x:ys) rts
    pure $plPP$plQs$nR =: ev tPs (pR,lP):mt (AElem pR 1 lP 0 pSz) x:mQs 0++seed++[loop]
πr (Cond _ p e0 e1) t = cond p e0 e1 (ΠT (tr<$>t))
πr e@(EApp _ (Builtin _ TAt{}) Var{}) t = do
    aa <- tat e
    pure (mvts t (gpt aa))
πr (EApp _ (Builtin _ (TAt i)) (Tup _ es)) t = πr (es!!(i-1)) t
πr (EApp _ (Builtin _ (TAt i)) e) t = do
    (ss, as) <- plΠ e
    pure (ss++mvts t (gpt (as!!(i-1))))
πr (Var _ x) t = do
    st <- gets πvars
    pure (mvts t (getT st x))
πr e _ = nyi e

gpt (TΠ rs)=rs

πe :: E (T ()) -> Temp -> CM ([Int64], Maybe Int64, [AL], [CS ()])
πe e t | P tys <- eAnn e, offs <- szT tys, sz <- last offs = do
    (pl,as) <- plΠ e
    pure (offs, Just sz, catt as, pl++[WrT () (TupM t Nothing) as])
  where
    catt = mapMaybe g where g (TA _ l)=l; g _=Nothing

unsupported = error"Requires statically known rank."
usi = error"Requires statically known dimensions."

qmap f g h k ~(x,y,z,w) = (f x, g y, h z, k w)

nyi e = error ("Not yet implemented: " ++ show e)
