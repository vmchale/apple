{-# LANGUAGE TupleSections #-}

module IR.Trans ( writeC
                ) where

import           A
import           Control.Monad              (zipWithM, (<=<))
import           Control.Monad.State.Strict (State, gets, modify, runState)
import           Data.Foldable              (fold)
import           Data.Functor               (($>))
import           Data.Int                   (Int64)
import qualified Data.IntMap                as IM
import           Data.List                  (scanl')
import           IR
import           Name
import           U

data IRSt = IRSt { labels :: [Label]
                 , temps  :: [Int]
                 , arrs   :: [Int]
                 , vars   :: IM.IntMap Temp -- track vars so that (Var x) can be replaced at the site
                 , avars  :: IM.IntMap (Maybe Int, Temp)
                 , fvars  :: IM.IntMap (Label, [(Maybe Int, Temp)], (Maybe Int, Temp))
                 , mts    :: IM.IntMap Temp
                 }

getT :: IM.IntMap b -> Name a -> b
getT st n@(Name _ (U i) _) = IM.findWithDefault (error ("Internal error: variable " ++ show n ++ " not mapped to register.")) i st

nextI :: IRM Int
nextI = do
    i <- gets (head.temps)
    modify (\(IRSt l (_:t) ar v a f ts) -> IRSt l t ar v a f ts) $> i

nextArr :: IRM Int
nextArr = do
    a <- gets (head.arrs)
    modify (\(IRSt l t (_:ar) v aϵ f ts) -> IRSt l t ar v aϵ f ts) $> a

newITemp :: IRM Temp
newITemp = ITemp <$> nextI

newFTemp :: IRM Temp
newFTemp = FTemp <$> nextI

newLabel :: IRM Label
newLabel = do
    i <- gets (head.labels)
    modify (\(IRSt l t ar v a f ts) -> IRSt (tail l) t ar v a f ts) $> i

addMT :: Int -> Temp -> IRSt -> IRSt
addMT i tϵ (IRSt l t ar v a f ts) = IRSt l t ar v a f (IM.insert i tϵ ts)

addVar :: Name a -> Temp -> IRSt -> IRSt
addVar (Name _ (U i) _) r (IRSt l t ar v a f ts) = IRSt l t ar (IM.insert i r v) a f ts

addAVar :: Name a -> (Maybe Int, Temp) -> IRSt -> IRSt
addAVar (Name _ (U i) _) r (IRSt l t ar v a f ts) = IRSt l t ar v (IM.insert i r a) f ts

addFVar :: Name a -> (Label, [(Maybe Int, Temp)], (Maybe Int, Temp)) -> IRSt -> IRSt
addFVar (Name _ (U i) _) x (IRSt l t ar v a f ts) = IRSt l t ar v a (IM.insert i x f) ts

type IRM = State IRSt

mAF :: T a -> Maybe (T a)
mAF (Arrow (Arr _ t) F) = Just t
mAF _                   = Nothing

isFFF :: T a -> Bool
isFFF (Arrow F (Arrow F F)) = True
isFFF _                     = False

isF :: T a -> Bool
isF F = True
isF _ = False

isI :: T a -> Bool
isI I = True
isI _ = False

isT :: T a -> Bool
isT P{} = True; isT _ = False

isB :: T a -> Bool; isB B = True; isB _ = False

isArr Arr{} = True
isArr _     = False

writeC :: E (T ()) -> ([Stmt], WSt, IM.IntMap Temp)
writeC = π.flip runState (IRSt [0..] [0..] [0..] IM.empty IM.empty IM.empty IM.empty) . writeCM where π (s, IRSt l t _ _ _ _ a) = (s, WSt l t, a)

-- %xmm0 – %xmm7
writeCM :: E (T ()) -> IRM [Stmt]
writeCM e' = go e' [F0,F1,F2,F3,F4,F5] [C0,C1,C2,C3,C4,C5] where
    go (Lam _ x@(Name _ _ F) e) (fr:frs) rs = do
        modify (addVar x fr)
        go e frs rs
    go (Lam _ (Name _ _ F) _) [] _ = error "Not enough floating-point registers!"
    go (Lam _ x@(Name _ _ I) e) frs (r:rs) = do
        modify (addVar x r)
        go e frs rs
    go (Lam _ x@(Name _ _ P{}) e) frs (r:rs) = do
        modify (addVar x r)
        go e frs rs
    go (Lam _ x@(Name _ _ Arr{}) e) frs (r:rs) = do
        modify (addAVar x (Nothing, r))
        go e frs rs
    go Lam{} _ [] = error "Not enough registers!"
    go e _ _ | isF (eAnn e) = do {f <- newFTemp ; (++[MX FRet (FReg f)]) <$> eval e f} -- avoid clash with xmm0 (arg + ret)
             | isI (eAnn e) = eval e CRet
             | isB (eAnn e) = eval e CRet
             | isArr (eAnn e) = do{(l,r) <- aeval e CRet; pure$case l of {Just m -> r++[RA m]}}
             | P [F,F] <- eAnn e = do {t<- newITemp; p <- eval e t; pure$p++[MX FRet (FAt (AP t Nothing Nothing)), MX FRet1 (FAt (AP t (Just$ConstI 8) Nothing)), Pop 16]}
             | ty@P{} <- eAnn e = let b = bT ty in do{t <- newITemp; a <- nextArr; pl <- eval e t; pure $ pl ++ [Ma a CRet (ConstI b), Cpy (AP CRet Nothing (Just a)) (AP t Nothing Nothing) (ConstI $ b `div` 8), RA a]}
             | otherwise = error ("Unsupported return type: " ++ show (eAnn e))

writeRF :: E (T ()) -> [Temp] -> Temp -> IRM [Stmt]
writeRF e rs = fmap snd . writeF e ((Nothing,) <$> rs)

dim1 a t n = [Wr (AP t Nothing a) (ConstI 1), Wr (AP t (Just (ConstI 8)) a) n]
tick reg = MT reg (IB IPlus (Reg reg) (ConstI 1))
sib ireg = IB IPlus (IB IAsl (Reg ireg) (ConstI 3)) (ConstI 16)

stadim a t ns = Wr (AP t Nothing a) (ConstI (fromIntegral$length ns)):zipWith (\o n -> Wr (AP t (Just (ConstI$8*o)) a) n) [1..] ns

-- incr.
doN t e ss = do
    l <- newLabel; endL <- newLabel
    pure $ MT t (ConstI 0):L l:MJ (IRel IGeq (Reg t) e) endL:ss++[tick t, J l, L endL]

staRnk :: Integral b => Sh a -> Maybe b
staRnk Nil           = Just 0
staRnk (_ `Cons` sh) = (1+) <$> staRnk sh
staRnk _             = Nothing

tRnk :: Integral b => T a -> Maybe (T a, b)
tRnk (Arr sh t) = (t,) <$> staRnk sh
tRnk _          = Nothing

dimR rnk (t,l) = (\o -> EAt (AP t (Just$ConstI (8*o)) l)) <$> [1..rnk]
offByDim :: [Exp] -> IRM ([Temp], [Stmt])
offByDim dims = do
    ts <- traverse (\_ -> newITemp) (undefined:dims)
    let ss=zipWith3 (\t1 t0 d -> MT t1 (IB ITimes (Reg t0) d)) (tail ts) ts dims
    pure (reverse ts, MT (head ts) (ConstI 1):ss)
    -- drop 1 for strides

xp strides ixs (t,l) =
    let offs=foldl1 (IB IPlus) $ zipWith (\d i -> IB ITimes i d) strides ixs
    in AP t (Just (IB IAsl offs (ConstI 3))) l

stacopy sdims ddims x ls ad as = do
    t <- newITemp
    let eachIx = crossN $ fmap (\l -> [0..(l-1)]) ls
    pure $ concat [ [ MT t (EAt (xp sdims (at is) as)), Wr (xp ddims (at is) ad) (Reg t) ] | is <- eachIx ]
    where at = zipWith (\x_a i -> IB IPlus x_a (ConstI i)) x

crossN :: [[a]] -> [[a]]
crossN = foldr (\xs yss -> [ x:ys | x <- xs, ys <- yss ]) [[]]

-- write loop body (updates global state, dependent on ast being globally renamed)
writeF :: E (T ())
       -> [(Maybe Int, Temp)] -- ^ Registers for arguments
       -> Temp -- ^ Register for return value
       -> IRM (Maybe Int, [Stmt])
writeF (Lam _ x e) (r:rs) ret | isArr (loc x) = do
    modify (addAVar x r)
    writeF e rs ret
writeF (Lam _ x e) ((_,r):rs) ret = do
    modify (addVar x r)
    writeF e rs ret
writeF Lam{} [] _ = error "Internal error: wrong number of registers to arguments."
writeF e _ ret | isArr (eAnn e) = aeval e ret
               | otherwise = (Nothing,) <$> eval e ret

fop op e0 = EApp F (EApp (Arrow F F) (Builtin (Arrow F (Arrow F F)) op) e0)
eMinus = fop Minus
ePlus = fop Plus
eDiv = fop Div

aeval :: E (T ()) -> Temp -> IRM (Maybe Int, [Stmt])
aeval (Var Arr{} x) t = do
    st <- gets avars
    let (i, r) = getT st x
    pure (i, [MT t (Reg r)])
aeval (EApp res (EApp _ (Builtin _ (Map 1)) op) e) t | f1 (eAnn e) && f1 res = do
    a <- nextArr
    arrP <- newITemp
    (l, plE) <- aeval e arrP
    -- cause f1 (skip rank)
    let sz = EAt (AP arrP (Just (ConstI 8)) l)
    f <- newFTemp
    ss <- writeRF op [f] f
    iR <- newITemp
    szR <- newITemp
    let loopBody = MX f (FAt (AP arrP Nothing l)):ss++[WrF (AP t (Just (sib iR)) (Just a)) (FReg f), MT arrP (IB IPlus (Reg arrP) (ConstI 8))]
    loop <- doN iR (Reg szR) loopBody
    modify (addMT a t)
    pure (Just a, plE ++ MT szR sz:Ma a t (IB IPlus (IB IAsl (Reg szR) (ConstI 3)) (ConstI 16)):dim1 (Just a) t (Reg szR) ++ MT arrP (IB IPlus (Reg arrP) (ConstI 16)):loop)
aeval (EApp res (EApp _ (EApp _ (Builtin _ Zip) op) xs) ys) t | f1(eAnn xs) && f1(eAnn ys) && f1 res = do
    a <- nextArr
    arrPX <- newITemp; arrPY <- newITemp
    (lX, plEX) <- aeval xs arrPX; (lY, plEY) <- aeval ys arrPY
    let sz = EAt (AP arrPX (Just (ConstI 8)) lX)
    x <- newFTemp; y <- newFTemp; z <- newFTemp
    ss <- writeRF op [x,y] z
    iR <- newITemp
    szR <- newITemp
    let loopBody = let ireg = Just (sib iR) in MX x (FAt (AP arrPX ireg lX)):MX y (FAt (AP arrPY ireg lY)):ss++[WrF (AP t ireg (Just a)) (FReg z)]
    loop <- doN iR (Reg szR) loopBody
    modify (addMT a t)
    pure (Just a, plEX ++ plEY ++ MT szR sz:Ma a t (IB IPlus (IB IAsl (Reg iR) (ConstI 3)) (ConstI 16)):dim1 (Just a) t (Reg szR) ++ loop)
aeval (EApp res (EApp _ (EApp _ (Builtin _ Scan) op) seed) e) t | f1 (eAnn e) && f1 res && isF (eAnn seed) = do
    a <- nextArr
    arrP <- newITemp
    acc <- newFTemp
    plSeed <- eval seed acc
    (l, plE) <- aeval e arrP
    let sz = EAt (AP arrP (Just (ConstI 8)) l)
    n <- newFTemp
    ss <- writeRF op [acc, n] acc
    iR <- newITemp; szR <- newITemp
    let loopBody = MX n (FAt (AP arrP (Just$sib iR) l)):WrF (AP t (Just (sib iR)) (Just a)) (FReg acc):ss
    loop <- doN iR (Reg szR) loopBody
    modify (addMT a t)
    pure (Just a, plE ++ plSeed ++ MT szR (IB IPlus sz (ConstI 1)):Ma a t (IB IPlus (IB IAsl (Reg szR) (ConstI 3)) (ConstI 16)):dim1 (Just a) t (Reg szR) ++ loop)
aeval (EApp res (EApp _ (EApp _ (Builtin _ Scan) op) seed) e) t | i1 (eAnn e) && i1 res && isI (eAnn seed) = do
    a <- nextArr
    arrP <- newITemp
    acc <- newITemp
    plSeed <- eval seed acc
    (l, plE) <- aeval e arrP
    -- rank1
    let sz = EAt (AP arrP (Just$ConstI 8) l)
    n <- newITemp
    ss <- writeRF op [acc, n] acc
    iR <- newITemp
    szR <- newITemp
    -- TODO: why arrP and iR?
    let loopBody=MT n (EAt (AP arrP (Just$IB IAsl (Reg iR) (ConstI 3)) l)):Wr (AP t (Just (sib iR)) (Just a)) (Reg acc):ss
    loop <- doN iR (Reg szR) loopBody
    modify (addMT a t)
    pure (Just a, plE ++ plSeed ++ MT szR (IB IPlus sz (ConstI 1)):Ma a t (IB IPlus (IB IAsl (Reg szR) (ConstI 3)) (ConstI 16)):dim1 (Just a) t (Reg szR) ++ MT arrP (IB IPlus (Reg arrP) (ConstI 16)):loop)
aeval (EApp res (EApp _ (Builtin _ (Map 1)) op) e) t | i1 (eAnn e) && i1 res = do
    a <- nextArr
    arrP <- newITemp
    (l, plE) <- aeval e arrP
    -- cause f1 (skip rank)
    let sz = EAt (AP arrP (Just (ConstI 8)) l)
    m <- newITemp
    ss <- writeRF op [m] m
    iR <- newITemp
    szR <- newITemp
    -- TODO: why arrP and iR?
    let loopBody = MT m (EAt (AP arrP (Just$IB IAsl (Reg iR) (ConstI 3)) l)):ss++[Wr (AP t (Just (sib iR)) (Just a)) (Reg m)]
    loop <- doN iR (Reg szR) loopBody
    modify (addMT a t)
    pure (Just a, plE ++ MT szR sz:Ma a t (IB IPlus (IB IAsl (Reg szR) (ConstI 3)) (ConstI 16)):Wr (AP t Nothing (Just a)) (ConstI 1):Wr (AP t (Just$ConstI 8) (Just a)) (Reg szR):MT arrP (IB IPlus (Reg arrP) (ConstI 16)):loop)
aeval (EApp _ (EApp _ (EApp _ (Builtin _ IRange) start) end) (ILit _ 1)) t = do
    a <- nextArr
    n <- newITemp
    startR <- newITemp
    endR <- newITemp
    i <- newITemp
    putStart <- eval start startR; putEnd <- eval end endR
    l <- newLabel; endL <- newLabel
    modify (addMT a t)
    let putN = MT n (IB IPlus (IB IMinus (Reg endR) (Reg startR)) (ConstI 1))
    let loop = [MJ (IRel IGt (Reg startR) (Reg endR)) endL, Wr (AP t (Just (Reg i)) (Just a)) (Reg startR), MT startR (IB IPlus (Reg startR) (ConstI 1)), MT i (IB IPlus (Reg i) (ConstI 8))]
    pure (Just a, putStart++putEnd++putN:Ma a t (IB IPlus (IB IAsl (Reg n) (ConstI 3)) (ConstI 24)):Wr (AP t Nothing (Just a)) (ConstI 1):Wr (AP t (Just (ConstI 8)) (Just a)) (Reg n):MT i (ConstI 16):L l:loop ++ [J l, L endL])
aeval (EApp _ (EApp _ (EApp _ (Builtin _ IRange) start) end) incr) t = do
    a <- nextArr
    n <- newITemp
    startR <- newITemp
    endR <- newITemp
    incrR <- newITemp
    i <- newITemp
    putStart <- eval start startR
    putEnd <- eval end endR
    putIncr <- eval incr incrR
    l <- newLabel; endL <- newLabel
    modify (addMT a t)
    let putN = MT n (IB IR.IDiv (IB IMinus (Reg endR) (Reg startR)) (Reg incrR))
    let loop = [MJ (IRel IGt (Reg startR) (Reg endR)) endL, Wr (AP t (Just (Reg i)) (Just a)) (Reg startR), MT startR (IB IPlus (Reg startR) (Reg incrR)), MT i (IB IPlus (Reg i) (ConstI 8))]
    pure (Just a, putStart++putEnd++putIncr++putN:Ma a t (IB IPlus (IB IAsl (Reg n) (ConstI 3)) (ConstI 24)):Wr (AP t Nothing (Just a)) (ConstI 1):Wr (AP t (Just (ConstI 8)) (Just a)) (Reg n):MT i (ConstI 16):L l:loop ++ [J l, L endL])
aeval (EApp _ (EApp _ (EApp _ (Builtin _ FRange) start) end) nSteps) t = do
    a <- nextArr
    i <- newITemp
    startR <- newFTemp
    incrR <- newFTemp
    n <- newITemp
    putStart <- eval start startR; putN <- eval nSteps n
    modify (addMT a t)
    putIncr <- eval ((end `eMinus` start) `eDiv` (EApp F (Builtin (Arrow I F) ItoF) nSteps `eMinus` FLit F 1)) incrR
    let loopBody = [WrF (AP t (Just (sib i)) (Just a)) (FReg startR), MX startR (FB FPlus (FReg startR) (FReg incrR))]
    loop <- doN i (Reg n) loopBody
    pure (Just a, putStart ++ putIncr ++ putN ++ Ma a t (IB IPlus (IB IAsl (Reg n) (ConstI 3)) (ConstI 16)):dim1 (Just a) t (Reg n) ++ loop)
aeval (EApp oTy (EApp _ (Builtin _ Succ) op) arr) t | f1 (eAnn arr) && f1 oTy = do
    a <- nextArr
    arrP <- newITemp
    szR <- newITemp
    fArg0R <- newFTemp; fArg1R <- newFTemp; fRetR <- newFTemp
    (arrL, putX) <- aeval arr arrP
    -- f1 (skip rank)
    let sz = EAt (AP arrP (Just (ConstI 8)) arrL)
    i <- newITemp
    ss <- writeRF op [fArg0R, fArg1R] fRetR
    let loopBody = MX fArg1R (FAt (AP arrP (Just (sib i)) arrL)):MX fArg0R (FAt (AP arrP (Just (sib i)) arrL)):ss++[WrF (AP t (Just (sib i)) (Just a)) (FReg fRetR)]
    loop <- doN i (Reg szR) loopBody
    pure (Just a, putX ++ MT szR sz:Ma a t (IB IPlus (IB IAsl (Reg szR) (ConstI 3)) (ConstI 16)):dim1 (Just a) t (IB IMinus (Reg szR) (ConstI 1)) ++ loop)
aeval (EApp oTy (EApp _ (Builtin _ Succ) op) arr) t | i1 (eAnn arr) && i1 oTy = do
    a <- nextArr
    arrP <- newITemp
    szR <- newITemp
    arg0R <- newITemp; arg1R <- newITemp; retR <- newITemp
    (arrL, putX) <- aeval arr arrP
    -- f1 (skip rank)
    let sz = EAt (AP arrP (Just (ConstI 8)) arrL)
    i <- newITemp
    ss <- writeRF op [arg0R, arg1R] retR
    let loopBody = MT arg1R (EAt (AP arrP (Just (sib i)) arrL)):MT arg0R (EAt (AP arrP (Just (sib i)) arrL)):ss++[Wr (AP t (Just (sib i)) (Just a)) (Reg retR)]
    loop <- doN i (Reg szR) loopBody
    pure (Just a, putX ++ MT szR sz:Ma a t (IB IPlus (IB IAsl (Reg szR) (ConstI 3)) (ConstI 16)):dim1 (Just a) t (IB IMinus (Reg szR) (ConstI 1)) ++ loop)
aeval (EApp oTy (EApp _ (Builtin _ (DI n)) op) arr) t | f1 (eAnn arr) && f1 oTy = do
    a <- nextArr
    arrP <- newITemp
    slopP <- newITemp
    szR <- newITemp
    fR <- newFTemp
    (arrL, putX) <- aeval arr arrP
    -- cause f1 (skip rank)
    let sz = EAt (AP arrP (Just (ConstI 8)) arrL)
        nIr = 16+n*8
    iR <- newITemp
    ss <- writeRF op [slopP] fR
    let loopBody = Cpy (AP slopP (Just (ConstI 16)) Nothing) (AP arrP (Just (sib iR)) arrL) (ConstI $ fromIntegral n + 2):ss++[WrF (AP t (Just (sib iR)) arrL) (FReg fR)]
    loop <- doN iR (Reg szR) loopBody
    pure (Just a, putX++MT szR sz:Ma a t (IB IPlus (IB IAsl (Reg szR) (ConstI 3)) (ConstI (24-8*fromIntegral n))):Wr (AP t Nothing (Just a)) (ConstI 1):Wr (AP t (Just (ConstI 8)) (Just a)) (IB IMinus (Reg szR) (ConstI $ fromIntegral n - 1)):Sa slopP nIr:Wr (AP slopP Nothing Nothing) (ConstI 1):Wr (AP slopP (Just (ConstI 8)) Nothing) (ConstI $ fromIntegral n):loop ++ [Pop nIr])
aeval (EApp oTy (EApp _ (EApp _ (Builtin _ Gen) seed) op) n) t | i1 oTy = do
    a <- nextArr
    arg <- newITemp
    i <- newITemp
    nR <- newITemp
    let sz = IB IPlus (Reg nR) (ConstI 16)
    modify (addMT a t)
    putSeed <- eval seed arg; putN <- eval n nR
    ss <- writeRF op [arg] arg
    let loopBody = Wr (AP t (Just (sib i)) (Just a)) (Reg arg):ss
    loop <- doN i (Reg nR) loopBody
    pure (Just a, putSeed ++ putN ++ Ma a t sz:dim1 (Just a) t (Reg nR) ++ loop)
aeval (EApp oTy (EApp _ (EApp _ (Builtin _ Gen) seed) op) (ILit _ n)) t | (Arr (_ `Cons` Nil) ty@P{}) <- oTy = do
    a <- nextArr
    arg <- newITemp
    i <- newITemp
    let ptN :: Integral a => a; ptN=bT ty;pt = ConstI ptN
        nE=ConstI$asI n;sz = IB IPlus (IB ITimes nE pt) (ConstI$16+ptN)
    modify (addMT a t)
    putSeed <- eval seed arg
    l <- newLabel; endL <- newLabel
    ss <- writeRF op [arg] arg
    let loop = ss ++ [Cpy (AP t (Just (IB IPlus (IB ITimes (Reg i) pt) (ConstI 16))) (Just a)) (AP arg Nothing Nothing) (ConstI$ptN`div`8)]
    pure (Just a, Ma a t sz:dim1 (Just a) t nE ++ Sa arg ptN:putSeed ++ MT i (ConstI 0):L l:MJ (IRel IGeq (Reg i) nE) endL:loop ++ [MT i (IB IPlus (Reg i) (ConstI 1)), J l, L endL, Pop ptN])
aeval (EApp oTy (EApp _ (EApp _ (Builtin _ Gen) seed) op) n) t | (Arr (_ `Cons` Nil) ty@P{}) <- oTy = do
    a <- nextArr
    arg <- newITemp
    i <- newITemp
    nR <- newITemp
    let ptN :: Integral a => a; ptN=bT ty;pt = ConstI ptN
        sz = IB IPlus (IB ITimes (Reg nR) pt) (ConstI$16+ptN)
    modify (addMT a t)
    putSeed <- eval seed arg; putN <- eval n nR
    ss <- writeRF op [arg] arg
    let loopBody = ss ++ [Cpy (AP t (Just (IB IPlus (IB ITimes (Reg i) pt) (ConstI 16))) (Just a)) (AP arg Nothing Nothing) (ConstI$ptN`div`8)]
    loop <- doN i (Reg nR) loopBody
    pure (Just a, putN ++ Ma a t sz:dim1 (Just a) t (Reg nR) ++ Sa arg ptN:putSeed ++ loop ++ [Pop ptN])
aeval (EApp oTy (EApp _ (Builtin _ Re) n) x) t | f1 oTy = do
    a <- nextArr
    xR <- newFTemp
    nR <- newITemp
    i <- newITemp
    let sz = IB IPlus (Reg nR) (ConstI 16)
    putN <- eval n nR; putX <- eval x xR
    let step = WrF (AP t (Just (sib i)) (Just a)) (FReg xR)
    loop <- doN i (Reg nR) [step]
    pure (Just a, putX ++ putN ++ Ma a t sz:dim1 (Just a) t (Reg nR) ++ loop)
aeval (Id _ (AShLit ns es)) t | isF (eAnn$head es) = do
    a <- nextArr
    xR <- newFTemp
    let ne=product ns; rnk=fromIntegral$length ns;sz=ConstI$8*fromIntegral ne+24
    modify (addMT a t)
    steps <- concat<$>zipWithM (\i e -> do{ss <- eval e xR; pure$ss++[WrF (AP t (Just (ConstI$8*rnk+8*i)) (Just a)) (FReg xR)]}) [1..fromIntegral ne] es
    pure (Just a, Ma a t sz:stadim (Just a) t (ConstI .fromIntegral<$>ns) ++ steps)
aeval (Id oTy (AShLit [n] es)) t | i1 oTy = do
    a <- nextArr
    xR <- newITemp
    let sz = ConstI$8*fromIntegral n+24
    modify (addMT a t)
    steps <- concat<$>zipWithM (\i e -> do{ss <- eval e xR; pure$ss++[Wr (AP t (Just (ConstI$16+8*i)) (Just a)) (Reg xR)]}) [0..(fromIntegral n-1)] es
    pure (Just a, Ma a t sz:dim1 (Just a) t (ConstI$fromIntegral n) ++ steps)
aeval (EApp res (EApp _ (Builtin _ ConsE) x) xs) t | i1 res = do
    a <- nextArr
    xR <- newITemp; xsR <- newITemp
    plX <- eval x xR
    (l, plXs) <- aeval xs xsR
    nR <- newITemp; nϵR <- newITemp
    let nϵ = EAt (AP xsR (Just (ConstI 8)) l); n = IB IPlus (Reg nϵR) (ConstI 1)
    pure (Just a, plX ++ plXs ++ MT nϵR nϵ:MT nR n:Ma a t (IB IPlus (IB IAsl (Reg nR) (ConstI 3)) (ConstI 16)):dim1 (Just a) t (Reg nR) ++ [Cpy (AP t (Just (ConstI 24)) (Just a)) (AP xsR (Just (ConstI 16)) l) (Reg nϵR), Wr (AP t (Just (ConstI 16)) (Just a)) (Reg xR)])
aeval (EApp res (EApp _ (Builtin _ ConsE) x) xs) t | f1 res = do
    a <- nextArr
    xR <- newFTemp; xsR <- newITemp
    plX <- eval x xR
    (l, plXs) <- aeval xs xsR
    nR <- newITemp; nϵR <- newITemp
    let nϵ = EAt (AP xsR (Just (ConstI 8)) l); n = IB IPlus (Reg nϵR) (ConstI 1)
    pure (Just a, plX ++ plXs ++ MT nϵR nϵ:MT nR n:Ma a t (IB IPlus (IB IAsl (Reg nR) (ConstI 3)) (ConstI 16)):dim1 (Just a) t (Reg nR) ++ [Cpy (AP t (Just (ConstI 24)) (Just a)) (AP xsR (Just (ConstI 16)) l) (Reg nϵR), WrF (AP t (Just (ConstI 16)) (Just a)) (FReg xR)])
aeval (EApp _ (Builtin _ T) x) t | Just (ty, rnk) <- tRnk (eAnn x) = do
    a <- nextArr
    xR <- newITemp; xRd <- newITemp; td <- newITemp
    let dE = ConstI$8+8*rnk
    (l, plX) <- aeval x xR
    let dimE = dimR rnk (xR, l); sze = bT ty
    (dts, dss) <- unzip <$> traverse (\e -> do {dt <- newITemp; pure (dt, MT dt e)}) dimE
    (sts, sss) <- offByDim (Reg <$> dts)
    (std, ssd) <- offByDim (Reg <$> reverse dts)
    let nOut:sstrides = sts; (_:dstrides) = std
    ixs <- traverse (\_ -> newITemp) [1..rnk]
    loop <- threadM (zipWith doN ixs (Reg <$> dts)) [Cpy (xp (Reg <$> dstrides) (Reg <$> reverse ixs) (td, Just a)) (xp (Reg <$> sstrides) (Reg <$> ixs) (xRd, l)) (ConstI$sze`div`8)]
    pure (Just a, plX ++ dss ++ sss ++ Ma a t (IB IPlus (IB IAsl (IB IPlus (Reg nOut) (ConstI rnk)) (ConstI 3)) (ConstI 8)):ssd ++ MT xRd (IB IPlus (Reg xR) dE):MT td (IB IPlus (Reg t) dE):loop)
aeval (EApp _ (EApp _ (Builtin _ (Conv is)) f) x) t | Just iTy <- mAF (eAnn f) = do
    a <- nextArr
    xR <- newITemp; xRd <- newITemp; slopP <- newITemp; ret <- newFTemp; td <- newITemp
    (l, plX) <- aeval x xR
    let rnk = length is; rnk64 = fromIntegral rnk; dimE = dimR rnk64 (xR, l); dE = ConstI$8+8*rnk64
        nIr = 8+8*rnk+bT iTy*product is
        i64s = fromIntegral <$> is; slopStrides = tail (scanr (*) 1 i64s)
        dimEs = zipWith (IB IMinus) (dimR rnk64 (xR, l)) (ConstI . fromIntegral <$> is)
    (dts, dss) <- unzip <$> traverse (\e -> do {dt <- newITemp; pure (dt, MT dt e)}) dimEs
    (sts, sss) <- offByDim (Reg <$> dts) -- strides
    let nOut:strides = sts
    ss <- writeRF f [slopP] ret
    ixs <- traverse (\_ -> newITemp) is
    preCopy <- stacopy (Reg <$> strides) (ConstI <$> slopStrides) (Reg <$> ixs) i64s (td, Just a) (xRd, l)
    loop <- threadM (zipWith doN ixs (Reg <$> dts)) (preCopy ++ ss ++ [WrF (xp (Reg <$> sts) (Reg <$> ixs) (td, Just a)) (FReg ret)])
    pure (Just a, plX ++ Sa slopP nIr : dss ++ sss ++ Ma a t (IB IPlus (IB IAsl (IB IPlus (Reg nOut) (ConstI rnk64)) (ConstI 3)) (ConstI 8)):Wr (AP t Nothing (Just a)) (ConstI 1):zipWith (\o t -> Wr (AP t (Just$ConstI (8*o)) (Just a)) (Reg t)) [1..] dts ++ MT xRd (IB IPlus (Reg xR) dE):MT td (IB IPlus (Reg t) dE):loop ++ [Pop nIr])
aeval (EApp _ (EApp _ (Builtin _ CatE) x) y) t | Just (ty, 1) <- tRnk (eAnn x) = do
    a <- nextArr
    xR <- newITemp; yR <- newITemp
    xnR <- newITemp; ynR <- newITemp; tn <- newITemp; mid <- newITemp
    let tyN=bT ty`div`8
    (lX, plX) <- aeval x xR; (lY, plY) <- aeval y yR
    pure (Just a, plX ++ plY ++ MT xnR (EAt (AP xR (Just$ConstI 8) lX)):MT ynR (EAt (AP yR (Just$ConstI 8) lY)):MT tn (IB IPlus (Reg xnR) (Reg ynR)):Ma a t (IB IPlus (IB IAsl (Reg tn) (ConstI 3)) (ConstI 16)):dim1 (Just a) t (Reg tn) ++ [Cpy (AP t (Just$ConstI 16) (Just a)) (AP xR (Just$ConstI 16) lX) (IB ITimes (Reg xnR) (ConstI tyN)), MT mid (IB IPlus (Reg t) (IB IAsl (IB IPlus (Reg ynR) (ConstI 2)) (ConstI 3))), Cpy (AP mid Nothing (Just a)) (AP yR (Just$ConstI 16) lY) (IB ITimes (Reg ynR) (ConstI tyN))])
aeval e _ = error (show e)

threadM :: Monad m => [a -> m a] -> a -> m a
threadM = foldr (<=<) pure

eval :: E (T ()) -> Temp -> IRM [Stmt]
eval (LLet _ (n, e') e) t | isF (eAnn e') = do
    f <- newFTemp
    plF <- eval e' f
    modify (addVar n f)
    (plF ++) <$> eval e t
eval (LLet _ (n, e') e) t | isI (eAnn e') = do
    t' <- newITemp
    plT <- eval e' t'
    modify (addVar n t')
    (plT ++) <$> eval e t
eval (LLet _ (n, e') e) t | isArr (eAnn e') = do
    t' <- newITemp
    (l, ss) <- aeval e' t'
    modify (addAVar n (l, t'))
    (ss ++) <$> eval e t
eval (LLet _ (n, e') e) t | (Arrow F F) <- eAnn e' = do
    l <- newLabel; endL <- newLabel; arg <- newFTemp; ret <- newFTemp
    ss <- writeRF e' [arg] ret
    modify (addFVar n (l, [(Nothing, arg)], (Nothing, ret)))
    (++ (J endL:L l:ss++[IR.R l, L endL])) <$> eval e t
eval (EApp _ (EApp _ (EApp _ (Builtin _ (Fold 1)) op) seed) (EApp _ (EApp _ (EApp _ (Builtin _ IRange) start) end) (ILit _ j))) acc = do
    i <- newITemp
    endR <- newITemp
    l <- newLabel; endL <- newLabel
    putStart <- eval start i; putAcc <- eval seed acc; irEnd <- eval end endR
    step <- writeRF op [acc, i] acc
    pure $ putStart ++ putAcc ++ irEnd ++ (L l:MJ (IRel IGt (Reg i) (Reg endR)) endL:step) ++ [MT i (IB IPlus (Reg i) (ConstI $ asI j)), J l, L endL]
eval (EApp _ (EApp _ (EApp _ (Builtin _ (Fold 1)) op) seed) (EApp _ (EApp _ (EApp _ (Builtin _ IRange) start) end) incr)) acc = do
    i <- newITemp
    endR <- newITemp
    incrR <- newITemp
    l <- newLabel; endL <- newLabel
    putStart <- eval start i; putAcc <- eval seed acc; irEnd <- eval end endR
    irIncr <- eval incr incrR
    step <- writeRF op [acc, i] acc
    -- TODO: is this shortest loop?
    pure $ putStart ++ putAcc ++ irEnd ++ irIncr ++ (L l:MJ (IRel IGt (Reg i) (Reg endR)) endL:step) ++ [MT i (IB IPlus (Reg i) (Reg incrR)), J l, L endL]
-- TODO: start, end, nSteps a literal
eval (EApp _ (EApp _ (EApp _ (Builtin _ (Fold 1)) op) seed) (EApp _ (EApp _ (EApp _ (Builtin _ FRange) (ILit _ start)) (ILit _ end)) (ILit _ nSteps))) acc = do
    i <- newITemp
    l <- newLabel; endL <- newLabel
    let incr = fromIntegral (end-start+1)/fromIntegral nSteps
    xR <- newFTemp
    putAcc <- eval seed acc
    step <- writeRF op [acc, xR] acc
    pure $ putAcc ++ (MX xR (ConstF $ fromIntegral start):MT i (ConstI 1):L l:MJ (IRel IGt (Reg i) (ConstI $ asI nSteps)) endL:step) ++ [tick i, MX xR (FB FPlus (FReg xR) (ConstF incr)), J l, L endL]
eval (EApp _ (EApp _ (EApp _ (Builtin _ (Fold 1)) op) seed) (EApp _ (EApp _ (EApp _ (Builtin _ FRange) start) end) nSteps@(EApp _ (Builtin _ Floor) nStepsF))) acc = do
    i <- newITemp
    startR <- newFTemp
    incrR <- newFTemp
    xR <- newFTemp
    endI <- newITemp
    l <- newLabel; endL <- newLabel
    putStart <- eval start startR; putAcc <- eval seed acc; putIEnd <- eval nSteps endI
    putIncr <- eval (((end `eMinus` start) `ePlus` FLit F 1) `eDiv` nStepsF) incrR
    -- step the accumulating value
    step <- writeRF op [acc, xR] acc
    pure $ putStart ++ (MX xR (FReg startR):putIEnd) ++ putIncr ++ putAcc ++ (MT i (ConstI 1):L l:MJ (IRel IGt (Reg i) (Reg endI)) endL:step) ++ [tick i, MX xR (FB FPlus (FReg xR) (FReg incrR)), J l, L endL]
eval (EApp _ (EApp _ (EApp _ (Builtin _ (Fold 1)) op) seed) (EApp _ (EApp _ (EApp _ (Builtin _ FRange) start) end) nSteps)) acc = do
    i <- newITemp
    startR <- newFTemp
    incrR <- newFTemp
    xR <- newFTemp
    endI <- newITemp
    l <- newLabel; endL <- newLabel
    putStart <- eval start startR
    putAcc <- eval seed acc
    putIEnd <- eval nSteps endI
    putIncr <- eval (((end `eMinus` start) `ePlus` FLit F 1) `eDiv` EApp F (Builtin (Arrow I F) ItoF) nSteps) incrR
    -- step the accumulating value
    step <- writeRF op [acc, xR] acc
    pure $ putStart ++ (MX xR (FReg startR):putIEnd) ++ putIncr ++ putAcc ++ (MT i (ConstI 1):L l:MJ (IRel IGt (Reg i) (Reg endI)) endL:step) ++ [tick i, MX xR (FB FPlus (FReg xR) (FReg incrR)), J l, L endL]
eval (EApp _ (EApp _ (Builtin (Arrow I _) Plus) e0) e1) t = do
    t0 <- newITemp; t1 <- newITemp
    pl0 <- eval e0 t0; pl1 <- eval e1 t1
    pure $ pl0 ++ pl1 ++ [MT t (IB IPlus (Reg t0) (Reg t1))]
eval (EApp _ (EApp _ (Builtin _ Plus) (Var F x)) (EApp _ (EApp _ (Builtin _ Times) e0) e1)) t = do
    st <- gets vars
    t0 <- newFTemp; t1 <- newFTemp
    pl0 <- eval e0 t0; pl1 <- eval e1 t1
    pure $ pl0 ++ pl1 ++ [MX t (FB FPlus (FReg $ getT st x) (FB FTimes (FReg t0) (FReg t1)))]
eval (EApp _ (EApp _ (Builtin (Arrow F _) Plus) e0) e1) t = do
    t0 <- newFTemp; t1 <- newFTemp
    pl0 <- eval e0 t0; pl1 <- eval e1 t1
    pure $ pl0 ++ pl1 ++ [MX t (FB FPlus (FReg t0) (FReg t1))]
eval (EApp _ (EApp _ (Builtin (Arrow I _) Times) e0) e1) t = do
    t0 <- newITemp; t1 <- newITemp
    pl0 <- eval e0 t0; pl1 <- eval e1 t1
    pure $ pl0 ++ pl1 ++ [MT t (IB ITimes (Reg t0) (Reg t1))]
eval (EApp _ (EApp _ (Builtin _ Minus) e) (ILit F i)) t = do
    tϵ <- newFTemp
    pl <- eval e tϵ
    pure $ pl ++ [MX t (FB FMinus (FReg tϵ) (ConstF $ fromIntegral i))]
eval (EApp _ (EApp _ (Builtin _ Minus) e) (ILit I i)) t = do
    tϵ <- newITemp
    pl <- eval e tϵ
    pure $ pl ++ [MT t (IB IMinus (Reg tϵ) (ConstI $ asI i))]
eval (EApp _ (EApp _ (Builtin (Arrow I _) Minus) e0) e1) t = do
    t0 <- newITemp; t1 <- newITemp
    pl0 <- eval e0 t0; pl1 <- eval e1 t1
    pure $ pl0 ++ pl1 ++ [MT t (IB IMinus (Reg t0) (Reg t1))]
eval (ILit F x) t = pure [MX t (ConstF $ fromIntegral x)] -- if it overflows you deserve it
eval (ILit _ i) t = pure [MT t (ConstI $ asI i)]
eval (Var F x) t = do
  st <- gets vars
  pure [MX t (FReg $ getT st x)]
eval (Var _ x) t = do
    st <- gets vars
    pure [MT t (Reg $ getT st x)]
eval (EApp _ (Builtin _ ItoF) (ILit _ i)) t = do
    pure [MX t (ConstF $ fromIntegral i)]
eval (EApp _ (Builtin _ ItoF) e) t = do
    iR <- newITemp
    pl<- eval e iR
    pure $ pl ++ [MX t (FConv $ Reg iR)]
eval (EApp _ (EApp _ (Builtin _ Div) e0) e1) t = do
    t0 <- newFTemp; t1 <- newFTemp
    pl0 <- eval e0 t0; pl1 <- eval e1 t1
    pure $ pl0 ++ pl1 ++ [MX t (FB FDiv (FReg t0) (FReg t1))]
eval (EApp _ (EApp _ (Builtin _ A.IDiv) e0) e1) t = do
    t0 <- newITemp; t1 <- newITemp
    pl0 <- eval e0 t0; pl1 <- eval e1 t1
    pure $ pl0 ++ pl1 ++ [MT t (IB IR.IDiv (Reg t0) (Reg t1))]
eval (EApp F (EApp _ (Builtin _ Times) e0) e1) t = do
    t0 <- newFTemp; t1 <- newFTemp
    pl0 <- eval e0 t0; pl1 <- eval e1 t1
    pure $ pl0 ++ pl1 ++ [MX t (FB FTimes (FReg t0) (FReg t1))]
eval (EApp F (EApp _ (Builtin _ Minus) e0) e1) t = do
    t0 <- newFTemp; t1 <- newFTemp
    pl0 <- eval e0 t0; pl1 <- eval e1 t1
    pure $ pl0 ++ pl1 ++ [MX t (FB FMinus (FReg t0) (FReg t1))]
eval (EApp F (EApp _ (Builtin _ Exp) (FLit _ x)) e) t = do
    f <- newFTemp
    plE <- eval e f
    pure $ plE ++ [MX t (FB FExp (ConstF x) (FReg f))]
eval (EApp F (EApp _ (Builtin _ Exp) e0) e1) t = do
    f0 <- newFTemp; f1ϵ <- newFTemp
    plE0 <- eval e0 f0; plE1 <- eval e1 f1ϵ
    pure $ plE0 ++ plE1 ++ [MX t (FB FExp (FReg f0) (FReg f1ϵ))]
eval (EApp F (EApp _ (Builtin _ IntExp) x) n) t = do
    i <- newITemp
    nR <- newITemp
    xR <- newFTemp
    plR <- eval n nR; plX <- eval x xR
    l <- newLabel; endL <- newLabel
    pure $ plR ++ plX ++ [MX t (ConstF 1), MT i (Reg nR), L l, MJ (IRel IEq (Reg i) (ConstI 0)) endL, MX t (FB FTimes (FReg t) (FReg xR)), MT i (IB IMinus (Reg i) (ConstI 1)), J l, L endL]
eval (EApp _ (EApp _ (Builtin _ IntExp) x) n) t = do
    i <- newITemp
    nR <- newITemp
    xR <- newITemp
    plR <- eval n nR; plX <- eval x xR
    l <- newLabel; endL <- newLabel
    pure $ plR ++ plX ++ [MT t (ConstI 1), MT i (Reg nR), L l, MJ (IRel IEq (Reg i) (ConstI 0)) endL, MT t (IB ITimes (Reg t) (Reg xR)), MT i (IB IMinus (Reg i) (ConstI 1)), J l, L endL]
eval (EApp _ (Builtin _ Floor) (Var _ x)) t = do
    st <- gets vars
    pure [MT t (IRFloor (FReg $ getT st x))]
eval (EApp _ (Builtin _ Floor) x) t = do
    fR <- newFTemp
    plX <- eval x fR
    pure $ plX ++ [MT t (IRFloor (FReg fR))]
eval (EApp _ (Builtin (Arrow F _) Neg) x) t = do
    fR <- newFTemp
    plX <- eval x fR
    pure $ plX ++ [MX t (FB FMinus (ConstF 0) (FReg fR))]
eval (FLit _ x) t = pure [MX t (ConstF x)]
eval (EApp _ (Builtin _ Sqrt) (FLit _ x)) t =
    pure [MX t (ConstF (sqrt x))]
eval (EApp _ (Builtin _ Sqrt) (ILit F x)) t =
    pure [MX t (ConstF (sqrt $ realToFrac x :: Double))]
eval (EApp _ (Builtin _ Sqrt) e) t = do
    eR <- newFTemp
    plE <- eval e eR
    pure $ plE ++ [MX t (FU FSqrt (FReg eR))]
eval (EApp _ (EApp _ (EApp _ (Builtin _ (Fold 1)) op) seed) e) acc | f1 (eAnn e) = do
    x <- newFTemp
    arrR <- newITemp
    eR <- newITemp
    szR <- newITemp
    i <- newITemp
    (mI, plE) <- aeval e eR
    putAcc <- eval seed acc
    stepR <- writeRF op [acc, x] acc
    let step = MX x (FAt (AP arrR (Just$IB IAsl (Reg i) (ConstI 3)) mI)):stepR
    loop <- doN i (Reg szR) step
    pure $ plE ++ putAcc ++ MT szR (EAt (AP eR (Just (ConstI 8)) mI)):MT arrR (IB IPlus (Reg eR) (ConstI 16)):loop
eval (EApp _ (EApp _ (EApp _ (Builtin _ FoldA) op) seed) e) acc | isFFF (eAnn op) = do
    x <- newFTemp
    eR <- newITemp; datR <- newITemp
    szR <- newITemp; rnkR <- newITemp
    i <- newITemp; j <- newITemp
    (mI, plE) <- aeval e eR
    putAcc <- eval seed acc
    stepR <- writeRF op [acc, x] acc
    let stepSz = MT szR (IB ITimes (Reg szR) (EAt (AP eR (Just (IB IPlus (IB IAsl (Reg j) (ConstI 3)) (ConstI 8))) mI)))
        step = MX x (FAt (AP datR (Just (IB IPlus (IB IAsl (Reg i) (ConstI 3)) (ConstI 8))) mI)):stepR
    szLoop <- doN j (Reg rnkR) [stepSz]
    loop <- doN i (Reg szR) step
    pure $ plE ++ putAcc ++ MT rnkR (EAt (AP eR Nothing mI)):MT datR (IB IPlus (Reg eR) (IB IAsl (Reg rnkR) (ConstI 3))):MT szR (ConstI 1):szLoop ++ loop
eval (EApp _ (EApp _ (EApp _ (Builtin _ Foldl) op) seed) e) acc | f1 (eAnn e) = do
    x <- newFTemp
    arrR <- newITemp
    eR <- newITemp
    i <- newITemp
    (mI, plE) <- aeval e eR
    putAcc <- eval seed acc
    l <- newLabel; endL <- newLabel
    stepR <- writeRF op [acc, x] acc
    let step = MX x (FAt (AP arrR (Just$IB IAsl (Reg i) (ConstI 3)) mI)):stepR ++ [MT i (IB IMinus (Reg i) (ConstI 1))]
    -- GHC uses 'length' but our szR needs to be one less
    pure $ plE ++ putAcc ++ MT i (EAt (AP eR (Just (ConstI 8)) mI)):MT arrR (IB IPlus (Reg eR) (ConstI 16)):L l:MJ (IRel ILt (Reg i) (ConstI 0)) endL:step++[J l, L endL]
eval (EApp _ (EApp _ (EApp _ (Builtin _ (Fold 1)) op) seed) e) acc | i1 (eAnn e) = do
    x <- newITemp
    arrR <- newITemp
    eR <- newITemp
    szR <- newITemp
    i <- newITemp
    (mI, plE) <- aeval e eR
    putAcc <- eval seed acc
    stepR <- writeRF op [acc, x] acc
    let step = MT x (EAt (AP arrR Nothing mI)):stepR ++ [MT arrR (IB IPlus (Reg arrR) (ConstI 8))]
    loop <- doN i (Reg szR) step
    -- GHC uses 'length' but our szR needs to be one less
    pure $ plE ++ putAcc ++ MT szR (EAt (AP eR (Just (ConstI 8)) mI)):MT arrR (IB IPlus (Reg eR) (ConstI 16)):loop
eval (Id F (FoldOfZip seed op [p, q])) acc | f1 (eAnn p) && f1 (eAnn q) = do
    x <- newFTemp; y <- newFTemp
    pR <- newITemp; qR <- newITemp
    szR <- newITemp
    arr0R <- newITemp; arr1R <- newITemp
    i <- newITemp
    (iP, plP) <- aeval p pR; (iQ, plQ) <- aeval q qR
    putAcc <- eval seed acc
    stepR <- writeRF op [acc, x, y] acc
    let step = MX x (FAt (AP arr0R (Just$IB IAsl (Reg i) (ConstI 3)) iP)):MX y (FAt (AP arr1R (Just$IB IAsl (Reg i) (ConstI 3)) iQ)):stepR
    loop <- doN i (Reg szR) step
    -- FIXME: this assumes the arrays are the same size
    pure $ plP ++ plQ ++ putAcc ++ MT szR (EAt (AP pR (Just (ConstI 8)) iP)):MT arr0R (IB IPlus (Reg pR) (ConstI 16)):MT arr1R (IB IPlus (Reg qR) (ConstI 16)):loop
eval (Id F (FoldOfZip seed op [EApp _ (EApp _ (EApp _ (Builtin _ IRange) start) _) incr, Id ty (AShLit [_] qs)])) acc | f1 ty = do
    x <- newITemp
    i <- newITemp
    y <- newFTemp
    plX <- eval start x; plI <- eval incr i; putAcc <- eval seed acc
    stepR <- writeRF op [acc, x, y] acc
    steps <- foldMapA (\q -> do { plY <- eval q y ; pure $ plY ++ stepR ++ [MT x (IB IPlus (Reg x) (Reg i))] }) qs -- FIXME: doesn't check arrays are same size
    pure $ plX ++ plI ++ putAcc ++ steps
eval (EApp _ (Builtin _ Log) (Var _ x)) t = do
    st <- gets vars
    pure [MX t (FU FLog (FReg $ getT st x))]
eval (EApp _ (Builtin _ Log) e) t = do
    t' <- newFTemp
    plE <- eval e t'
    pure $ plE ++ [MX t (FU FLog (FReg t'))]
eval (EApp _ (Builtin _ Sin) e) t = do
    f <- newFTemp
    plE <- eval e f
    pure $ plE ++ [MX t (FU FSin (FReg f))]
eval (EApp _ (Builtin _ Size) e) t | unDim (eAnn e) = do
    r <- newITemp
    (mI, plE) <- aeval e r
    pure $ plE ++ [MT t (EAt (AP r (Just (ConstI 8)) mI))]
eval (EApp _ (Builtin _ Size) e) t = do
    r <- newITemp
    (mI, plE) <- aeval e r
    rnkR <- newITemp
    l <- newLabel; endL <- newLabel
    i <- newITemp
    pure $ plE ++ [MT rnkR (EAt (AP r Nothing mI)), MT i (ConstI 8), MT t (EAt (AP r (Just (ConstI 8)) mI)), L l, MJ (IRel IGt (Reg i) (Reg rnkR)) endL, MT i (IB IPlus (Reg i) (ConstI 8)), MT t (IB ITimes (Reg t) (EAt (AP r (Just (Reg i)) mI))),J l, L endL]
eval (EApp I (EApp _ (Builtin _ Max) e0) e1) t = do
    e0R <- newITemp; e1R <- newITemp
    plE0 <- eval e0 e0R; plE1 <- eval e1 e1R
    pure $ plE0 ++ plE1 ++ [MT t (Reg e1R), Cmov (IRel IGt (Reg e0R) (Reg e1R)) t (Reg e0R)]
eval (Cond F (EApp _ (EApp _ (Builtin (Arrow F _) Gte) c0) c1) e0 e1) t = do
    c0R <- newFTemp; c1R <- newFTemp
    plC0 <- eval c0 c0R; plC1 <- eval c1 c1R
    fR <- newFTemp; plE0 <- eval e0 fR; plE1 <- eval e1 fR
    l <- newLabel; nextL <- newLabel
    pure $ plC0 ++ plC1 ++ MJ (FRel FGeq (FReg c0R) (FReg c1R)) l:plE1 ++ J nextL:L l:plE0 ++ [L nextL, MX t (FReg fR)]
eval (Cond F (EApp _ (EApp _ (Builtin (Arrow I _) Gt) c0) c1) e0 e1) t = do
    c0R <- newITemp; c1R <- newITemp
    plC0 <- eval c0 c0R; plC1 <- eval c1 c1R
    fR <- newFTemp; plE0 <- eval e0 fR; plE1 <- eval e1 fR
    l <- newLabel; nextL <- newLabel
    pure $ plC0 ++ plC1 ++ MJ (IRel IGeq (Reg c0R) (Reg c1R)) l:plE1 ++ J nextL:L l:plE0 ++ [L nextL, MX t (FReg fR)]
eval (Cond F (EApp _ (EApp _ (Builtin (Arrow I _) Eq) c0) c1) e0 e1) t = do
    c0R <- newITemp; c1R <- newITemp
    plC0 <- eval c0 c0R; plC1 <- eval c1 c1R
    fR <- newFTemp; plE0 <- eval e0 fR; plE1 <- eval e1 fR
    l <- newLabel; nextL <- newLabel
    pure $ plC0 ++ plC1 ++ MJ (IRel IEq (Reg c0R) (Reg c1R)) l:plE1 ++ J nextL:L l:plE0 ++ [L nextL, MX t (FReg fR)]
eval (Cond F (EApp _ (EApp _ (Builtin (Arrow I _) Neq) c0) c1) e0 e1) t = do
    c0R <- newITemp; c1R <- newITemp
    plC0 <- eval c0 c0R; plC1 <- eval c1 c1R
    fR <- newFTemp; plE0 <- eval e0 fR; plE1 <- eval e1 fR
    l <- newLabel; nextL <- newLabel
    pure $ plC0 ++ plC1 ++ MJ (IRel INeq (Reg c0R) (Reg c1R)) l:plE1 ++ J nextL:L l:plE0 ++ [L nextL, MX t (FReg fR)]
eval (Cond F (EApp _ (EApp _ (Builtin (Arrow I _) Lt) c0) c1) e0 e1) t = do
    c0R <- newITemp; c1R <- newITemp
    plC0 <- eval c0 c0R; plC1 <- eval c1 c1R
    fR <- newFTemp; plE0 <- eval e0 fR; plE1 <- eval e1 fR
    l <- newLabel; nextL <- newLabel
    pure $ plC0 ++ plC1 ++ MJ (IRel ILt (Reg c0R) (Reg c1R)) l:plE1 ++ J nextL:L l:plE0 ++ [L nextL, MX t (FReg fR)]
eval (Cond F (EApp _ (EApp _ (Builtin (Arrow I _) Lte) c0) c1) e0 e1) t = do
    c0R <- newITemp; c1R <- newITemp
    plC0 <- eval c0 c0R; plC1 <- eval c1 c1R
    fR <- newFTemp; plE0 <- eval e0 fR; plE1 <- eval e1 fR
    l <- newLabel; nextL <- newLabel
    pure $ plC0 ++ plC1 ++ MJ (IRel ILeq (Reg c0R) (Reg c1R)) l:plE1 ++ J nextL:L l:plE0 ++ [L nextL, MX t (FReg fR)]
eval (EApp F (Builtin _ Head) arr) t = do
    r <- newITemp
    (mL, plArr) <- aeval arr r
    -- rank 1
    pure $ plArr ++ [MX t (FAt (AP r (Just $ ConstI 16) mL))]
eval (EApp I (Builtin _ Head) arr) t = do
    r <- newITemp
    (mL, plArr) <- aeval arr r
    -- rank 1
    pure $ plArr ++ [MT t (EAt (AP r (Just $ ConstI 16) mL))]
eval (EApp I (Builtin _ Last) arr) t = do
    r <- newITemp
    (l, plArr) <- aeval arr r
    pure $ plArr ++ [MT t (EAt (AP r (Just (IB IPlus (IB IAsl (EAt (AP r (Just$ConstI 8) l)) (ConstI 3)) (ConstI 8))) l))]
eval (EApp F (Builtin _ Last) arr) t = do
    r <- newITemp
    (l, plArr) <- aeval arr r
    pure $ plArr ++ [MX t (FAt (AP r (Just (IB IPlus (IB IAsl (EAt (AP r (Just$ConstI 8) l)) (ConstI 3)) (ConstI 8))) l))]
eval (EApp ty@P{} (Builtin _ Last) arr) t = do
    r <- newITemp
    (l, plArr) <- aeval arr r
    pure $ plArr ++ [Cpy (AP t Nothing Nothing) (AP r (Just (IB IPlus (IB ITimes (IB IMinus (EAt (AP r (Just$ConstI 8) l)) (ConstI 1)) (ConstI$bT ty)) (ConstI 16))) l) (ConstI$bT ty `div` 8)]
eval (Tup _ es) t = do
    let szs = szT (eAnn<$>es)
    pls <- zipWithM (\e sz -> case eAnn e of {F -> do{fr <- newFTemp; p <- eval e fr; pure$p++[WrF (AP t (Just$ConstI sz) Nothing) (FReg fr)]};I -> do{r <- newITemp; p <- eval e r; pure$p++[Wr (AP t (Just$ConstI sz) Nothing) (Reg r)]}}) es szs
    pure$concat pls
eval (EApp F (Builtin _ (TAt n)) e@Var{}) t = do
    let (P tys) = eAnn e; szs = szT tys
    r <- newITemp; pl <- eval e r
    pure $ pl ++ [MX t (FAt (AP r (Just$ConstI (szs!!(n-1))) Nothing))]
eval (EApp F (Builtin _ (TAt n)) e) t = do
    let (P tys) = eAnn e; szs = szT tys; sz = fromIntegral (last szs)
    r <- newITemp; pl <- eval e r
    pure $ Sa r sz:pl ++ [MX t (FAt (AP r (Just$ConstI (szs!!(n-1))) Nothing)), Pop sz]
eval (EApp I (Builtin _ (TAt n)) e@Var{}) t = do
    let (P tys) = eAnn e; szs = szT tys
    r <- newITemp; pl <- eval e r
    pure $ pl ++ [MT t (EAt (AP r (Just$ConstI (szs!!(n-1))) Nothing))]
eval (EApp I (Builtin _ (TAt n)) e) t = do
    let (P tys) = eAnn e; szs = szT tys; sz = fromIntegral (last szs)
    r <- newITemp; pl <- eval e r
    pure $ Sa r sz:pl ++ [MT t (EAt (AP r (Just$ConstI (szs!!(n-1))) Nothing)), Pop sz]
eval (EApp F (Var _ f) e) t | isF (eAnn e) = do
    st <- gets fvars
    let (l, [(Nothing, arg)], (Nothing, ret)) = getT st f
    plE <- eval e arg
    retL <- newLabel
    pure $ plE ++ [C l, L retL, MX t (FReg ret)]
eval e _ = error (show e)

foldMapA :: (Applicative f, Traversable t, Monoid m) => (a -> f m) -> t a -> f m
foldMapA = (fmap fold .) . traverse

-- 1-dim'l array of floats
f1 :: T a -> Bool
f1 (Arr (_ `Cons` Nil) F) = True
f1 _                      = False

i1 :: T a -> Bool
i1 (Arr (_ `Cons` Nil) I) = True
i1 _                      = False

bT :: Integral b => T a -> b
bT (P ts) = sum (bT<$>ts)
bT F      = 8
bT I      = 8
bT Arr{}  = 8

szT = scanl' (\off ty -> off+bT ty) 0

unDim :: T a -> Bool
unDim (Arr (_ `Cons` Nil) _) = True
unDim _                      = False

asI :: Integer -> Int64
asI i | i < fromIntegral (minBound :: Int64) || i > fromIntegral (maxBound :: Int64) = error "integer literal out of bounds!"
      | otherwise = fromIntegral i
