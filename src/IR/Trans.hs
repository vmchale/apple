{-# LANGUAGE TupleSections #-}

module IR.Trans ( writeC
                ) where

import           A
import           Control.Monad              (zipWithM, (<=<))
import           Control.Monad.State.Strict (State, gets, modify, runState)
import           Data.Bifunctor             (second)
import           Data.Either                (lefts, rights)
import           Data.Foldable              (fold)
import           Data.Functor               (($>))
import           Data.Int                   (Int64)
import qualified Data.IntMap                as IM
import qualified Data.IntSet                as IS
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

isFF :: T a -> Bool
isFF (Arrow F F) = True
isFF _           = False

isAF :: T a -> Bool
isAF (Arrow Arr{} F) = True
isAF _               = False

mA1A1 :: T a -> Maybe (T a, T a)
mA1A1 (Arrow (Arr (_ `Cons` Nil) t0) (Arr (_ `Cons` Nil) t1)) = Just (t0, t1)
mA1A1 _                                                       = Nothing

isF :: T a -> Bool
isF F = True
isF _ = False

isI :: T a -> Bool
isI I = True
isI _ = False

isB :: T a -> Bool; isB B = True; isB _ = False

isArr Arr{} = True
isArr _     = False

writeC :: E (T ()) -> ([Stmt], WSt, IM.IntMap Temp)
writeC = π.flip runState (IRSt [0..] [0..] [0..] IM.empty IM.empty IM.empty IM.empty) . writeCM where π (s, IRSt l t _ _ _ _ a) = (s, WSt l t, a)

-- %xmm0 – %xmm7
writeCM :: E (T ()) -> IRM [Stmt]
writeCM e' = do
    cs <- traverse (\_ -> newITemp) [(0::Int)..5]; fs <- traverse (\_ -> newFTemp) [(0::Int)..5]
    (zipWith (\xr xr' -> MX xr' (FReg xr)) [F0,F1,F2,F3,F4,F5] fs ++) . (zipWith (\r r' -> MT r' (Reg r)) [C0,C1,C2,C3,C4,C5] cs ++) <$> go e' fs cs where
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
             | isArr (eAnn e) = do {i <- newITemp; (l,r) <- aeval e i; pure$case l of {Just m -> r++[MT CRet (Reg i), RA m]}}
             | P [F,F] <- eAnn e = do {t<- newITemp; p <- eval e t; pure$Sa t 16:p++[MX FRet (FAt (AP t Nothing Nothing)), MX FRet1 (FAt (AP t (Just 8) Nothing)), Pop 16]}
             | ty@P{} <- eAnn e = let b64=fromIntegral$bT ty; b=ConstI b64 in do {t <- newITemp; a <- nextArr; pl <- eval e t; pure $ Sa t b:pl ++ [Ma a CRet (ConstI b64), Cpy (AP CRet Nothing (Just a)) (AP t Nothing Nothing) (ConstI $ b64`div`8), Pop b, RA a]}
             | otherwise = error ("Unsupported return type: " ++ show (eAnn e))

writeRF :: E (T ()) -> [Temp] -> Temp -> IRM [Stmt]
writeRF e rs = fmap snd . writeF e ((Nothing,) <$> rs)

dim1 a t n = [Wr (AP t Nothing a) 1, Wr (AP t (Just 8) a) n]
tick reg = MT reg (Reg reg + 1)
sd ireg = IB IAsl (Reg ireg) 3
sib ireg = sd ireg+16
sib1 ireg = sd ireg+24
gd1 l a = EAt (AP a (Just 8) l)

stadim a t ns = Wr (AP t Nothing a) (ConstI (fromIntegral$length ns)):zipWith (\o n -> Wr (AP t (Just (ConstI$8*o)) a) n) [1..] ns

-- each (Right t) specifies a dimension; each (Left e) specifies a (currently)
-- fixed index; looping variables for copying are generated in the function body
extrCell :: [Either Exp Temp] -> [Temp] -> (Temp, Maybe Int) -> Temp -> IRM [Stmt]
extrCell fixedIxesDims sstrides src dest = do -- dims are bounds
    ts <- traverse (\_ -> newITemp) dims
    t <- newITemp; i <- newITemp
    fmap (MT i 0:) $ threadM (zipWith (\d tϵ s -> doN tϵ (Reg d) s) dims ts) $
        let ixes = either id Reg <$> replaceZs fixedIxesDims ts
            sp = xp (Reg <$> sstrides) ixes src
            -- TODO: what if size of data is not 8?
        in [MT t (EAt sp), Wr (AP dest (Just (IB IAsl (Reg i) 3)) Nothing) (Reg t), tick i]
    where dims = rights fixedIxesDims
          replaceZs (f@Left{}:ds) ts    = f:replaceZs ds ts
          replaceZs (Right{}:ds) (t:ts) = Right t:replaceZs ds ts
          replaceZs [] []               = []

-- incr.
doI t el eu rel ss = do
    l <- newLabel; endL <- newLabel
    pure $ MT t el:L l:MJ (IRel rel (Reg t) eu) endL:ss++[tick t, J l, L endL]

doN t e = doI t 0 e IGeq; doN1 t e = doI t 1 e IGt; fN1 t e = doI t 1 e IGeq

man (a, t) rnk n = Ma a t (IB IAsl n 3 + ConstI (8+8*rnk))

staRnk :: Integral b => Sh a -> Maybe b
staRnk Nil           = Just 0
staRnk (_ `Cons` sh) = (1+) <$> staRnk sh
staRnk _             = Nothing

tRnk :: T a -> Maybe (T a, Int64)
tRnk (Arr sh t) = (t,) <$> staRnk sh
tRnk _          = Nothing

dimR rnk (t,l) = (\o -> EAt (AP t (Just$ConstI (8*o)) l)) <$> [1..rnk]

plDim :: Int64 -> (Temp, Maybe Int) -> IRM ([Temp], [Stmt])
plDim rnk (t,l) = do
    let dimE = dimR rnk (t, l)
    unzip <$> traverse (\e -> do {dt <- newITemp; pure (dt, MT dt e)}) dimE

offByDim :: [Exp] -> IRM ([Temp], [Stmt])
offByDim dims = do
    ts <- traverse (\_ -> newITemp) (undefined:dims)
    let ss=zipWith3 (\t1 t0 d -> MT t1 (Reg t0 * d)) (tail ts) ts dims
    pure (reverse ts, MT (head ts) 1:ss)
    -- drop 1 for strides

xp strides ixs (t,l) =
    let offs=foldl1 (IB IPlus) $ zipWith (\d i -> i*d) strides ixs
    -- FIXME: data not of size 8
    in AP t (Just (IB IAsl offs 3)) l

stacopy sdims ddims x ls ad as = do
    t <- newITemp
    let eachIx = crossN $ fmap (\l -> [0..(l-1)]) ls
    pure $ concat [ [ MT t (EAt (xp sdims (at is) as)), Wr (xp ddims (at is) ad) (Reg t) ] | is <- eachIx ]
    where at = zipWith (\x_a i -> x_a + ConstI i) x

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
eDiv = fop Div

aeval :: E (T ()) -> Temp -> IRM (Maybe Int, [Stmt])
aeval (Var Arr{} x) t = do
    st <- gets avars
    let (i, r) = getT st x
    pure (i, [MT t (Reg r)])
aeval (EApp res (EApp _ (Builtin _ Cyc) xs) n) t | i1 res || f1 res = do
    a <- nextArr
    xR <- newITemp; i <- newITemp; nR <- newITemp; nO <- newITemp
    (lX, plX) <- aeval xs xR
    szR <- newITemp; ix <- newITemp
    plN <- eval n nR
    body <- doN i (Reg nR) [Cpy (AP t (Just$sib ix) (Just a)) (AP xR (Just 16) lX) (Reg szR), MT ix (Reg ix + Reg szR)]
    pure (Just a, plX ++ MT szR (gd1 lX xR):plN ++ MT nO (Reg szR * Reg nR):man (a,t) 1 (Reg nO):dim1 (Just a) t (Reg nO) ++ MT ix 0:body)
aeval (EApp res (EApp _ (Builtin _ Map) op) e) t | f1 (eAnn e) && f1 res = do
    a <- nextArr
    arrP <- newITemp
    (l, plE) <- aeval e arrP
    -- cause f1 (skip rank)
    let sz = EAt (AP arrP (Just 8) l)
    f <- newFTemp
    ss <- writeRF op [f] f
    iR <- newITemp; szR <- newITemp
    let loopBody = MX f (FAt (AP arrP (Just (sib iR)) l)):ss++[WrF (AP t (Just (sib iR)) (Just a)) (FReg f)]
    loop <- doN iR (Reg szR) loopBody
    modify (addMT a t)
    pure (Just a, plE ++ MT szR sz:man (a,t) 1 (Reg szR):dim1 (Just a) t (Reg szR)++loop)
aeval (EApp res (EApp _ (EApp _ (Builtin _ Zip) op) xs) ys) t | f1 (eAnn xs) && f1 (eAnn ys) && f1 res = do
    a <- nextArr
    arrPX <- newITemp; arrPY <- newITemp
    (lX, plEX) <- aeval xs arrPX; (lY, plEY) <- aeval ys arrPY
    let sz = EAt (AP arrPX (Just 8) lX)
    x <- newFTemp; y <- newFTemp; z <- newFTemp
    ss <- writeRF op [x,y] z
    iR <- newITemp
    szR <- newITemp
    let loopBody = let ireg = Just (sib iR) in MX x (FAt (AP arrPX ireg lX)):MX y (FAt (AP arrPY ireg lY)):ss++[WrF (AP t ireg (Just a)) (FReg z)]
    loop <- doN iR (Reg szR) loopBody
    modify (addMT a t)
    pure (Just a, plEX ++ plEY ++ MT szR sz:man (a,t) 1 (Reg szR):dim1 (Just a) t (Reg szR) ++ loop)
aeval (EApp res (EApp _ (Builtin _ Scan) op) xs) t | f1 (eAnn xs) && f1 res = do
    a <- nextArr
    arrP <- newITemp
    acc <- newFTemp; x <- newFTemp
    (l, plE) <- aeval xs arrP
    let sz = EAt (AP arrP (Just 8) l)
    ss <- writeRF op [acc, x] acc
    iR <- newITemp; szR <- newITemp
    let loopBody = MX x (FAt (AP arrP (Just$sib iR) l)):WrF (AP t (Just (IB IAsl (Reg iR) 3 + 8)) (Just a)) (FReg acc):ss
    loop <- doN1 iR (Reg szR) loopBody
    modify (addMT a t)
    pure (Just a, plE ++ MT szR sz:man (a, t) 1 (Reg szR):dim1 (Just a) t (Reg szR) ++ MX acc (FAt (AP arrP (Just 16) l)):loop)
aeval (EApp res (EApp _ (EApp _ (Builtin _ ScanS) op) seed) e) t | f1 (eAnn e) && f1 res && isF (eAnn seed) = do
    a <- nextArr
    arrP <- newITemp
    acc <- newFTemp
    plSeed <- eval seed acc
    (l, plE) <- aeval e arrP
    let sz = EAt (AP arrP (Just 8) l)
    n <- newFTemp
    ss <- writeRF op [acc, n] acc
    iR <- newITemp; szR <- newITemp
    let loopBody = MX n (FAt (AP arrP (Just$sib iR) l)):WrF (AP t (Just (sib iR)) (Just a)) (FReg acc):ss
    loop <- doN iR (Reg szR) loopBody
    modify (addMT a t)
    pure (Just a, plE ++ plSeed ++ MT szR (sz+1):Ma a t (IB IAsl (Reg szR) 3 + 16):dim1 (Just a) t (Reg szR) ++ loop)
aeval (EApp res (EApp _ (EApp _ (Builtin _ ScanS) op) seed) e) t | i1 (eAnn e) && i1 res && isI (eAnn seed) = do
    a <- nextArr
    arrP <- newITemp
    acc <- newITemp
    plSeed <- eval seed acc
    (l, plE) <- aeval e arrP
    -- rank1
    let sz = gd1 l arrP
    n <- newITemp
    ss <- writeRF op [acc, n] acc
    iR <- newITemp
    szR <- newITemp
    let loopBody=MT n (EAt (AP arrP (Just (sib iR)) l)):Wr (AP t (Just (sib iR)) (Just a)) (Reg acc):ss
    loop <- doN iR (Reg szR) loopBody
    modify (addMT a t)
    pure (Just a, plE ++ plSeed ++ MT szR (sz+1):Ma a t (IB IAsl (Reg szR) 3 + 16):dim1 (Just a) t (Reg szR) ++ loop)
aeval (EApp res (EApp _ (Builtin _ Map) op) e) t | i1 (eAnn e) && i1 res = do
    a <- nextArr
    arrP <- newITemp
    (l, plE) <- aeval e arrP
    -- cause f1 (skip rank)
    let sz = EAt (AP arrP (Just 8) l)
    m <- newITemp
    ss <- writeRF op [m] m
    iR <- newITemp
    szR <- newITemp
    let loopBody = MT m (EAt (AP arrP (Just (sib iR)) l)):ss++[Wr (AP t (Just (sib iR)) (Just a)) (Reg m)]
    loop <- doN iR (Reg szR) loopBody
    modify (addMT a t)
    pure (Just a, plE ++ MT szR sz:Ma a t (IB IAsl (Reg szR) 3 + 16):Wr (AP t Nothing (Just a)) 1:Wr (AP t (Just 8) (Just a)) (Reg szR):loop)
aeval (EApp _ (EApp _ (EApp _ (Builtin _ IRange) start) end) (ILit _ 1)) t = do
    a <- nextArr
    n <- newITemp
    startR <- newITemp
    endR <- newITemp
    i <- newITemp
    putStart <- eval start startR; putEnd <- eval end endR
    modify (addMT a t)
    let putN = MT n ((Reg endR-Reg startR) + 1)
    let step = [Wr (AP t (Just (sib i)) (Just a)) (Reg startR), tick startR]
    loop <- doN i (Reg n) step
    pure (Just a, putStart++putEnd++putN:man (a, t) 1 (Reg n):dim1 (Just a) t (Reg n)++MT i 16:loop)
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
    let putN = MT n (IB IR.IDiv (Reg endR - Reg startR) (Reg incrR))
    let loop = [MJ (IRel IGt (Reg startR) (Reg endR)) endL, Wr (AP t (Just (Reg i)) (Just a)) (Reg startR), MT startR (Reg startR+Reg incrR), MT i (Reg i + 8)]
    pure (Just a, putStart++putEnd++putIncr++putN:Ma a t (IB IAsl (Reg n) 3 + 24):Wr (AP t Nothing (Just a)) 1:Wr (AP t (Just 8) (Just a)) (Reg n):MT i 16:L l:loop ++ [J l, L endL])
aeval (EApp _ (EApp _ (EApp _ (Builtin _ FRange) start) end) nSteps) t = do
    a <- nextArr
    i <- newITemp
    startR <- newFTemp
    incrR <- newFTemp
    n <- newITemp
    putStart <- eval start startR; putN <- eval nSteps n
    modify (addMT a t)
    putIncr <- eval ((end `eMinus` start) `eDiv` (EApp F (Builtin (Arrow I F) ItoF) nSteps `eMinus` FLit F 1)) incrR
    let loopBody = [WrF (AP t (Just (sib i)) (Just a)) (FReg startR), MX startR (FReg startR + FReg incrR)]
    loop <- doN i (Reg n) loopBody
    pure (Just a, putStart ++ putIncr ++ putN ++ man (a,t) 1 (Reg n):dim1 (Just a) t (Reg n) ++ loop)
aeval (EApp oTy (EApp _ (Builtin _ Succ) op) arr) t | f1 (eAnn arr) && f1 oTy = do
    a <- nextArr
    arrP <- newITemp
    szR <- newITemp; szR' <- newITemp
    fArg0R <- newFTemp; fArg1R <- newFTemp; fRetR <- newFTemp
    (arrL, putX) <- aeval arr arrP
    -- f1 (skip rank)
    let sz = EAt (AP arrP (Just 8) arrL)
    i <- newITemp
    ss <- writeRF op [fArg0R, fArg1R] fRetR
    let loopBody = MX fArg1R (FAt (AP arrP (Just (sib i)) arrL)):MX fArg0R (FAt (AP arrP (Just (sib1 i)) arrL)):ss++[WrF (AP t (Just (sib i)) (Just a)) (FReg fRetR)]
    loop <- doN i (Reg szR') loopBody
    modify (addMT a t)
    pure (Just a, putX ++ MT szR sz:MT szR' (Reg szR - 1):man (a,t) 1 (Reg szR'):dim1 (Just a) t (Reg szR') ++ loop)
aeval (EApp oTy (EApp _ (Builtin _ Succ) op) arr) t | i1 (eAnn arr) && i1 oTy = do
    a <- nextArr
    arrP <- newITemp
    szR <- newITemp; szR' <- newITemp
    arg0R <- newITemp; arg1R <- newITemp; retR <- newITemp
    (arrL, putX) <- aeval arr arrP
    -- f1 (skip rank)
    let sz = EAt (AP arrP (Just 8) arrL)
    i <- newITemp
    ss <- writeRF op [arg0R, arg1R] retR
    let loopBody = MT arg1R (EAt (AP arrP (Just (sib i)) arrL)):MT arg0R (EAt (AP arrP (Just (sib1 i)) arrL)):ss++[Wr (AP t (Just (sib i)) (Just a)) (Reg retR)]
    loop <- doN i (Reg szR) loopBody
    modify (addMT a t)
    pure (Just a, putX ++ MT szR sz:MT szR' (Reg szR - 1):man (a,t) 1 (Reg szR'):dim1 (Just a) t (Reg szR') ++ loop)
aeval (EApp oTy (EApp _ (Builtin _ (DI n)) op) arr) t | f1 (eAnn arr) && f1 oTy = do
    a <- nextArr
    arrP <- newITemp
    slopP <- newITemp
    szR <- newITemp
    fR <- newFTemp
    (arrL, putX) <- aeval arr arrP
    -- cause f1 (skip rank)
    let sz = EAt (AP arrP (Just 8) arrL)
        nIr = fromIntegral$16+n*8
    iR <- newITemp
    ss <- writeRF op [slopP] fR
    let loopBody = Cpy (AP slopP (Just 16) Nothing) (AP arrP (Just (sib iR)) arrL) (ConstI$fromIntegral n+2):ss++[WrF (AP t (Just (sib iR)) arrL) (FReg fR)]
    loop <- doN iR (Reg szR - ConstI (fromIntegral n-1)) loopBody
    modify (addMT a t)
    pure (Just a, putX++MT szR sz:Ma a t (IB IAsl (Reg szR) 3+ConstI (24-8*fromIntegral n)):Wr (AP t Nothing (Just a)) 1:Wr (AP t (Just 8) (Just a)) (Reg szR - ConstI (fromIntegral n-1)):Sa slopP (ConstI nIr):Wr (AP slopP Nothing Nothing) 1:Wr (AP slopP (Just 8) Nothing) (ConstI $ fromIntegral n):loop ++ [Pop (ConstI nIr)])
aeval (EApp oTy (EApp _ (EApp _ (Builtin _ Gen) seed) op) n) t | i1 oTy = do
    a <- nextArr
    arg <- newITemp
    i <- newITemp
    nR <- newITemp
    let sz = Reg nR + 16
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
        nE=ConstI$asI n;sz = nE*pt + ConstI (16+ptN)
    modify (addMT a t)
    putSeed <- eval seed arg
    l <- newLabel; endL <- newLabel
    ss <- writeRF op [arg] arg
    let loop = ss ++ [Cpy (AP t (Just (Reg i * pt + 16)) (Just a)) (AP arg Nothing Nothing) (ConstI$ptN`div`8)]
    pure (Just a, Ma a t sz:dim1 (Just a) t nE ++ Sa arg pt:putSeed ++ MT i 0:L l:MJ (IRel IGeq (Reg i) nE) endL:loop ++ [tick i, J l, L endL, Pop pt])
aeval (EApp oTy (EApp _ (EApp _ (Builtin _ Gen) seed) op) n) t | (Arr (_ `Cons` Nil) ty@P{}) <- oTy = do
    a <- nextArr
    arg <- newITemp
    i <- newITemp
    nR <- newITemp
    let ptN :: Integral a => a; ptN=bT ty;pt = ConstI ptN
        sz = (Reg nR * pt) + ConstI (16+ptN)
    modify (addMT a t)
    putSeed <- eval seed arg; putN <- eval n nR
    ss <- writeRF op [arg] arg
    let loopBody = ss ++ [Cpy (AP t (Just ((Reg i * pt) + 16)) (Just a)) (AP arg Nothing Nothing) (ConstI$ptN`div`8)]
    loop <- doN i (Reg nR) loopBody
    pure (Just a, putN ++ Ma a t sz:dim1 (Just a) t (Reg nR) ++ Sa arg (ConstI ptN):putSeed ++ loop ++ [Pop (ConstI ptN)])
aeval (EApp oTy (EApp _ (Builtin _ Re) n) x) t | f1 oTy = do
    a <- nextArr
    xR <- newFTemp
    nR <- newITemp
    i <- newITemp
    let sz = Reg nR + 16
    putN <- eval n nR; putX <- eval x xR
    let step = WrF (AP t (Just (sib i)) (Just a)) (FReg xR)
    loop <- doN i (Reg nR) [step]
    modify (addMT a t)
    pure (Just a, putX ++ putN ++ Ma a t sz:dim1 (Just a) t (Reg nR) ++ loop)
aeval (EApp oTy (EApp _ (Builtin _ Re) n) x) t | i1 oTy = do
    a <- nextArr
    xR <- newITemp
    nR <- newITemp
    i <- newITemp
    let sz = Reg nR + 16
    putN <- eval n nR; putX <- eval x xR
    let step = Wr (AP t (Just (sib i)) (Just a)) (Reg xR)
    loop <- doN i (Reg nR) [step]
    modify (addMT a t)
    pure (Just a, putX ++ putN ++ Ma a t sz:dim1 (Just a) t (Reg nR) ++ loop)
aeval (Id _ (AShLit ns es)) t | isF (eAnn$head es) = do
    a <- nextArr
    xR <- newFTemp
    let ne=product ns; rnk=fromIntegral$length ns;sz=ConstI$8*fromIntegral ne+24
    modify (addMT a t)
    steps <- concat<$>zipWithM (\i e -> do {ss <- eval e xR; pure$ss++[WrF (AP t (Just (ConstI$8*rnk+8*i)) (Just a)) (FReg xR)]}) [1..fromIntegral ne] es
    pure (Just a, Ma a t sz:stadim (Just a) t (ConstI .fromIntegral<$>ns) ++ steps)
aeval (Id _ (AShLit ns es)) t | isI (eAnn$head es) = do
    a <- nextArr
    xR <- newITemp
    let ne=product ns; rnk=fromIntegral$length ns;sz=ConstI$8*fromIntegral ne+24
    modify (addMT a t)
    steps <- concat<$>zipWithM (\i e -> do {ss <- eval e xR; pure$ss++[Wr (AP t (Just (ConstI$8*rnk+8*i)) (Just a)) (Reg xR)]}) [1..fromIntegral ne] es
    pure (Just a, Ma a t sz:stadim (Just a) t (ConstI .fromIntegral<$>ns) ++ steps)
aeval (EApp res (EApp _ (Builtin _ ConsE) x) xs) t | i1 res = do
    a <- nextArr
    xR <- newITemp; xsR <- newITemp
    plX <- eval x xR
    (l, plXs) <- aeval xs xsR
    nR <- newITemp; nϵR <- newITemp
    let nϵ = EAt (AP xsR (Just 8) l); n = Reg nϵR + 1
    modify (addMT a t)
    pure (Just a, plX ++ plXs ++ MT nϵR nϵ:MT nR n:man (a,t) 1 (Reg nR):dim1 (Just a) t (Reg nR) ++ [Cpy (AP t (Just 24) (Just a)) (AP xsR (Just 16) l) (Reg nϵR), Wr (AP t (Just 16) (Just a)) (Reg xR)])
aeval (EApp res (EApp _ (Builtin _ ConsE) x) xs) t | f1 res = do
    a <- nextArr
    xR <- newFTemp; xsR <- newITemp
    plX <- eval x xR
    (l, plXs) <- aeval xs xsR
    nR <- newITemp; nϵR <- newITemp
    let nϵ = EAt (AP xsR (Just 8) l); n = Reg nϵR + 1
    modify (addMT a t)
    pure (Just a, plXs ++ MT nϵR nϵ:MT nR n:man (a,t) 1 (Reg nR):dim1 (Just a) t (Reg nR) ++ plX ++ [Cpy (AP t (Just 24) (Just a)) (AP xsR (Just 16) l) (Reg nϵR), WrF (AP t (Just 16) (Just a)) (FReg xR)])
aeval (EApp res (EApp _ (Builtin _ Snoc) x) xs) t | f1 res = do
    a <- nextArr
    xR <- newFTemp; xsR <- newITemp
    plX <- eval x xR
    (l, plXs) <- aeval xs xsR
    nR <- newITemp; nϵR <- newITemp
    let nϵ=EAt (AP xsR (Just 8) l); n=Reg nϵR + 1
    modify (addMT a t)
    pure (Just a, plXs ++ plX ++ MT nϵR nϵ:MT nR n:man (a,t) 1 (Reg nR):dim1 (Just a) t (Reg nR) ++ [Cpy (AP t (Just 16) (Just a)) (AP xsR (Just 16) l) (Reg nϵR), WrF (AP t (Just (IB IAsl (Reg nR) 3 + 8)) (Just a)) (FReg xR)])
aeval (EApp _ (Builtin _ T) x) t | Just (ty, rnk) <- tRnk (eAnn x) = do
    a <- nextArr
    xR <- newITemp; xRd <- newITemp; td <- newITemp
    let dE = ConstI$8+8*rnk
    (l, plX) <- aeval x xR
    let sze = bT ty
    (dts, dss) <- plDim rnk (xR, l)
    (sts, sss) <- offByDim (Reg <$> dts)
    (std, ssd) <- offByDim (Reg <$> reverse dts)
    let nOut:sstrides = sts; (_:dstrides) = std
    ixs <- traverse (\_ -> newITemp) [1..rnk]
    loop <- threadM (zipWith doN ixs (Reg <$> dts)) [Cpy (xp (Reg <$> dstrides) (Reg <$> reverse ixs) (td, Just a)) (xp (Reg <$> sstrides) (Reg <$> ixs) (xRd, l)) (ConstI$sze`div`8)]
    modify (addMT a t)
    -- FIXME: data not of size 8 lol
    pure (Just a, plX ++ dss ++ sss ++ man (a,t) (1+rnk) (Reg nOut):Wr (AP t Nothing (Just a)) (ConstI rnk):zipWith (\tϵ o -> Wr (AP t (Just (ConstI$8*o)) (Just a)) (Reg tϵ)) (reverse dts) [1..] ++ ssd ++ MT xRd (Reg xR + dE):MT td (Reg t + dE):loop)
aeval (EApp _ (EApp _ (Builtin _ (Conv is)) f) x) t | Just iTy <- mAF (eAnn f) = do
    a <- nextArr
    xR <- newITemp; xRd <- newITemp; slopP <- newITemp; ret <- newFTemp; td <- newITemp
    (l, plX) <- aeval x xR
    let rnk = length is; rnk64 = fromIntegral rnk; dE = ConstI$8+8*rnk64
        nIr = fromIntegral$8+8*rnk+bT iTy*product is
        i64s = fromIntegral <$> is; slopStrides = tail (scanr (*) 1 i64s)
        dimEs = zipWith (-) (dimR rnk64 (xR, l)) (ConstI . fromIntegral <$> is)
    (dts, dss) <- unzip <$> traverse (\e -> do {dt <- newITemp; pure (dt, MT dt e)}) dimEs
    (sts, sss) <- offByDim (Reg <$> dts) -- strides
    let nOut:strides = sts
    ss <- writeRF f [slopP] ret
    ixs <- traverse (\_ -> newITemp) is
    preCopy <- stacopy (Reg <$> strides) (ConstI <$> slopStrides) (Reg <$> ixs) i64s (td, Just a) (xRd, l)
    loop <- threadM (zipWith doN ixs (Reg <$> dts)) (preCopy ++ ss ++ [WrF (xp (Reg <$> sts) (Reg <$> ixs) (td, Just a)) (FReg ret)])
    modify (addMT a t)
    pure (Just a, plX ++ Sa slopP (ConstI nIr) : dss ++ sss ++ man (a,t) (1+rnk64) (Reg nOut):Wr (AP t Nothing (Just a)) 1:zipWith (\o t' -> Wr (AP t (Just$ConstI (8*o)) (Just a)) (Reg t')) [1..] dts ++ MT xRd (Reg xR + dE):MT td (Reg t + dE):loop ++ [Pop (ConstI nIr)])
aeval (EApp _ (EApp _ (Builtin _ CatE) x) y) t | Just (ty, 1) <- tRnk (eAnn x) = do
    a <- nextArr
    xR <- newITemp; yR <- newITemp
    xnR <- newITemp; ynR <- newITemp; tn <- newITemp; mid <- newITemp
    let tyN=bT ty`div`8
    (lX, plX) <- aeval x xR; (lY, plY) <- aeval y yR
    modify (addMT a t)
    -- TODO: hardcoded to 8-byte types MT mid ...
    pure (Just a, plX ++ plY ++ MT xnR (gd1 lX xR):MT ynR (gd1 lY yR):MT tn (Reg xnR + Reg ynR):man (a,t) 1 (Reg tn):dim1 (Just a) t (Reg tn) ++ [Cpy (AP t (Just 16) (Just a)) (AP xR (Just 16) lX) (Reg xnR * ConstI tyN), MT mid (Reg t + IB IAsl (Reg xnR + 2) 3), Cpy (AP mid Nothing (Just a)) (AP yR (Just 16) lY) (Reg ynR * ConstI tyN)])
aeval (LLet _ (n, e') e) t | isArr (eAnn e') = do
    t' <- newITemp
    (l, ss) <- aeval e' t'
    modify (addAVar n (l, t'))
    second (ss ++) <$> aeval e t
aeval (EApp oTy (Builtin _ Init) x) t | f1 oTy = do
    a <- nextArr
    xR <- newITemp; nR <- newITemp
    (lX, plX) <- aeval x xR
    modify (addMT a t)
    let n=gd1 lX xR
    pure (Just a, plX ++ MT nR (n - 1):man (a,t) 1 (Reg nR):dim1 (Just a) t (Reg nR) ++ [Cpy (AP t (Just 16) (Just a)) (AP xR (Just 16) lX) (Reg nR)])
aeval (EApp oTy (Builtin _ Tail) x) t | f1 oTy = do
    a <- nextArr
    xR <- newITemp; nR <- newITemp
    (lX, plX) <- aeval x xR
    modify (addMT a t)
    let n=gd1 lX xR
    pure (Just a, plX ++ MT nR (n - 1):man (a,t) 1 (Reg nR):dim1 (Just a) t (Reg nR) ++ [Cpy (AP t (Just 16) (Just a)) (AP xR (Just 24) lX) (Reg nR)])
aeval (EApp _ (EApp _ (Builtin _ (Rank [(0, _)])) f) xs) t | isFF (eAnn f) = do
    a <- nextArr; xR <- newITemp; rnkR <- newITemp; szR <- newITemp; j <- newITemp
    i <- newITemp; x <- newFTemp; y <- newFTemp; xRd <- newITemp; tD <- newITemp; offsR <- newITemp
    (lX, plX) <- aeval xs xR
    ss <- writeRF f [x] y
    let step = MX x (FAt (AP xRd (Just (IB IAsl (Reg i) 3)) lX)):ss++[WrF (AP tD (Just (IB IAsl (Reg i) 3)) (Just a)) (FReg y)]
    loop <- doN i (Reg szR) step
    mSz <- doN j (Reg rnkR) [MT szR (Reg szR * EAt (AP t (Just (IB IAsl (Reg j) 3 + 8)) (Just a)))]
    modify (addMT a t)
    pure (Just a, plX ++ MT rnkR (EAt (AP xR Nothing lX)):MT szR 1:mSz++[Ma a t (IB IAsl (Reg rnkR + Reg szR) 3 + 8), Cpy (AP t Nothing (Just a)) (AP xR Nothing lX) (Reg rnkR + 1), MT offsR (IB IAsl (Reg rnkR) 3 + 8), MT xRd (Reg xR + Reg offsR), MT tD (Reg t + Reg offsR)] ++ loop)
aeval (EApp _ (EApp _ (Builtin _ (Rank [(cr, Just ixs)])) f) xs) t | Just (F, rnk) <- tRnk (eAnn xs), isAF (eAnn f) = do
    a <- nextArr
    xR <- newITemp
    (lX, plX) <- aeval xs xR
    modify (addMT a t)
    slopP <- newITemp; y <- newFTemp
    let ixsIs = IS.fromList ixs; allIx = [ if ix `IS.notMember` ixsIs then Right ix else Left ix | ix <- [1..fromIntegral rnk] ]
    oSz <- newITemp; slopSz <- newITemp
    (dts, dss) <- plDim rnk (xR, lX)
    (sts, sss) <- offByDim (Reg <$> dts)
    let _:sstrides = sts
    allts <- traverse (\i -> case i of {Right{} -> Right <$> newITemp; Left{} -> Left <$> newITemp}) allIx
    let complts = lefts allts
        allDims = zipWith (\ix dt -> case ix of {Right{} -> Right dt; Left{} -> Left dt}) allIx dts
        complDims = lefts allDims; oDims = rights allDims
        wrOSz = MT oSz 1:[ MT oSz (Reg oSz * Reg dϵ) | dϵ <- oDims ]
        wrSlopSz = MT slopSz 1:[ MT slopSz (Reg slopSz * Reg dϵ) | dϵ <- complDims ]
    (_, ss) <- writeF f [(Nothing, slopP)] y
    let ecArg = zipWith (\d tt -> case (d,tt) of (dϵ,Right{}) -> Right dϵ; (_,Left tϵ) -> Left (Reg tϵ)) dts allts
    xRd <- newITemp; slopPd <- newITemp
    place <- extrCell ecArg sstrides (xRd, lX) slopPd
    di <- newITemp
    let oRnk=rnk-fromIntegral cr; slopRnk=rnk-oRnk
    loop <- threadM (zipWith (\d tϵ s -> doN tϵ (Reg d) s) complDims complts) $ place ++ ss ++ [MX y (FAt (AP t (Just$IB IAsl (Reg di) 3 + ConstI (8+8*oRnk)) (Just a))), tick di]
    pure (Just a, plX ++ dss ++ wrOSz ++ man (a,t) oRnk (Reg oSz):Wr (AP t Nothing (Just a)) (ConstI oRnk):zipWith (\d i -> Wr (AP t (Just$ConstI (8+i)) (Just a)) (Reg d)) oDims [0..] ++ wrSlopSz ++ Sa slopP (Reg slopSz):Wr (AP slopP Nothing Nothing) (ConstI slopRnk):zipWith (\d i -> Wr (AP slopP (Just$ConstI (8+i)) Nothing) (Reg d)) complDims [0..] ++ sss ++ [MT xRd (Reg xR + ConstI (8+8*rnk)), MT slopPd (Reg slopP + ConstI (8+8*slopRnk))] ++ MT di 0:loop ++ [Pop (Reg slopSz)])
aeval (EApp _ (EApp _ (Builtin _ Rot) i) xs) t | let ty=eAnn xs in f1 ty||i1 ty = do
    a <- nextArr
    xR <- newITemp; iR <- newITemp; szR <- newITemp; iC <- newITemp
    plI <- eval i iR
    (lX, plX) <- aeval xs xR
    modify (addMT a t)
    -- TODO: edge cases: negative/wrap (just need modulo idk)
    pure (Just a, plX ++ plI ++ MT szR (gd1 lX xR):man (a,t) 1 (Reg szR):dim1 (Just a) t (Reg szR) ++ [MT iC (Reg szR - Reg iR), Cpy (AP t (Just 16) (Just a)) (AP xR (Just (IB IAsl (Reg iR) 3 + 16)) lX) (Reg iC), Cpy (AP t (Just (IB IAsl (Reg iC) 3 + 16)) (Just a)) (AP xR (Just 16) lX) (Reg iR)])
aeval (EApp _ (EApp _ (Builtin _ VMul) a) x) t | f1 (eAnn x) = do
    aL <- nextArr
    xR <- newITemp; aR <- newITemp; i <- newITemp; j <- newITemp; m <- newITemp; n <- newITemp; z <- newFTemp
    (lA, plA) <- aeval a aR
    (lX, plX) <- aeval x xR
    modify (addMT aL t)
    let aAddr=AP aR (Just (IB IAsl ((Reg n * Reg i) + Reg j) 3 + 24)) lA; xAddr=AP xR (Just (IB IAsl (Reg j) 3 + 16)) lX
    loop <- doN i (Reg m) =<< do
        loopϵ <- doN j (Reg n) [MX z (FReg z + (FAt aAddr * FAt xAddr))]
        pure $ MX z 0:loopϵ ++ [WrF (AP t (Just (IB IAsl (Reg i) 3 + 16)) (Just aL)) (FReg z)]
    pure (Just aL, plA ++ plX ++ MT m (EAt (AP aR (Just 8) lA)):man (aL,t) 1 (Reg m):dim1 (Just aL) t (Reg m)++MT n (EAt (AP aR (Just 16) lA)):loop)
aeval (EApp _ (EApp _ (Builtin _ Mul) a) b) t | Just (F, _) <- tRnk (eAnn a) = do
    aL <- nextArr
    aR <- newITemp; bR <- newITemp; i <- newITemp; j <- newITemp; k <- newITemp; m <- newITemp; n <- newITemp; o <- newITemp; z <- newFTemp
    (lA, plA) <- aeval a aR
    (lB, plB) <- aeval b bR
    modify (addMT aL t)
    let aAddr=AP aR (Just (IB IAsl (Reg n * Reg i + Reg k) 3 + 24)) lA;bAddr=AP bR (Just (IB IAsl (Reg k + Reg j * Reg n) 3 + 24)) lB
    loop <- doN i (Reg m) =<< doN j (Reg o) =<< do
        loopϵ <- doN k (Reg n) [MX z (FReg z + FAt aAddr * FAt bAddr)]
        pure $ MX z 0:loopϵ ++ [WrF (AP t (Just (IB IAsl (Reg i * Reg o + Reg j) 3 + 24)) (Just aL)) (FReg z)]
    pure (Just aL, plA ++ plB ++ MT m (gd1 lA aR):MT n (gd1 lB bR):MT o (EAt (AP bR (Just 16) lB)):Ma aL t (IB IAsl (Reg m * Reg o) 3 + 24):Wr (AP t Nothing (Just aL)) 2:Wr (AP t (Just 8) (Just aL)) (Reg m):Wr (AP t (Just 16) (Just aL)) (Reg o):loop)
aeval (EApp _ (EApp _ (EApp _ (Builtin _ Outer) op) xs) ys) t | isFFF (eAnn op) = do
    a <- nextArr
    x <- newFTemp; y <- newFTemp; z <- newFTemp
    xR <- newITemp; yR <- newITemp; szXR <- newITemp; szYR <- newITemp; i <- newITemp; j <- newITemp; k <- newITemp
    (lX, plX) <- aeval xs xR
    (lY, plY) <- aeval ys yR
    modify (addMT a t)
    ss <- writeRF op [x, y] z
    loop <- doN i (Reg szXR) =<< doN j (Reg szYR) (MX x (FAt (AP xR (Just (sib i)) lX)):MX y (FAt (AP yR (Just (sib j)) lY)):ss ++ [WrF (AP t (Just (sib1 k)) (Just a)) (FReg z), tick k])
    pure (Just a, plX ++ plY ++ MT szXR (gd1 lX xR):MT szYR (gd1 lY yR):Ma a t (IB IAsl (Reg szXR * Reg szYR) 3 + 24):Wr (AP t Nothing (Just a)) 2:Wr (AP t (Just 8) (Just a)) (Reg szXR):Wr (AP t (Just 16) (Just a)) (Reg szYR):MT k 0:loop)
aeval (EApp _ (EApp _ (Builtin _ Map) f) xs) t | Just (F, F) <- mA1A1 (eAnn f) = do
    a <- nextArr
    slopP <- newITemp; y <- newITemp; y0 <- newITemp
    xR <- newITemp; szXR <- newITemp; szSlopR <- newITemp; szYR <- newITemp; i <- newITemp
    (lX, plX) <- aeval xs xR
    modify (addMT a t)
    (lY0, ss0) <- writeF f [(Nothing, slopP)] y0
    (lY, ss) <- writeF f [(Nothing, slopP)] y -- writeF ... f/ss is "linear" it can only be placed once b/c assembler needs unique labels
    loop <- doN i (Reg szXR) $ Cpy (AP slopP (Just 16) Nothing) (AP xR (Just (IB IAsl (Reg i * Reg szSlopR) 3 + 24)) lX) (Reg szSlopR):ss++[Cpy (AP t (Just (IB IAsl (Reg i * Reg szYR) 3 + 24)) (Just a)) (AP y (Just 16) lY) (Reg szYR)]
    pure (Just a, plX ++ MT szXR (gd1 lX xR):MT szSlopR (EAt (AP xR (Just 16) lX)):Sa slopP (IB IAsl (Reg szSlopR) 3 + 16):dim1 Nothing slopP (Reg szSlopR) ++ Cpy (AP slopP (Just 16) Nothing) (AP xR (Just 24) lX) (Reg szSlopR):ss0 ++ [MT szYR (gd1 lY0 y0), Ma a t (IB IAsl (Reg szXR * Reg szYR) 3 + 24), Wr (AP t Nothing (Just a)) 2, Wr (AP t (Just 8) (Just a)) (Reg szXR), Wr (AP t (Just 16) (Just a)) (Reg szYR)] ++ loop ++ [Pop (IB IAsl (Reg szSlopR) 3 + 16)])
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
eval (LLet _ (n, e') e) t | (Arrow F (Arrow F F)) <- eAnn e' = do
    l <- newLabel; endL <- newLabel; arg0 <- newFTemp; arg1 <- newFTemp; ret <- newFTemp
    ss <- writeRF e' [arg0, arg1] ret
    modify (addFVar n (l, [(Nothing, arg0), (Nothing, arg1)], (Nothing, ret)))
    (++ (J endL:L l:ss++[IR.R l, L endL])) <$> eval e t
eval (EApp _ (EApp _ (EApp _ (Builtin _ FoldS) op) seed) (EApp _ (EApp _ (EApp _ (Builtin _ IRange) start) end) (ILit _ j))) acc = do
    i <- newITemp
    endR <- newITemp
    l <- newLabel; endL <- newLabel
    putStart <- eval start i; putAcc <- eval seed acc; irEnd <- eval end endR
    step <- writeRF op [acc, i] acc
    pure $ putStart ++ putAcc ++ irEnd ++ (L l:MJ (IRel IGt (Reg i) (Reg endR)) endL:step) ++ [MT i (Reg i + ConstI (asI j)), J l, L endL]
eval (EApp _ (EApp _ (EApp _ (Builtin _ FoldS) op) seed) (EApp _ (EApp _ (EApp _ (Builtin _ IRange) start) end) incr)) acc = do
    i <- newITemp
    endR <- newITemp
    incrR <- newITemp
    l <- newLabel; endL <- newLabel
    putStart <- eval start i; putAcc <- eval seed acc; irEnd <- eval end endR
    irIncr <- eval incr incrR
    step <- writeRF op [acc, i] acc
    -- TODO: is this shortest loop?
    pure $ putStart ++ putAcc ++ irEnd ++ irIncr ++ (L l:MJ (IRel IGt (Reg i) (Reg endR)) endL:step) ++ [MT i (Reg i + Reg incrR), J l, L endL]
-- TODO: start, end, nSteps a literal
eval (EApp _ (EApp _ (EApp _ (Builtin _ FoldS) op) seed) (EApp _ (EApp _ (EApp _ (Builtin _ FRange) (ILit _ start)) (ILit _ end)) (ILit _ nSteps))) acc = do
    i <- newITemp
    l <- newLabel; endL <- newLabel
    let incr = fromIntegral (end-start+1)/fromIntegral nSteps
    xR <- newFTemp
    putAcc <- eval seed acc
    step <- writeRF op [acc, xR] acc
    pure $ putAcc ++ (MX xR (ConstF $ fromIntegral start):MT i 1:L l:MJ (IRel IGt (Reg i) (ConstI $ asI nSteps)) endL:step) ++ [tick i, MX xR (FReg xR + ConstF incr), J l, L endL]
eval (EApp _ (EApp _ (EApp _ (Builtin _ FoldS) op) seed) (EApp _ (EApp _ (EApp _ (Builtin _ FRange) start) end) nSteps@(EApp _ (Builtin _ Floor) nStepsF))) acc = do
    i <- newITemp
    startR <- newFTemp
    incrR <- newFTemp
    xR <- newFTemp
    endI <- newITemp
    l <- newLabel; endL <- newLabel
    putStart <- eval start startR; putAcc <- eval seed acc; putIEnd <- eval nSteps endI
    putIncr <- eval ((end `eMinus` start) `eDiv` (nStepsF `eMinus` FLit F 1)) incrR
    -- step the accumulating value
    step <- writeRF op [acc, xR] acc
    pure $ putStart ++ (MX xR (FReg startR):putIEnd) ++ putIncr ++ putAcc ++ (MT i 1:L l:MJ (IRel IGt (Reg i) (Reg endI)) endL:step) ++ [tick i, MX xR (FReg xR + FReg incrR), J l, L endL]
eval (EApp _ (EApp _ (EApp _ (Builtin _ FoldS) op) seed) (EApp _ (EApp _ (EApp _ (Builtin _ FRange) start) end) nSteps)) acc = do
    i <- newITemp
    startR <- newFTemp
    incrR <- newFTemp
    xR <- newFTemp
    endI <- newITemp
    l <- newLabel; endL <- newLabel
    putStart <- eval start startR
    putAcc <- eval seed acc
    putIEnd <- eval nSteps endI
    putIncr <- eval ((end `eMinus` start) `eDiv` (EApp F (Builtin (Arrow I F) ItoF) nSteps `eMinus` FLit F 1)) incrR
    -- step the accumulating value
    step <- writeRF op [acc, xR] acc
    pure $ putStart ++ (MX xR (FReg startR):putIEnd) ++ putIncr ++ putAcc ++ (MT i 1:L l:MJ (IRel IGt (Reg i) (Reg endI)) endL:step) ++ [tick i, MX xR (FReg xR + FReg incrR), J l, L endL]
eval (EApp _ (EApp _ (Builtin (Arrow I _) Plus) e0) e1) t = do
    t0 <- newITemp; t1 <- newITemp
    pl0 <- eval e0 t0; pl1 <- eval e1 t1
    pure $ pl0 ++ pl1 ++ [MT t (Reg t0 + Reg t1)]
eval (EApp _ (EApp _ (Builtin _ Plus) (Var F x)) (EApp _ (EApp _ (Builtin _ Times) e0) e1)) t = do
    st <- gets vars
    t0 <- newFTemp; t1 <- newFTemp
    pl0 <- eval e0 t0; pl1 <- eval e1 t1
    pure $ pl0 ++ pl1 ++ [MX t (FReg (getT st x) + (FReg t0 * FReg t1))]
eval (EApp _ (EApp _ (Builtin (Arrow F _) Plus) e0) e1) t = do
    t0 <- newFTemp; t1 <- newFTemp
    pl0 <- eval e0 t0; pl1 <- eval e1 t1
    pure $ pl0 ++ pl1 ++ [MX t (FReg t0 + FReg t1)]
eval (EApp _ (EApp _ (Builtin (Arrow I _) Times) e0) e1) t = do
    t0 <- newITemp; t1 <- newITemp
    pl0 <- eval e0 t0; pl1 <- eval e1 t1
    pure $ pl0 ++ pl1 ++ [MT t (Reg t0 * Reg t1)]
eval (EApp _ (EApp _ (Builtin _ Mod) e0) e1) t = do
    t0 <- newITemp; t1 <- newITemp
    pl0 <- eval e0 t0; pl1 <- eval e1 t1
    pure $ pl0 ++ pl1 ++ [MT t (IB IRem (Reg t0) (Reg t1))]
eval (EApp _ (EApp _ (Builtin _ Minus) e) (ILit F i)) t = do
    tϵ <- newFTemp
    pl <- eval e tϵ
    pure $ pl ++ [MX t (FReg tϵ - ConstF (fromIntegral i))]
eval (EApp _ (EApp _ (Builtin _ Minus) e) (ILit I i)) t = do
    tϵ <- newITemp
    pl <- eval e tϵ
    pure $ pl ++ [MT t (Reg tϵ - ConstI (asI i))]
eval (EApp _ (EApp _ (Builtin (Arrow I _) Minus) e0) e1) t = do
    t0 <- newITemp; t1 <- newITemp
    pl0 <- eval e0 t0; pl1 <- eval e1 t1
    pure $ pl0 ++ pl1 ++ [MT t (Reg t0 - Reg t1)]
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
eval (EApp F (EApp _ (Builtin _ Max) e0) e1) t = do
    t0 <- newFTemp; t1 <- newFTemp
    pl0 <- eval e0 t0; pl1 <- eval e1 t1
    pure $ pl0 ++ pl1 ++ [MX t (FB FMax (FReg t0) (FReg t1))]
eval (EApp _ (EApp _ (Builtin _ A.IDiv) e0) e1) t = do
    t0 <- newITemp; t1 <- newITemp
    pl0 <- eval e0 t0; pl1 <- eval e1 t1
    pure $ pl0 ++ pl1 ++ [MT t (IB IR.IDiv (Reg t0) (Reg t1))]
eval (EApp F (EApp _ (Builtin _ Times) e0) e1) t = do
    t0 <- newFTemp; t1 <- newFTemp
    pl0 <- eval e0 t0; pl1 <- eval e1 t1
    pure $ pl0 ++ pl1 ++ [MX t (FReg t0 * FReg t1)]
eval (EApp F (EApp _ (Builtin _ Minus) e0) e1) t = do
    t0 <- newFTemp; t1 <- newFTemp
    pl0 <- eval e0 t0; pl1 <- eval e1 t1
    pure $ pl0 ++ pl1 ++ [MX t (FReg t0 - FReg t1)]
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
    pure $ plR ++ plX ++ [MX t 1, MT i (Reg nR), L l, MJ (IRel IEq (Reg i) 0) endL, MX t (FReg t * FReg xR), MT i (Reg i - 1), J l, L endL]
eval (EApp _ (EApp _ (Builtin _ IntExp) x) n) t = do
    i <- newITemp
    nR <- newITemp
    xR <- newITemp
    plR <- eval n nR; plX <- eval x xR
    l <- newLabel; endL <- newLabel
    pure $ plR ++ plX ++ [MT t 1, MT i (Reg nR), L l, MJ (IRel IEq (Reg i) 0) endL, MT t (Reg t * Reg xR), MT i (Reg i - 1), J l, L endL]
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
    pure $ plX ++ [MX t (negate (FReg fR))]
eval (FLit _ x) t = pure [MX t (ConstF x)]
eval (EApp _ (Builtin _ Sqrt) (FLit _ x)) t =
    pure [MX t (ConstF (sqrt x))]
eval (EApp _ (Builtin _ Sqrt) (ILit F x)) t =
    pure [MX t (ConstF (sqrt $ realToFrac x :: Double))]
eval (EApp _ (Builtin _ Sqrt) e) t = do
    eR <- newFTemp
    plE <- eval e eR
    pure $ plE ++ [MX t (FU FSqrt (FReg eR))]
eval (EApp _ (EApp _ (EApp _ (Builtin _ FoldS) op) seed) e) acc | f1 (eAnn e) = do
    x <- newFTemp
    eR <- newITemp
    szR <- newITemp
    i <- newITemp
    (mI, plE) <- aeval e eR
    putAcc <- eval seed acc
    stepR <- writeRF op [acc, x] acc
    let step = MX x (FAt (AP eR (Just$sib i) mI)):stepR
    loop <- doN i (Reg szR) step
    pure $ plE ++ putAcc ++ MT szR (EAt (AP eR (Just 8) mI)):loop
eval (EApp _ (EApp _ (Builtin _ Fold) op) e) acc | f1 (eAnn e) = do
    x <- newFTemp
    arrP <- newITemp
    szR <- newITemp
    i <- newITemp
    (l, plE) <- aeval e arrP
    ss <- writeRF op [acc, x] acc
    let step = MX x (FAt (AP arrP (Just$sib i) l)):ss
    loop <- fN1 i (Reg szR) step
    let sz = EAt (AP arrP (Just 8) l)
    pure (plE ++ MT szR sz:MX acc (FAt (AP arrP (Just 16) l)):loop)
eval (EApp _ (EApp _ (EApp _ (Builtin _ FoldA) op) seed) e) acc | isFFF (eAnn op) = do
    x <- newFTemp
    eR <- newITemp; datR <- newITemp
    szR <- newITemp; rnkR <- newITemp
    i <- newITemp; j <- newITemp
    (mI, plE) <- aeval e eR
    putAcc <- eval seed acc
    stepR <- writeRF op [acc, x] acc
    let stepSz = MT szR (Reg szR * EAt (AP eR (Just (sd j + 8)) mI))
        step = MX x (FAt (AP datR (Just (sd i + 8)) mI)):stepR
    szLoop <- doN j (Reg rnkR) [stepSz]
    loop <- doN i (Reg szR) step
    pure $ plE ++ putAcc ++ MT rnkR (EAt (AP eR Nothing mI)):MT datR (Reg eR + IB IAsl (Reg rnkR) 3):MT szR 1:szLoop ++ loop
eval (EApp _ (EApp _ (EApp _ (Builtin _ Foldl) op) seed) e) acc | f1 (eAnn e) = do
    x <- newFTemp
    eR <- newITemp
    i <- newITemp
    (mI, plE) <- aeval e eR
    putAcc <- eval seed acc
    l <- newLabel; endL <- newLabel
    stepR <- writeRF op [acc, x] acc
    let step = MX x (FAt (AP eR (Just$sib i) mI)):stepR ++ [MT i (Reg i - 1)]
    -- GHC uses 'length' but our szR needs to be one less
    pure $ plE ++ putAcc ++ MT i (EAt (AP eR (Just 8) mI)):L l:MJ (IRel ILt (Reg i) 0) endL:step++[J l, L endL]
eval (EApp _ (EApp _ (EApp _ (Builtin _ FoldS) op) seed) e) acc | i1 (eAnn e) = do
    x <- newITemp
    arrR <- newITemp
    eR <- newITemp
    szR <- newITemp
    i <- newITemp
    (mI, plE) <- aeval e eR
    putAcc <- eval seed acc
    stepR <- writeRF op [acc, x] acc
    let step = MT x (EAt (AP arrR Nothing mI)):stepR ++ [MT arrR (Reg arrR + 8)]
    loop <- doN i (Reg szR) step
    -- GHC uses 'length' but our szR needs to be one less
    pure $ plE ++ putAcc ++ MT szR (EAt (AP eR (Just 8) mI)):MT arrR (Reg eR + 16):loop
eval (Id F (FoldOfZip zop op [p])) acc | f1 (eAnn p) = do
    x <- newFTemp
    pR <- newITemp
    szR <- newITemp
    i <- newITemp
    (iP, plP) <- aeval p pR
    ss <- writeRF op [acc, x] acc
    let step = MX x (FAt (AP pR (Just$sib i) iP)):ss
    loop <- fN1 i (Reg szR) step
    sseed <- writeRF zop [x] acc
    pure $ plP ++ MT szR (gd1 iP pR):MX x (FAt (AP pR (Just 16) iP)):sseed ++ loop
eval (Id F (FoldOfZip zop op [p])) acc | i1 (eAnn p) = do
    x <- newITemp
    pR <- newITemp
    szR <- newITemp
    i <- newITemp
    (iP, plP) <- aeval p pR
    ss <- writeRF op [acc, x] acc
    let step = MT x (EAt (AP pR (Just$sib i) iP)):ss
    loop <- fN1 i (Reg szR) step
    let x0 = EAt (AP pR (Just 16) iP)
    sseed <- writeRF zop [x] acc
    pure $ plP ++ MT szR (gd1 iP pR):MT x x0:sseed ++ loop
eval (Id F (FoldSOfZip seed op [p, q])) acc | f1 (eAnn p) && f1 (eAnn q) = do
    x <- newFTemp; y <- newFTemp
    pR <- newITemp; qR <- newITemp
    szR <- newITemp
    i <- newITemp
    (iP, plP) <- aeval p pR; (iQ, plQ) <- aeval q qR
    putAcc <- eval seed acc
    stepR <- writeRF op [acc, x, y] acc
    let step = MX x (FAt (AP pR (Just$sib i) iP)):MX y (FAt (AP qR (Just$sib i) iQ)):stepR
    loop <- doN i (Reg szR) step
    -- FIXME: this assumes the arrays are the same size
    pure $ plP ++ plQ ++ putAcc ++ MT szR (EAt (AP pR (Just 8) iP)):loop
eval (Id F (FoldOfZip zop op [p, q])) acc | f1 (eAnn p) && f1 (eAnn q) = do
    x <- newFTemp; y <- newFTemp
    pR <- newITemp; qR <- newITemp
    szR <- newITemp
    i <- newITemp
    (iP, plP) <- aeval p pR; (iQ, plQ) <- aeval q qR
    ss <- writeRF op [acc, x, y] acc
    let step = MX x (FAt (AP pR (Just$sib i) iP)):MX y (FAt (AP qR (Just$sib i) iQ)):ss
    loop <- fN1 i (Reg szR) step
    sseed <- writeRF zop [x, y] acc
    pure $ plP ++ plQ ++ MX x (FAt (AP pR (Just 16) iP)):MX y (FAt (AP qR (Just 16) iQ)):sseed ++ MT szR (EAt (AP pR (Just 8) iP)):loop
eval (Id F (FoldOfZip zop op [p, q])) acc | f1 (eAnn p) && i1 (eAnn q) = do
    x <- newFTemp; y <- newITemp
    pR <- newITemp; qR <- newITemp
    szR <- newITemp
    i <- newITemp
    (iP, plP) <- aeval p pR; (iQ, plQ) <- aeval q qR
    ss <- writeRF op [acc, x, y] acc
    let step = MX x (FAt (AP pR (Just$sib i) iP)):MT y (EAt (AP qR (Just$sib i) iQ)):ss
    loop <- fN1 i (Reg szR) step
    sseed <- writeRF zop [x, y] acc
    pure $ plP ++ plQ ++ MT szR (EAt (AP pR (Just 8) iP)):MX x (FAt (AP pR (Just 16) iP)):MT y (EAt (AP qR (Just 16) iQ)):sseed ++ loop
eval (Id F (FoldOfZip zop op [EApp _ (EApp _ (EApp _ (Builtin _ IRange) start) _) incr, Id ty (AShLit [_] qs)])) acc | f1 ty = do
    x <- newITemp; y <- newFTemp
    i <- newITemp
    plX <- eval start x; plY <- eval (head qs) y; plI <- eval incr i
    sseed <- writeRF zop [x, y] acc
    step <- writeRF op [acc, x, y] acc
    steps <- foldMapA (\q -> do {plYϵ <- eval q y; pure $ [MT x (Reg x + Reg i)] ++ plYϵ ++ step}) (tail qs)
    pure $ plX ++ plY ++ sseed ++ plI ++ steps
eval (Id F (FoldOfZip zop op [p, q])) acc | i1 (eAnn p) && f1 (eAnn q) = do
    x <- newITemp; y <- newFTemp
    pR <- newITemp; qR <- newITemp
    szR <- newITemp
    i <- newITemp
    (iP, plP) <- aeval p pR; (iQ, plQ) <- aeval q qR
    ss <- writeRF op [acc, x, y] acc
    let step = MT x (EAt (AP pR (Just$sib i) iP)):MX y (FAt (AP qR (Just$sib i) iQ)):ss
    loop <- fN1 i (Reg szR) step
    sseed <- writeRF zop [x, y] acc
    pure $ plP ++ plQ ++ MT szR (EAt (AP pR (Just 8) iP)):MT x (EAt (AP pR (Just 16) iP)):MX y (FAt (AP qR (Just 16) iQ)):sseed ++ loop
eval (Id F (FoldOfZip zop op [p, q])) acc | f1 (eAnn p) && f1 (eAnn q) = do
    x <- newFTemp; y <- newFTemp
    pR <- newITemp; qR <- newITemp
    szR <- newITemp
    i <- newITemp
    (iP, plP) <- aeval p pR; (iQ, plQ) <- aeval q qR
    ss <- writeRF op [acc, x, y] acc
    let step = MX x (FAt (AP pR (Just$sib i) iP)):MX y (FAt (AP qR (Just$sib i) iQ)):ss
    loop <- fN1 i (Reg szR) step
    sseed <- writeRF zop [x, y] acc
    pure $ plP ++ plQ ++ MT szR (EAt (AP pR (Just 8) iP)):MX x (FAt (AP pR (Just 16) iP)):MX y (FAt (AP qR (Just 16) iQ)):sseed ++ loop
eval (Id F (FoldSOfZip seed op [EApp _ (EApp _ (EApp _ (Builtin _ IRange) start) _) incr, Id ty (AShLit [_] qs)])) acc | f1 ty = do
    x <- newITemp
    i <- newITemp
    y <- newFTemp
    plX <- eval start x; plI <- eval incr i; putAcc <- eval seed acc
    stepR <- writeRF op [acc, x, y] acc
    steps <- foldMapA (\q -> do { plY <- eval q y ; pure $ plY ++ stepR ++ [MT x (Reg x + Reg i)] }) qs -- FIXME: doesn't check arrays are same size
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
eval (EApp _ (Builtin _ Cos) e) t = do
    f <- newFTemp
    plE <- eval e f
    pure $ plE ++ [MX t (FU FCos (FReg f))]
eval (EApp _ (Builtin _ Size) e) t | unDim (eAnn e) = do
    r <- newITemp
    (mI, plE) <- aeval e r
    pure $ plE ++ [MT t (EAt (AP r (Just 8) mI))]
eval (EApp _ (Builtin _ Size) e) t = do
    r <- newITemp
    (mI, plE) <- aeval e r
    rnkR <- newITemp
    l <- newLabel; endL <- newLabel
    i <- newITemp
    pure $ plE ++ [MT rnkR (EAt (AP r Nothing mI)), MT i 8, MT t (EAt (AP r (Just 8) mI)), L l, MJ (IRel IGt (Reg i) (Reg rnkR)) endL, MT i (Reg i + 8), MT t (Reg t * EAt (AP r (Just (Reg i)) mI)),J l, L endL]
eval (EApp I (EApp _ (Builtin _ Max) e0) e1) t = do
    e0R <- newITemp; e1R <- newITemp
    plE0 <- eval e0 e0R; plE1 <- eval e1 e1R
    pure $ plE0 ++ plE1 ++ [MT t (Reg e1R), Cmov (IRel IGt (Reg e0R) (Reg e1R)) t (Reg e0R)]
eval (Cond _ (EApp _ (EApp _ (Builtin (Arrow F _) Gte) c0) c1) e0 e1) t = do
    c0R <- newFTemp; c1R <- newFTemp
    plC0 <- eval c0 c0R; plC1 <- eval c1 c1R
    plE0 <- eval e0 t; plE1 <- eval e1 t
    l <- newLabel; nextL <- newLabel
    pure $ plC0 ++ plC1 ++ MJ (FRel FGeq (FReg c0R) (FReg c1R)) l:plE1 ++ J nextL:L l:plE0 ++ [L nextL]
eval (Cond _ (EApp _ (EApp _ (Builtin (Arrow I _) Gt) c0) c1) e0 e1) t = do
    c0R <- newITemp; c1R <- newITemp
    plC0 <- eval c0 c0R; plC1 <- eval c1 c1R
    plE0 <- eval e0 t; plE1 <- eval e1 t
    l <- newLabel; nextL <- newLabel
    pure $ plC0 ++ plC1 ++ MJ (IRel IGeq (Reg c0R) (Reg c1R)) l:plE1 ++ J nextL:L l:plE0 ++ [L nextL]
eval (Cond _ (EApp _ (EApp _ (Builtin (Arrow I _) Eq) c0) c1) e0 e1) t = do
    c0R <- newITemp; c1R <- newITemp
    plC0 <- eval c0 c0R; plC1 <- eval c1 c1R
    plE0 <- eval e0 t; plE1 <- eval e1 t
    l <- newLabel; nextL <- newLabel
    pure $ plC0 ++ plC1 ++ MJ (IRel IEq (Reg c0R) (Reg c1R)) l:plE1 ++ J nextL:L l:plE0 ++ [L nextL]
eval (Cond _ (EApp _ (EApp _ (Builtin (Arrow I _) Neq) c0) c1) e0 e1) t = do
    c0R <- newITemp; c1R <- newITemp
    plC0 <- eval c0 c0R; plC1 <- eval c1 c1R
    plE0 <- eval e0 t; plE1 <- eval e1 t
    l <- newLabel; nextL <- newLabel
    pure $ plC0 ++ plC1 ++ MJ (IRel INeq (Reg c0R) (Reg c1R)) l:plE1 ++ J nextL:L l:plE0 ++ [L nextL]
eval (Cond _ (EApp _ (EApp _ (Builtin (Arrow I _) Lt) c0) c1) e0 e1) t = do
    c0R <- newITemp; c1R <- newITemp
    plC0 <- eval c0 c0R; plC1 <- eval c1 c1R
    plE0 <- eval e0 t; plE1 <- eval e1 t
    l <- newLabel; nextL <- newLabel
    pure $ plC0 ++ plC1 ++ MJ (IRel ILt (Reg c0R) (Reg c1R)) l:plE1 ++ J nextL:L l:plE0 ++ [L nextL]
eval (Cond _ (EApp _ (EApp _ (Builtin (Arrow I _) Lte) c0) c1) e0 e1) t = do
    c0R <- newITemp; c1R <- newITemp
    plC0 <- eval c0 c0R; plC1 <- eval c1 c1R
    plE0 <- eval e0 t; plE1 <- eval e1 t
    l <- newLabel; nextL <- newLabel
    pure $ plC0 ++ plC1 ++ MJ (IRel ILeq (Reg c0R) (Reg c1R)) l:plE1 ++ J nextL:L l:plE0 ++ [L nextL]
eval (Cond _ (EApp _ (Builtin _ Odd) c) e0 e1) t = do
    cR <- newITemp; plC <- eval c cR
    plE0 <- eval e0 t; plE1 <- eval e1 t
    l <- newLabel; nextL <- newLabel
    pure $ plC ++ MJ (IU IOdd (Reg cR)) l:plE1 ++ J nextL:L l:plE0 ++ [L nextL]
eval (Cond _ (EApp _ (Builtin _ Even) c) e0 e1) t = do
    cR <- newITemp; plC <- eval c cR
    plE0 <- eval e0 t; plE1 <- eval e1 t
    l <- newLabel; nextL <- newLabel
    pure $ plC ++ MJ (IU IEven (Reg cR)) l:plE1 ++ J nextL:L l:plE0 ++ [L nextL]
eval (EApp F (Builtin _ Head) arr) t = do
    r <- newITemp
    (mL, plArr) <- aeval arr r
    -- rank 1
    pure $ plArr ++ [MX t (FAt (AP r (Just 16) mL))]
eval (EApp I (Builtin _ Head) arr) t = do
    r <- newITemp
    (mL, plArr) <- aeval arr r
    -- rank 1
    pure $ plArr ++ [MT t (EAt (AP r (Just 16) mL))]
eval (EApp I (Builtin _ Last) arr) t = do
    r <- newITemp
    (l, plArr) <- aeval arr r
    pure $ plArr ++ [MT t (EAt (AP r (Just (IB IAsl (EAt (AP r (Just 8) l)) 3 + 8)) l))]
eval (EApp F (Builtin _ Last) arr) t = do
    r <- newITemp
    (l, plArr) <- aeval arr r
    pure $ plArr ++ [MX t (FAt (AP r (Just (IB IAsl (EAt (AP r (Just 8) l)) 3 + 8)) l))]
eval (EApp ty@P{} (Builtin _ Last) arr) t = do
    r <- newITemp
    (l, plArr) <- aeval arr r
    pure $ plArr ++ [Cpy (AP t Nothing Nothing) (AP r (Just (((gd1 l r-1)*ConstI (bT ty))+16)) l) (ConstI$bT ty `div` 8)]
eval (Tup _ es) t = do
    let szs = szT (eAnn<$>es)
    pls <- zipWithM (\e sz -> case eAnn e of {F -> do {fr <- newFTemp; p <- eval e fr; pure$p++[WrF (AP t (Just$ConstI sz) Nothing) (FReg fr)]};I -> do {r <- newITemp; p <- eval e r; pure$p++[Wr (AP t (Just$ConstI sz) Nothing) (Reg r)]}; Arr{} -> do {r <- newITemp; (_, p) <- aeval e r; pure$p++[Wr (AP t (Just$ConstI sz) Nothing) (Reg r)]}}) es szs
    pure$concat pls
eval (EApp F (Builtin _ (TAt n)) e@Var{}) t = do
    let (P tys) = eAnn e; szs = szT tys
    r <- newITemp; pl <- eval e r
    pure $ pl ++ [MX t (FAt (AP r (Just$ConstI (szs!!(n-1))) Nothing))]
eval (EApp F (Builtin _ (TAt n)) e) t = do
    let (P tys) = eAnn e; szs = szT tys; sz = fromIntegral (last szs)
    r <- newITemp; pl <- eval e r
    pure $ Sa r (ConstI sz):pl ++ [MX t (FAt (AP r (Just$ConstI (szs!!(n-1))) Nothing)), Pop (ConstI sz)]
eval (EApp I (Builtin _ (TAt n)) e@Var{}) t = do
    let (P tys) = eAnn e; szs = szT tys
    r <- newITemp; pl <- eval e r
    pure $ pl ++ [MT t (EAt (AP r (Just$ConstI (szs!!(n-1))) Nothing))]
eval (EApp I (Builtin _ (TAt n)) e) t = do
    let (P tys) = eAnn e; szs = szT tys; sz = fromIntegral (last szs)
    r <- newITemp; pl <- eval e r
    pure $ Sa r (ConstI sz):pl ++ [MT t (EAt (AP r (Just$ConstI (szs!!(n-1))) Nothing)), Pop (ConstI sz)]
eval (EApp F (Var _ f) e) t | isF (eAnn e) = do
    st <- gets fvars
    let (l, [(Nothing, arg)], (Nothing, ret)) = getT st f
    plE <- eval e arg
    retL <- newLabel
    pure $ plE ++ [C l, L retL, MX t (FReg ret)]
eval (EApp F (EApp _ (Var _ f) e0) e1) t | isF (eAnn e0) && isF (eAnn e1) = do
    st <- gets fvars
    let (l, [(Nothing, arg0), (Nothing, arg1)], (Nothing, ret)) = getT st f
    plE0 <- eval e0 arg0; plE1 <- eval e1 arg1
    retL <- newLabel
    pure $ plE0 ++ plE1 ++ [C l, L retL, MX t (FReg ret)]
eval (EApp F (EApp _ (Builtin _ A1) e) i) t = do
    eR <- newITemp; iR <- newITemp
    (lE, plE) <- aeval e eR; plI <- eval i iR
    pure $ plE ++ plI ++ [MX t (FAt (AP eR (Just (IB IAsl (Reg iR) 3+16)) lE))]
eval (EApp I (EApp _ (Builtin _ A.R) e0) e1) t = do
    e0R <- newITemp; e1R <- newITemp
    plE0 <- eval e0 e0R; plE1 <- eval e1 e1R
    pure $ plE0 ++ plE1 ++ [IRnd t, MT t (IB IRem (Reg t) (Reg e1R - Reg e0R) - Reg e0R)]
eval (EApp F (Builtin _ Abs) e) t = do
    plE <- eval e t
    l <- newLabel
    pure $ plE ++ [MJ (FRel FGeq (FReg t) 0) l, MX t (negate (FReg t)), L l]
eval (EApp I (Builtin _ Abs) e) t = do
    plE <- eval e t
    l <- newLabel
    pure $ plE ++ [MJ (IRel IGeq (Reg t) 0) l, MT t (negate $ Reg t), L l]
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
