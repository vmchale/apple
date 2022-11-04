{-# LANGUAGE TupleSections #-}

module IR.Trans ( writeC
                ) where

import           A
import           Control.Monad.State.Strict (State, gets, modify, runState)
import           Data.Foldable              (fold)
import           Data.Functor               (($>))
import           Data.Int                   (Int64)
import qualified Data.IntMap                as IM
import           IR
import           Name
import           U

data IRSt = IRSt { labels :: [Label]
                 , temps  :: [Int]
                 , arrs   :: [Int]
                 , vars   :: IM.IntMap Temp -- track vars so that (Var x) can be replaced at the site
                 , avars  :: IM.IntMap (Maybe Int, Temp)
                 , mts    :: IM.IntMap Temp
                 }

getT :: IM.IntMap b -> Name a -> b
getT st (Name _ (U i) _) = IM.findWithDefault (error "Internal error: variable not mapped to register.") i st

nextI :: IRM Int
nextI = do
    i <- gets (head.temps)
    modify (\(IRSt l (_:t) ar v a ts) -> IRSt l t ar v a ts) $> i

nextArr :: IRM Int
nextArr = do
    a <- gets (head.arrs)
    modify (\(IRSt l t (_:ar) v aϵ ts) -> IRSt l t ar v aϵ ts) $> a

newITemp :: IRM Temp
newITemp = ITemp <$> nextI

newFTemp :: IRM Temp
newFTemp = FTemp <$> nextI

newLabel :: IRM Label
newLabel = do
    i <- gets (head.labels)
    modify (\(IRSt l t ar v a ts) -> IRSt (tail l) t ar v a ts) $> i

addMT :: Int -> Temp -> IRSt -> IRSt
addMT i tϵ (IRSt l t ar v a ts) = IRSt l t ar v a (IM.insert i tϵ ts)

addVar :: Name a -> Temp -> IRSt -> IRSt
addVar (Name _ (U i) _) r (IRSt l t ar v a ts) = IRSt l t ar (IM.insert i r v) a ts

addAVar :: Name a -> (Maybe Int, Temp) -> IRSt -> IRSt
addAVar (Name _ (U i) _) r (IRSt l t ar v a ts) = IRSt l t ar v (IM.insert i r a) ts

type IRM = State IRSt

isF :: T a -> Bool
isF F = True
isF _ = False

isI :: T a -> Bool
isI I = True
isI _ = False

isArr Arr{} = True
isArr _     = False

writeC :: E (T ()) -> ([Stmt], WSt, IM.IntMap Temp)
writeC = π.flip runState (IRSt [0..] [0..] [0..] IM.empty IM.empty IM.empty) . writeCM where π (s, IRSt l t _ _ _ a) = (s, WSt l t, a)

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
    go (Lam _ x@(Name _ _ Arr{}) e) frs (r:rs) = do
        modify (addAVar x (Nothing, r))
        go e frs rs
    go Lam{} _ [] = error "Not enough registers!"
    go e _ _ | isF (eAnn e) = do {f <- newFTemp ; (++[MX FRet (FReg f)]) <$> eval e f} -- avoid clash with xmm0 (arg + ret)
             | isI (eAnn e) = eval e CRet
             | isArr (eAnn e) = do{(l,r) <- aeval e CRet; pure$case l of {Just m -> r++[RA m]}}
             | otherwise = error ("Unsupported return type: " ++ show (eAnn e))

writeRF :: E (T ()) -> [Temp] -> Temp -> IRM [Stmt]
writeRF e rs = fmap snd . writeF e ((Nothing,) <$> rs)

dim1 a t n = [Wr (AP t Nothing a) (ConstI 1), Wr (AP t (Just (ConstI 8)) a) n]

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
    let loop = MX f (FAt (AP arrP Nothing l)):ss++[WrF (AP t (Just (IB IPlus (IB IAsl (Reg iR) (ConstI 3)) (ConstI 16))) (Just a)) (FReg f), MT arrP (IB IPlus (Reg arrP) (ConstI 8)), MT iR (IB IPlus (Reg iR) (ConstI 1))]
    ll <- newLabel
    endL <- newLabel
    modify (addMT a t)
    pure (Just a, plE ++ (MT szR sz:Ma a t (IB IPlus (IB IAsl (Reg szR) (ConstI 3)) (ConstI 24)):MT iR (ConstI 0):Wr (AP t Nothing (Just a)) (ConstI 1):Wr (AP t (Just$ConstI 8) (Just a)) (Reg szR):MT arrP (IB IPlus (Reg arrP) (ConstI 16)):L ll:MJ (IRel IGt (Reg iR) (Reg szR)) endL:loop) ++ [J ll, L endL])
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
    let loop=MT n (EAt (AP arrP (Just$IB IAsl (Reg iR) (ConstI 3)) l)):Wr (AP t (Just (IB IPlus (IB IAsl (Reg iR) (ConstI 3)) (ConstI 16))) (Just a)) (Reg acc):ss++[MT iR (IB IPlus (Reg iR) (ConstI 1))]
    ll <- newLabel
    endL <- newLabel
    modify (addMT a t)
    pure (Just a, plE ++ plSeed ++ (MT szR (IB IPlus sz (ConstI 1)):Ma a t (IB IPlus (IB IAsl (Reg szR) (ConstI 3)) (ConstI 24)):MT iR (ConstI 0):Wr (AP t Nothing (Just a)) (ConstI 1):Wr (AP t (Just$ConstI 8) (Just a)) (Reg szR):MT arrP (IB IPlus (Reg arrP) (ConstI 16)):L ll:MJ (IRel IGt (Reg iR) (Reg szR)) endL:loop) ++ [J ll, L endL])
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
    let loop = MT m (EAt (AP arrP (Just$IB IAsl (Reg iR) (ConstI 3)) l)):ss++[Wr (AP t (Just (IB IPlus (IB IAsl (Reg iR) (ConstI 3)) (ConstI 16))) (Just a)) (Reg m), MT iR (IB IPlus (Reg iR) (ConstI 1))]
    ll <- newLabel
    endL <- newLabel
    modify (addMT a t)
    pure (Just a, plE ++ (MT szR sz:Ma a t (IB IPlus (IB IAsl (Reg szR) (ConstI 3)) (ConstI 24)):MT iR (ConstI 0):Wr (AP t Nothing (Just a)) (ConstI 1):Wr (AP t (Just$ConstI 8) (Just a)) (Reg szR):MT arrP (IB IPlus (Reg arrP) (ConstI 16)):L ll:MJ (IRel IGt (Reg iR) (Reg szR)) endL:loop) ++ [J ll, L endL])
aeval (EApp _ (EApp _ (EApp _ (Builtin _ IRange) start) end) (ILit _ 1)) t = do
    a <- nextArr
    n <- newITemp
    startR <- newITemp
    endR <- newITemp
    i <- newITemp
    putStart <- eval start startR
    putEnd <- eval end endR
    l <- newLabel
    endL <- newLabel
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
    l <- newLabel
    endL <- newLabel
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
    putStart <- eval start startR
    putN <- eval nSteps n
    l <- newLabel
    endL <- newLabel
    modify (addMT a t)
    putIncr <- eval (((end `eMinus` start) `ePlus` FLit F 1) `eDiv` EApp F (Builtin (Arrow I F) ItoF) nSteps) incrR
    let loop = [MJ (IRel IGt (Reg i) (Reg n)) endL, WrF (AP t (Just (IB IPlus (IB IAsl (Reg i) (ConstI 3)) (ConstI 16))) (Just a)) (FReg startR), MX startR (FB FPlus (FReg startR) (FReg incrR)), MT i (IB IPlus (Reg i) (ConstI 1))]
    pure (Just a, putStart ++ putIncr ++ putN ++ Ma a t (IB IPlus (IB IAsl (Reg n) (ConstI 3)) (ConstI 24)):dim1 (Just a) t (Reg n) ++ MT i (ConstI 0):L l:loop ++ [J l, L endL])
aeval (EApp oTy (EApp _ (Builtin _ Succ) op) arr) t | f1 (eAnn arr) && f1 oTy = do
    a <- nextArr
    arrP <- newITemp
    szR <- newITemp
    fArg0R <- newFTemp
    fArg1R <- newFTemp
    fRetR <- newFTemp
    (arrL, putX) <- aeval arr arrP
    -- f1 (skip rank)
    let sz = EAt (AP arrP (Just (ConstI 8)) arrL)
    i <- newITemp
    l <- newLabel
    endL <- newLabel
    ss <- writeRF op [fArg0R, fArg1R] fRetR
    let loop = MX fArg1R (FAt (AP arrP (Just (IB IPlus (IB IAsl (Reg i) (ConstI 3)) (ConstI 16))) arrL)):MX fArg0R (FAt (AP arrP (Just (IB IPlus (IB IAsl (Reg i) (ConstI 3)) (ConstI 24))) arrL)):ss++[WrF (AP t (Just (IB IPlus (IB IAsl (Reg i) (ConstI 3)) (ConstI 16))) (Just a)) (FReg fRetR)]
    pure (Just a, putX ++ MT szR sz:Ma a t (IB IPlus (IB IAsl (Reg szR) (ConstI 3)) (ConstI 16)):dim1 (Just a) t (IB IMinus (Reg szR) (ConstI 1)) ++ MT i (ConstI 0):L l:MJ (IRel IGeq (Reg i) (Reg szR)) endL:loop ++ [MT i (IB IPlus (Reg i) (ConstI 1)), J l, L endL])
aeval (EApp oTy (EApp _ (Builtin _ Succ) op) arr) t | i1 (eAnn arr) && i1 oTy = do
    a <- nextArr
    arrP <- newITemp
    szR <- newITemp
    arg0R <- newITemp
    arg1R <- newITemp
    retR <- newITemp
    (arrL, putX) <- aeval arr arrP
    -- f1 (skip rank)
    let sz = EAt (AP arrP (Just (ConstI 8)) arrL)
    i <- newITemp
    l <- newLabel
    endL <- newLabel
    ss <- writeRF op [arg0R, arg1R] retR
    let loop = MT arg1R (EAt (AP arrP (Just (IB IPlus (IB IAsl (Reg i) (ConstI 3)) (ConstI 16))) arrL)):MT arg0R (EAt (AP arrP (Just (IB IPlus (IB IAsl (Reg i) (ConstI 3)) (ConstI 24))) arrL)):ss++[Wr (AP t (Just (IB IPlus (IB IAsl (Reg i) (ConstI 3)) (ConstI 16))) (Just a)) (Reg retR)]
    pure (Just a, putX ++ MT szR sz:Ma a t (IB IPlus (IB IAsl (Reg szR) (ConstI 3)) (ConstI 16)):dim1 (Just a) t (IB IMinus (Reg szR) (ConstI 1)) ++ MT i (ConstI 0):L l:MJ (IRel IGeq (Reg i) (Reg szR)) endL:loop ++ [MT i (IB IPlus (Reg i) (ConstI 1)), J l, L endL])
aeval (EApp oTy (EApp _ (Builtin _ (DI n)) op) arr) t | f1 (eAnn arr) && f1 oTy = do
    a <- nextArr
    arrP <- newITemp
    slopP <- newITemp
    szR <- newITemp
    fR <- newFTemp
    (arrL, putX) <- aeval arr arrP
    -- cause f1 (skip rank)
    let sz = EAt (AP arrP (Just (ConstI 8)) arrL)
        nIr = ConstI (16+fromIntegral n*8)
    iR <- newITemp
    l <- newLabel
    endL <- newLabel
    -- return value is of type F
    (_, ss) <- writeF op [(Nothing, slopP)] fR
    let loop = Cpy (AP slopP (Just (ConstI 16)) Nothing) (AP arrP (Just (IB IPlus (IB IAsl (Reg iR) (ConstI 3)) (ConstI 16))) arrL) (ConstI $ fromIntegral n + 2):ss++[WrF (AP t (Just (IB IPlus (IB IAsl (Reg iR) (ConstI 3)) (ConstI 16))) arrL) (FReg fR)]
    pure (Just a, putX++MT szR sz:Ma a t (IB IPlus (IB IAsl (Reg szR) (ConstI 3)) (ConstI (24-8*fromIntegral n))):Wr (AP t Nothing (Just a)) (ConstI 1):Wr (AP t (Just (ConstI 8)) (Just a)) (IB IMinus (Reg szR) (ConstI $ fromIntegral n - 1)):Sa slopP nIr:Wr (AP slopP Nothing Nothing) (ConstI 1):Wr (AP slopP (Just (ConstI 8)) Nothing) (ConstI $ fromIntegral n):MT iR (ConstI 0):L l:MJ (IRel IGeq (Reg iR) (Reg szR)) endL:loop++[MT iR (IB IPlus (Reg iR) (ConstI 1)), J l, L endL, Pop nIr])
aeval (EApp oTy (EApp _ (EApp _ (Builtin _ Gen) seed) op) n) t | i1 (oTy) = do
    a <- nextArr
    arg <- newITemp
    i <- newITemp
    nR <- newITemp
    let sz = IB IPlus (Reg nR) (ConstI 24)
    putSeed <- eval seed arg
    putN <- eval n nR
    l <- newLabel
    endL <- newLabel
    ss <- writeRF op [arg] arg
    let loop = [Wr (AP t (Just (IB IPlus (IB IAsl (Reg i) (ConstI 3)) (ConstI 16))) (Just a)) (Reg arg)] ++ ss
    pure (Just a, putSeed ++ putN ++ Ma a t sz:dim1 (Just a) t (Reg nR) ++ MT i (ConstI 0):L l:MJ (IRel IGt (Reg i) (Reg nR)) endL:loop ++ [MT i (IB IPlus (Reg i) (ConstI 1)), J l, L endL])
aeval e _ = error (show e)

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
eval (EApp _ (EApp _ (EApp _ (Builtin _ (Fold 1)) op) seed) (EApp _ (EApp _ (EApp _ (Builtin _ IRange) start) end) (ILit _ j))) acc = do
    i <- newITemp
    endR <- newITemp
    l <- newLabel
    endL <- newLabel
    putStart <- eval start i
    putAcc <- eval seed acc
    irEnd <- eval end endR
    step <- writeRF op [acc, i] acc
    pure $ putStart ++ putAcc ++ irEnd ++ (L l:MJ (IRel IGt (Reg i) (Reg endR)) endL:step) ++ [MT i (IB IPlus (Reg i) (ConstI $ asI j)), J l, L endL]
eval (EApp _ (EApp _ (EApp _ (Builtin _ (Fold 1)) op) seed) (EApp _ (EApp _ (EApp _ (Builtin _ IRange) start) end) incr)) acc = do
    i <- newITemp
    endR <- newITemp
    incrR <- newITemp
    l <- newLabel
    endL <- newLabel
    putStart <- eval start i
    putAcc <- eval seed acc
    irEnd <- eval end endR
    irIncr <- eval incr incrR
    step <- writeRF op [acc, i] acc
    -- TODO: is this shortest loop?
    pure $ putStart ++ putAcc ++ irEnd ++ irIncr ++ (L l:MJ (IRel IGt (Reg i) (Reg endR)) endL:step) ++ [MT i (IB IPlus (Reg i) (Reg incrR)), J l, L endL]
-- TODO: start, end, nSteps a literal
eval (EApp _ (EApp _ (EApp _ (Builtin _ (Fold 1)) op) seed) (EApp _ (EApp _ (EApp _ (Builtin _ FRange) (ILit _ start)) (ILit _ end)) (ILit _ nSteps))) acc = do
    i <- newITemp
    l <- newLabel
    endL <- newLabel
    let incr = fromIntegral (end-start+1)/fromIntegral nSteps
    xR <- newFTemp
    putAcc <- eval seed acc
    step <- writeRF op [acc, xR] acc
    pure $ putAcc ++ (MX xR (ConstF $ fromIntegral start):MT i (ConstI 1):L l:MJ (IRel IGt (Reg i) (ConstI $ asI nSteps)) endL:step) ++ [MT i (IB IPlus (Reg i) (ConstI 1)), MX xR (FB FPlus (FReg xR) (ConstF incr)), J l, L endL]
eval (EApp _ (EApp _ (EApp _ (Builtin _ (Fold 1)) op) seed) (EApp _ (EApp _ (EApp _ (Builtin _ FRange) start) end) nSteps@(EApp _ (Builtin _ Floor) nStepsF))) acc = do
    i <- newITemp
    startR <- newFTemp
    incrR <- newFTemp
    xR <- newFTemp
    endI <- newITemp
    l <- newLabel
    endL <- newLabel
    putStart <- eval start startR
    putAcc <- eval seed acc
    putIEnd <- eval nSteps endI
    putIncr <- eval (((end `eMinus` start) `ePlus` FLit F 1) `eDiv` nStepsF) incrR
    -- step the accumulating value
    step <- writeRF op [acc, xR] acc
    pure $ putStart ++ (MX xR (FReg startR):putIEnd) ++ putIncr ++ putAcc ++ (MT i (ConstI 1):L l:MJ (IRel IGt (Reg i) (Reg endI)) endL:step) ++ [MT i (IB IPlus (Reg i) (ConstI 1)), MX xR (FB FPlus (FReg xR) (FReg incrR)), J l, L endL]
eval (EApp _ (EApp _ (EApp _ (Builtin _ (Fold 1)) op) seed) (EApp _ (EApp _ (EApp _ (Builtin _ FRange) start) end) nSteps)) acc = do
    i <- newITemp
    startR <- newFTemp
    incrR <- newFTemp
    xR <- newFTemp
    endI <- newITemp
    l <- newLabel
    endL <- newLabel
    putStart <- eval start startR
    putAcc <- eval seed acc
    putIEnd <- eval nSteps endI
    putIncr <- eval (((end `eMinus` start) `ePlus` FLit F 1) `eDiv` EApp F (Builtin (Arrow I F) ItoF) nSteps) incrR
    -- step the accumulating value
    step <- writeRF op [acc, xR] acc
    pure $ putStart ++ (MX xR (FReg startR):putIEnd) ++ putIncr ++ putAcc ++ (MT i (ConstI 1):L l:MJ (IRel IGt (Reg i) (Reg endI)) endL:step) ++ [MT i (IB IPlus (Reg i) (ConstI 1)), MX xR (FB FPlus (FReg xR) (FReg incrR)), J l, L endL]
eval (EApp _ (EApp _ (Builtin (Arrow I _) Plus) e0) e1) t = do
    t0 <- newITemp
    t1 <- newITemp
    pl0 <- eval e0 t0
    pl1 <- eval e1 t1
    pure $ pl0 ++ pl1 ++ [MT t (IB IPlus (Reg t0) (Reg t1))]
eval (EApp _ (EApp _ (Builtin _ Plus) (Var F x)) (EApp _ (EApp _ (Builtin _ Times) (Var _ y)) e0)) t = do
    st <- gets vars
    t0 <- newFTemp
    pl0 <- eval e0 t0
    pure $ pl0 ++ [MX t (FB FPlus (FReg $ getT st x) (FB FTimes (FReg t0) (FReg $ getT st y)))]
eval (EApp _ (EApp _ (Builtin _ Plus) (Var F x)) (EApp _ (EApp _ (Builtin _ Times) e0) e1)) t = do
    st <- gets vars
    t0 <- newFTemp
    t1 <- newFTemp
    pl0 <- eval e0 t0
    pl1 <- eval e1 t1
    pure $ pl0 ++ pl1 ++ [MX t (FB FPlus (FReg $ getT st x) (FB FTimes (FReg t0) (FReg t1)))]
eval (EApp _ (EApp _ (Builtin (Arrow F _) Plus) e0) e1) t = do
    t0 <- newFTemp
    t1 <- newFTemp
    pl0 <- eval e0 t0
    pl1 <- eval e1 t1
    pure $ pl0 ++ pl1 ++ [MX t (FB FPlus (FReg t0) (FReg t1))]
eval (EApp _ (EApp _ (Builtin (Arrow I _) Times) (Var I x)) (Var _ y)) t = do
    st <- gets vars
    let xT = getT st x
        yT = getT st y
    pure [MT t (IB ITimes (Reg xT) (Reg yT))]
eval (EApp _ (EApp _ (Builtin (Arrow I _) Times) e0) e1) t = do
    t0 <- newITemp
    t1 <- newITemp
    pl0 <- eval e0 t0
    pl1 <- eval e1 t1
    pure $ pl0 ++ pl1 ++ [MT t (IB ITimes (Reg t0) (Reg t1))]
eval (EApp _ (EApp _ (Builtin _ Minus) (Var _ x)) (ILit F n)) t = do
    st <- gets vars
    pure [MX t (FB FMinus (FReg $ getT st x) (ConstF $ fromIntegral n))]
eval (EApp _ (EApp _ (Builtin _ Minus) e) (ILit F i)) t = do
    tϵ <- newFTemp
    pl <- eval e tϵ
    pure $ pl ++ [MX t (FB FMinus (FReg tϵ) (ConstF $ fromIntegral i))]
eval (EApp _ (EApp _ (Builtin _ Minus) (Var _ x)) (ILit I i)) t = do
    st <- gets vars
    pure [MT t (IB IMinus (Reg $ getT st x) (ConstI $ asI i))]
eval (EApp _ (EApp _ (Builtin _ Minus) e) (ILit I i)) t = do
    tϵ <- newITemp
    pl <- eval e tϵ
    pure $ pl ++ [MT t (IB IMinus (Reg tϵ) (ConstI $ asI i))]
eval (EApp _ (EApp _ (Builtin (Arrow I _) Minus) e0) e1) t = do
    t0 <- newITemp
    t1 <- newITemp
    pl0 <- eval e0 t0
    pl1 <- eval e1 t1
    pure $ pl0 ++ pl1 ++ [MT t (IB IMinus (Reg t0) (Reg t1))]
eval (ILit F x) t = pure [MX t (ConstF $ fromIntegral x)] -- if it overflows you deserve it
eval (ILit _ i) t = pure [MT t (ConstI $ asI i)]
eval (Var F x) t = do
  st <- gets vars
  pure [MX t (FReg $ getT st x)]
eval (Var I x) t = do
    st <- gets vars
    pure [MT t (Reg $ getT st x)]
eval (EApp _ (Builtin _ ItoF) (ILit _ i)) t = do
    pure [MX t (ConstF $ fromIntegral i)]
eval (EApp _ (Builtin _ ItoF) (Var _ x)) t = do
    st <- gets vars
    pure [MX t (FConv $ Reg $ getT st x)]
eval (EApp _ (Builtin _ ItoF) e) t = do
    iR <- newITemp
    pl<- eval e iR
    pure $ pl ++ [MX t (FConv $ Reg iR)]
eval (EApp _ (EApp _ (Builtin _ Div) e) (Var _ x)) t = do
    tf <- newFTemp
    st <- gets vars
    pl <- eval e tf
    pure $ pl ++ [MX t (FB FDiv (FReg tf) (FReg $ getT st x))]
eval (EApp _ (EApp _ (Builtin _ Div) e0) e1) t = do
    t0 <- newFTemp
    t1 <- newFTemp
    pl0 <- eval e0 t0
    pl1 <- eval e1 t1
    pure $ pl0 ++ pl1 ++ [MX t (FB FDiv (FReg t0) (FReg t1))]
eval (EApp _ (EApp _ (Builtin _ A.IDiv) e0) e1) t = do
    t0 <- newITemp
    t1 <- newITemp
    pl0 <- eval e0 t0
    pl1 <- eval e1 t1
    pure $ pl0 ++ pl1 ++ [MT t (IB IR.IDiv (Reg t0) (Reg t1))]
eval (EApp F (EApp _ (Builtin _ Times) (Var F x)) (Var F y)) t = do
    st <- gets vars
    let xT = getT st x
        yT = getT st y
    pure [MX t (FB FTimes (FReg xT) (FReg yT))]
eval (EApp F (EApp _ (Builtin _ Times) (Var F x)) e) t = do
    st <- gets vars
    t' <- newFTemp
    pl <- eval e t'
    pure $ pl ++ [MX t (FB FTimes (FReg $ getT st x) (FReg t'))]
eval (EApp F (EApp _ (Builtin _ Times) e0) e1) t = do
    t0 <- newFTemp
    t1 <- newFTemp
    pl0 <- eval e0 t0
    pl1 <- eval e1 t1
    pure $ pl0 ++ pl1 ++ [MX t (FB FTimes (FReg t0) (FReg t1))]
eval (EApp F (EApp _ (Builtin _ Minus) e0) e1) t = do
    t0 <- newFTemp
    t1 <- newFTemp
    pl0 <- eval e0 t0
    pl1 <- eval e1 t1
    pure $ pl0 ++ pl1 ++ [MX t (FB FMinus (FReg t0) (FReg t1))]
eval (EApp F (EApp _ (Builtin _ Exp) (FLit _ x)) e) t = do
    f <- newFTemp
    plE <- eval e f
    pure $ plE ++ [MX t (FB FExp (ConstF x) (FReg f))]
eval (EApp F (EApp _ (Builtin _ Exp) e0) e1) t = do
    f0 <- newFTemp
    f1ϵ <- newFTemp
    plE0 <- eval e0 f0
    plE1 <- eval e1 f1ϵ
    pure $ plE0 ++ plE1 ++ [MX t (FB FExp (FReg f0) (FReg f1ϵ))]
eval (EApp F (EApp _ (Builtin _ IntExp) x) n) t = do
    i <- newITemp
    nR <- newITemp
    plR <- eval n nR
    xR <- newFTemp
    plX <- eval x xR
    l <- newLabel
    endL <- newLabel
    pure $ plR ++ plX ++ [MX t (ConstF 1), MT i (Reg nR), L l, MJ (IRel IEq (Reg i) (ConstI 0)) endL, MX t (FB FTimes (FReg t) (FReg xR)), MT i (IB IMinus (Reg i) (ConstI 1)), J l, L endL]
eval (EApp _ (EApp _ (Builtin _ IntExp) x) n) t = do
    i <- newITemp
    nR <- newITemp
    plR <- eval n nR
    xR <- newITemp
    plX <- eval x xR
    l <- newLabel
    endL <- newLabel
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
    l <- newLabel
    endL <- newLabel
    stepR <- writeRF op [acc, x] acc
    let step = MX x (FAt (AP arrR (Just$IB IAsl (Reg i) (ConstI 3)) mI)):stepR ++ [MT i (IB IPlus (Reg i) (ConstI 1))]
    -- GHC uses 'length' but our szR needs to be one less
    pure $ plE ++ putAcc ++ MT i (ConstI 0):MT szR (EAt (AP eR (Just (ConstI 8)) mI)):MT arrR (IB IPlus (Reg eR) (ConstI 16)):MT szR (IB IMinus (Reg szR) (ConstI 1)):L l:MJ (IRel IGt (Reg i) (Reg szR)) endL:step++[J l, L endL]
eval (EApp _ (EApp _ (EApp _ (Builtin _ Foldl) op) seed) e) acc | f1 (eAnn e) = do
    x <- newFTemp
    arrR <- newITemp
    eR <- newITemp
    i <- newITemp
    (mI, plE) <- aeval e eR
    putAcc <- eval seed acc
    l <- newLabel
    endL <- newLabel
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
    l <- newLabel
    endL <- newLabel
    stepR <- writeRF op [acc, x] acc
    let step = MT x (EAt (AP arrR Nothing mI)):stepR ++ [MT arrR (IB IPlus (Reg arrR) (ConstI 16))]
    -- GHC uses 'length' but our szR needs to be one less
    pure $ plE ++ putAcc ++ MT i (ConstI 0):MT szR (EAt (AP eR (Just (ConstI 8)) mI)):MT arrR (IB IPlus (Reg eR) (ConstI 16)):MT szR (IB IMinus (Reg szR) (ConstI 1)):L l:MJ (IRel IGt (Reg i) (Reg szR)) endL:step++[MT i (IB IPlus (Reg i) (ConstI 1)), J l, L endL]
eval (Id F (FoldOfZip seed op [p, q])) acc | f1 (eAnn p) && f1 (eAnn q) = do
    x <- newFTemp
    y <- newFTemp
    pR <- newITemp
    qR <- newITemp
    szR <- newITemp
    arr0R <- newITemp
    arr1R <- newITemp
    i <- newITemp
    (iP, plP) <- aeval p pR
    (iQ, plQ) <- aeval q qR
    putAcc <- eval seed acc
    l <- newLabel
    endL <- newLabel
    stepR <- writeRF op [acc, x, y] acc
    let step = MX x (FAt (AP arr0R (Just$IB IAsl (Reg i) (ConstI 3)) iP)):MX y (FAt (AP arr1R (Just$IB IAsl (Reg i) (ConstI 3)) iQ)):stepR ++ [MT i (IB IPlus (Reg i) (ConstI 1))]
    -- FIXME: this assumes the arrays are the same size
    pure $ plP ++ plQ ++ putAcc ++ MT i (ConstI 0):MT szR (EAt (AP pR (Just (ConstI 8)) iP)):MT arr0R (IB IPlus (Reg pR) (ConstI 16)):MT arr1R (IB IPlus (Reg qR) (ConstI 16)):MT szR (IB IMinus (Reg szR) (ConstI 1)):L l:MJ (IRel IGt (Reg i) (Reg szR)) endL:step++[J l, L endL]
eval (Id F (FoldOfZip seed op [EApp _ (EApp _ (EApp _ (Builtin _ IRange) start) _) incr, ALit ty qs])) acc | f1 ty = do
    x <- newITemp
    i <- newITemp
    y <- newFTemp
    plX <- eval start x
    plI <- eval incr i
    putAcc <- eval seed acc
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
eval (EApp _ (Builtin _ Size) e) t | unDim (eAnn e) = do
    r <- newITemp
    (mI, plE) <- aeval e r
    pure $ plE ++ [MT t (EAt (AP r (Just (ConstI 8)) mI))]
eval (EApp _ (Builtin _ Size) e) t = do
    r <- newITemp
    (mI, plE) <- aeval e r
    rnkR <- newITemp
    l <- newLabel
    endL <- newLabel
    i <- newITemp
    pure $ plE ++ [MT rnkR (EAt (AP r Nothing mI)), MT i (ConstI 8), MT t (EAt (AP r (Just (ConstI 8)) mI)), L l, MJ (IRel IGt (Reg i) (Reg rnkR)) endL, MT i (IB IPlus (Reg i) (ConstI 8)), MT t (IB ITimes (Reg t) (EAt (AP r (Just (Reg i)) mI))),J l, L endL]
eval (EApp I (EApp _ (Builtin _ Max) e0) (Var _ y)) t = do
    st <- gets vars
    e0R <- newITemp
    let e1R = getT st y
        e1RE = Reg e1R
    plE0 <- eval e0 e0R
    pure $ if e1R == t
        then plE0 ++ [MT t (Reg e0R), Cmov (IRel IGt e1RE (Reg e0R)) t e1RE]
        else plE0 ++ [MT t e1RE, Cmov (IRel IGt (Reg e0R) e1RE) t (Reg e0R)]
eval (EApp I (EApp _ (Builtin _ Max) e0) e1) t = do
    e0R <- newITemp
    e1R <- newITemp
    plE0 <- eval e0 e0R
    plE1 <- eval e1 e1R
    pure $ plE0 ++ plE1 ++ [MT t (Reg e1R), Cmov (IRel IGt (Reg e0R) (Reg e1R)) t (Reg e0R)]
eval (EApp F (Builtin _ Head) arr) t | f1 (eAnn arr) = do
    r <- newITemp
    (mL, plArr) <- aeval arr r
    -- rank 1
    pure $ plArr ++ [MX t (FAt (AP r (Just $ ConstI 16) mL))]
eval (EApp I (Builtin _ Head) arr) t | i1 (eAnn arr) = do
    r <- newITemp
    (mL, plArr) <- aeval arr r
    -- rank 1
    pure $ plArr ++ [MT t (EAt (AP r (Just $ ConstI 16) mL))]
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

unDim :: T a -> Bool
unDim (Arr (_ `Cons` Nil) _) = True
unDim _                      = False

asI :: Integer -> Int64
asI i | i < fromIntegral (minBound :: Int64) || i > fromIntegral (maxBound :: Int64) = error "integer literal out of bounds!"
      | otherwise = fromIntegral i
