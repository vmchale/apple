module A.Opt ( optA
             ) where

import           A
import           Data.Bits ((.<<.), (.>>.))
import           R
import           R.R
import           Sh

infixl 6 `iMinus`
infixl 6 `iPlus`

fop op e0 = EApp F (EApp (F ~> F) (Builtin (F ~> F ~> F) op) e0)
eMinus = fop Minus; eDiv = fop Div; ePlus = fop Plus

iop op e0 = EApp I (EApp (I ~> I) (Builtin (I ~> I ~> I) op) e0)
iDiv = iop IDiv; iMinus = iop Minus; iPlus = iop Plus

mShLit (Id _ (AShLit is es)) = Just (is, es)
mShLit (ALit _ es)           = Just ([length es], es)
mShLit _                     = Nothing

mSz :: Sh a -> Maybe Int
mSz (Ix _ i `Cons` sh) = (i*)<$>mSz sh
mSz Nil                = Just 1
mSz _                  = Nothing

ff :: Builtin -> Maybe (Double -> Double -> Double)
ff Plus = Just (+); ff Times = Just (*); ff Minus = Just (-)
ff _ = Nothing

optA :: E (T ()) -> RM (E (T ()))
optA (ILit F x)            = pure (FLit F (realToFrac x))
optA e@ILit{}              = pure e
optA e@FLit{}              = pure e
optA e@BLit{}              = pure e
optA e@Var{}               = pure e
optA (Builtin t (Rank rs)) = pure (Builtin t (Rank (g<$>rs))) where g r@(_,Just{})=r; g (cr,Nothing)=(cr, Just [1..cr])
-- TODO: nicer to do fold-of-seed
optA (Builtin ty Dot)      | Arrow tA (Arrow _ tN) <- ty = do
    a <- nextU "a" tA; b <- nextU "b" tA
    x₀ <- nextU "x₀" tN; y₀ <- nextU "y₀" tN
    x <- nextU "x" tN; y <- nextU "y" tN; z <- nextU "z" tN
    let n3=tN~>tN~>tN; n2=tN~>tN
    let op=Lam n3 x₀ (Lam n2 y₀ (EApp tN (EApp n2 (Builtin n3 Times) (Var tN x₀)) (Var tN y₀)))
        zop=Lam (tN~>tN~>tN~>tN) x (Lam n3 y (Lam n2 z (EApp tN (EApp n2 (Builtin n3 Plus) (Var tN x)) (EApp tN (EApp n2 (Builtin n3 Times) (Var tN y)) (Var tN z)))))
    pure $ Lam ty a (Lam undefined b (Id tN $ FoldOfZip op zop [Var tA a, Var tA b]))
optA (Builtin ty C)        | Arrow fTy (Arrow gTy@(Arrow _ gC) xTy@(Arrow tC tD)) <- ty = do
    f <- nextU "f" fTy; g <- nextU "g" gTy; x <- nextU "x" tC
    pure $ Lam ty f (Lam (gTy ~> xTy) g (Lam (tC ~> tD) x (EApp tD (Var fTy f) (EApp gC (Var gTy g) (Var tC x)))))
optA e@Builtin{}           = pure e
optA (EApp _ (Builtin _ Size) xs) | Arr sh _ <- eAnn xs, Just sz <- mSz sh = pure $ ILit I (toInteger sz)
optA (EApp _ (Builtin _ Dim) xs) | Arr (Ix _ i `Cons` _) _ <- eAnn xs = pure $ ILit I (toInteger i)
-- TODO: rewrite Head to Aɴ for simplicity in C.Trans (and A1, Last when possible...)
optA (EApp l (Builtin l₁ Head) e) =
    optA $ Id l (Aɴ e [ILit l₁ 0])
optA (EApp l (Builtin _ Last) e) =
    optA $ Id l (Aɴ e [EApp I (Builtin undefined Dim) e `iMinus` ILit I 1])
optA (EApp l (EApp _ (Builtin _ A1) e) n) =
    optA $ Id l (Aɴ e [n])
optA (Id l0 (Aɴ e ns)) = do
    e' <- optA e; ns' <- traverse optA ns
    pure $ case e' of
        (Id _ (Aɴ e₁ ns₁))                     -> Id l0 $ Aɴ e₁ (ns₁++ns')
        (EApp _ (EApp _ (Builtin _ A1) e₁) n₁) -> Id l0 $ Aɴ e₁ (n₁:ns')
        _                                      -> Id l0 (Aɴ e' ns')
optA (EApp l0 (EApp l1 op@(Builtin _ IDiv) e0) e1) = do
    e0' <- optA e0; e1' <- optA e1
    pure $ case (e0',e1') of
        (ILit _ i, ILit _ j) -> ILit l0 (i `quot` j)
        (x, ILit _ 1)        -> x
        _                    -> EApp l0 (EApp l1 op e0') e1'
optA (EApp l0 (EApp l1 op@(Builtin l2 Div) e0) e1) = do
    e0' <- optA e0
    e1' <- optA e1
    pure $ case (e0', e1') of
        (FLit _ x, FLit _ y) -> FLit l0 (x/y)
        (x, FLit t y)        -> EApp l0 (EApp l1 (Builtin l2 Times) x) (FLit t (1/y))
        _                    -> EApp l0 (EApp l1 op e0') e1'
optA (EApp l0 (EApp l1 op@(Builtin _ Exp) x) y) = do
    xO <- optA x
    yO <- optA y
    pure $ case (xO, yO) of
        (FLit _ x', FLit _ y') -> FLit F (x'**y')
        _                      -> EApp l0 (EApp l1 op xO) yO
optA (EApp l0 op@(Builtin _ N) e0) = do
    e0' <- optA e0
    pure $ case e0' of
        (BLit _ b) -> BLit B (not b)
        _          -> EApp l0 op e0'
optA (EApp l0 (EApp l1 op@(Builtin _ Sr) e0) e1) = do
    e0' <- optA e0
    e1' <- optA e1
    pure $ case (e0', e1') of
        (ILit _ m, ILit _ n) -> ILit I (m .>>. fromIntegral n)
        _                    -> EApp l0 (EApp l1 op e0') e1'
optA (EApp l0 (EApp l1 op@(Builtin _ Sl) e0) e1) = do
    e0' <- optA e0
    e1' <- optA e1
    pure $ case (e0', e1') of
        (ILit _ m, ILit _ n) -> ILit I (m .<<. fromIntegral n)
        _                    -> EApp l0 (EApp l1 op e0') e1'
optA (Lam l n e) = Lam l n <$> optA e
optA (EApp l0 (EApp l1 op@(Builtin _ Minus) x) y) = do
    x0 <- optA x; y0 <- optA y
    pure $ case (x0,y0) of
        (FLit _ x', FLit _ y')                                           -> FLit F (x'-y')
        (ILit I x', ILit I y')                                           -> ILit I (x'-y')
        -- ((1.0+kk)-2.0)
        (EApp l0ϵ s@(EApp _ (Builtin _ Minus) _) (FLit _ x'), FLit _ y') -> EApp l0ϵ s (FLit F (x'+y'))
        (x', ILit _ 0)                                                   -> x'
        _                                                                -> EApp l0 (EApp l1 op x0) y0
optA (EApp l0 (EApp l1 op@(Builtin _ Plus) x) y) = do
    x0 <- optA x; y0 <- optA y
    pure $ case (x0,y0) of
        (FLit _ x', FLit _ y')                                           -> FLit F (x'+y')
        (ILit I x', ILit I y')                                           -> ILit I (x'+y')
        (EApp l0ϵ a@(EApp _ (Builtin _ Minus) _) (ILit li m), ILit _  n) -> EApp l0ϵ a (ILit li (m-n))
        (x', ILit _ 0)                                                   -> x'
        (ILit _ 0, x')                                                   -> x'
        _                                                                -> EApp l0 (EApp l1 op x0) y0
optA (EApp l0 (EApp l1 op@(Builtin _ f) x) y) | Just g <- ff f = do
    xO <- optA x
    yO <- optA y
    pure $ case (xO, yO) of
        (FLit _ x', FLit _ y') -> FLit F (g x' y')
        _                      -> EApp l0 (EApp l1 op xO) yO
optA (EApp l0 f@(Builtin _ ItoF) x) = do
    x' <- optA x
    pure $ case x' of
        ILit _ n -> FLit F (realToFrac n)
        _        -> EApp l0 f x'
optA (EApp l op@(Builtin _ Sqrt) x) = do
    xO <- optA x
    pure $ case xO of
        FLit _ z -> FLit F (sqrt z)
        _        -> EApp l op xO
optA (EApp _ (Builtin _ Floor) (EApp _ (Builtin _ ItoF) x)) = optA x
optA (EApp ty (EApp _ (Builtin _ IntExp) x) (ILit _ 2)) = pure $ EApp ty (EApp (ty ~> ty) (Builtin (ty ~> ty ~> ty) Times) x) x
optA (EApp l (EApp _ (EApp _ (Builtin _ FRange) start) end) nSteps) = do
    start' <- optA start; end' <- optA end; nSteps' <- optA nSteps
    incr <- optA $ (end' `eMinus` start') `eDiv` (EApp F (Builtin (Arrow I F) ItoF) nSteps' `eMinus` FLit F 1)
    n <- nextU "n" F
    pure $ EApp l (EApp undefined (EApp undefined (Builtin undefined Gen) start') (Lam (F ~> F) n (Var F n `ePlus` incr))) nSteps'
optA (EApp l (EApp _ (EApp _ (Builtin _ IRange) start) end) incr) = do
    start' <- optA start; end' <- optA end; incr' <- optA incr
    k <- nextU "k" I
    n <- optA $ (end' `iMinus` start' `iPlus` ILit I 1) `iDiv` incr'
    pure $ EApp l (EApp undefined (EApp undefined (Builtin undefined Gen) start') (Lam (I ~> I) k (Var I k `iPlus` incr'))) n
optA (EApp l0 (EApp l1 ho0@(Builtin _ Fold) op) e) = do
    e' <- optA e; op' <- optA op
    case e' of
        (EApp _ (EApp _ (EApp _ (Builtin _ Gen) seed) f) n) ->
            pure $ Id l0 $ FoldGen seed f op' n
        (EApp _ (EApp _ (Builtin _ Map) f) x)
            | fTy@(Arrow dom fCod) <- eAnn f
            , Arrow _ (Arrow _ cod) <- eAnn op' -> do
                  f' <- rE f
                  x0 <- nextU "x" cod; x1 <- nextU "y" dom
                  x0' <- nextU "x" dom
                  let vx0 = Var cod x0; vx1 = Var dom x1
                      vx0' = Var dom x0'
                      opT = cod ~> dom ~> cod
                      opϵ = Lam opT x0 (Lam (dom ~> cod) x1 (EApp cod (EApp undefined op' vx0) (EApp fCod f vx1)))
                      f'' = Lam fTy x0' (EApp fCod f' vx0')
                  pure $ Id l0 $ FoldOfZip f'' opϵ [x]
        (EApp _ (EApp _ (EApp _ (Builtin _ Zip) f) xs) ys)
            | fTy@(Arrow dom0 (Arrow dom1 dom2)) <- eAnn f
            , Arrow _ (Arrow _ cod) <- eAnn op -> do
                f' <- rE f
                x0 <- nextU "x" cod; x1 <- nextU "y" dom0; x2 <- nextU "z" dom1
                x0' <- nextU "x" dom0; x1' <- nextU "y" dom1
                let vx0 = Var cod x0; vx1 = Var dom0 x1; vx2 = Var dom1 x2
                    vx0' = Var dom0 x0'; vx1' = Var dom1 x1'
                    opT = cod ~> dom0 ~> dom1 ~> cod
                    opϵ = Lam opT x0 (Lam undefined x1 (Lam (dom1 ~> cod) x2 (EApp cod (EApp undefined op' vx0) (EApp dom2 (EApp undefined f vx1) vx2))))
                    f'' = Lam fTy x0' (Lam undefined x1' (EApp dom2 (EApp undefined f' vx0') vx1'))
                pure $ Id l0 $ FoldOfZip f'' opϵ [xs,ys]
        _ -> pure $ EApp l0 (EApp l1 ho0 op') e'
optA (EApp l0 (EApp _ (Builtin _ Succ) f) (EApp _ (EApp _ (Builtin _ Map) g) xs))
    | (Arrow _ (Arrow _ fCod)) <- eAnn f
    , (Arrow gDom _) <- eAnn g = do
        f' <- optA f; g' <- optA g; g'' <- rE g
        xs' <- optA xs
        x <- nextU "w" gDom; y <- nextU "v" gDom
        let vx=Var gDom x; vy=Var gDom y
            f2g=Lam (gDom ~> gDom ~> fCod) x (Lam (gDom ~> fCod) y (EApp undefined (EApp undefined f' (EApp undefined g' vx)) (EApp undefined g'' vy)))
        pure (EApp l0 (EApp undefined (Builtin undefined Succ) f2g) xs')
optA (EApp l0 (EApp _ (Builtin _ Map) f) (EApp _ (EApp _ (Builtin _ Map) g) xs))
    | (Arrow _ fCod) <- eAnn f
    , (Arrow gDom gCod) <- eAnn g = do
        f' <- optA f; g' <- optA g
        xs' <- optA xs
        x <- nextU "x" gDom
        let vx=Var gDom x
            fog=Lam (gDom ~> fCod) x (EApp fCod f' (EApp gCod g' vx))
        pure (EApp l0 (EApp undefined (Builtin undefined Map) fog) xs')
optA (EApp l0 (EApp _ (Builtin _ (Rank [(0,_)])) f) (EApp _ (EApp _ (EApp _ ho@(Builtin _ (Rank [(0,_),(0,_)])) op) xs) ys))
    | Arrow _ cod <- eAnn f
    , Arrow dom0 (Arrow dom1 _) <- eAnn op = do
        f' <- optA f; opA <- optA op; ho' <- optA ho
        xs' <- optA xs; ys' <- optA ys
        x <- nextU "x" dom0; y <- nextU "y" dom1
        let vx=Var dom0 x; vy=Var dom1 y
            opTy = dom0 ~> dom1 ~> cod
            op' = Lam opTy x (Lam undefined y (EApp undefined f' (EApp undefined (EApp undefined opA vx) vy)))
        pure (EApp l0 (EApp undefined (EApp undefined (ho' { eAnn = undefined }) op') xs') ys')
optA (EApp l0 (EApp _ (EApp _ ho@(Builtin _ (Rank [(0,_),(0,_)])) op) (EApp _ (EApp _ (Builtin _ (Rank [(0,_)])) f) xs)) (EApp _ (EApp _ (Builtin _ (Rank [(0,_)])) g) ys))
    | Arrow dom0 _ <- eAnn f
    , Arrow dom1 _ <- eAnn g
    , Arrow _ (Arrow _ cod) <- eAnn op = do
        f' <- optA f; g' <- optA g
        opA <- optA op; ho' <- optA ho
        xs' <- optA xs; ys' <- optA ys
        x <- nextU "x" dom0; y <- nextU "y" dom1
        let vx = Var dom0 x; vy = Var dom1 y
            opTy = dom0 ~> dom1 ~> cod
            op' = Lam opTy x (Lam undefined y (EApp undefined (EApp undefined opA (EApp undefined f' vx)) (EApp undefined g' vy)))
        pure (EApp l0 (EApp undefined (EApp undefined (ho' { eAnn = undefined }) op') xs') ys')
optA (EApp l0 (EApp _ (EApp _ ho@(Builtin _ (Rank [(0,_),(0,_)])) op) xs) (EApp _ (EApp _ (Builtin _ (Rank [(0,_)])) g) ys))
    | Arrow dom _ <- eAnn g
    , Arrow xT (Arrow _ cod) <- eAnn op = do
        g' <- optA g
        opA <- optA op; ho' <- optA ho
        xs' <- optA xs; ys' <- optA ys
        x <- nextU "x" xT; y <- nextU "y" dom
        let vx = Var xT x; vy = Var dom y
            opTy = xT ~> dom ~> cod
            op' = Lam opTy x (Lam undefined y (EApp undefined (EApp undefined opA vx) (EApp undefined g' vy)))
        pure (EApp l0 (EApp undefined (EApp undefined (ho' { eAnn = undefined }) op') xs') ys')
optA (EApp l0 (EApp _ (EApp _ ho@(Builtin _ (Rank [(0,_),(0,_)])) op) (EApp _ (EApp _ (Builtin _ (Rank [(0,_)])) f) xs)) ys)
    | Arrow dom _ <- eAnn f
    , Arrow _ (Arrow yT cod) <- eAnn op = do
        f' <- optA f
        opA <- optA op; ho' <- optA ho
        xs' <- optA xs; ys' <- optA ys
        x <- nextU "x" dom; y <- nextU "y" yT
        let vx = Var dom x; vy = Var yT y
            opTy = dom ~> yT ~> cod
            op' = Lam opTy x (Lam undefined y (EApp undefined (EApp undefined opA (EApp undefined f' vx)) vy))
        pure (EApp l0 (EApp undefined (EApp undefined (ho' { eAnn = undefined }) op') xs') ys')
optA (EApp _ (EApp _ (EApp _ (Builtin _ Zip) op) (EApp _ (EApp _ (Builtin _ Map) f) xs)) (EApp _ (EApp _ (Builtin _ Map) g) ys))
    | Arrow dom0 _ <- eAnn f
    , Arrow dom1 _ <- eAnn g
    , Arrow _ (Arrow _ cod) <- eAnn op = do
        f' <- optA f; g' <- optA g
        opA <- optA op
        xs' <- optA xs; ys' <- optA ys
        x0 <- nextU "x" dom0; x1 <- nextU "y" dom1
        let vx0 = Var dom0 x0; vx1 = Var dom1 x1
            opTy = dom0 ~> dom1 ~> cod
            op' = Lam opTy x0 (Lam undefined x1 (EApp undefined (EApp undefined opA (EApp undefined f' vx0)) (EApp undefined g' vx1)))
        pure (EApp undefined (EApp undefined (EApp undefined (Builtin undefined Zip) op') xs') ys')
optA (EApp l (EApp _ (EApp _ (Builtin _ Zip) op) (EApp _ (EApp _ (Builtin _ Map) f) xs)) ys)
    | Arrow dom0 _ <- eAnn f
    , Arrow _ (Arrow dom1 cod) <- eAnn op = do
        f' <- optA f
        opA <- optA op
        xs' <- optA xs; ys' <- optA ys
        x0 <- nextU "x" dom0; x1 <- nextU "y" dom1
        let vx0 = Var dom0 x0; vx1 = Var dom1 x1
            opTy = dom0 ~> dom1 ~> cod
            op' = Lam opTy x0 (Lam undefined x1 (EApp undefined (EApp undefined opA (EApp undefined f' vx0)) vx1))
        pure (EApp l (EApp undefined (EApp undefined (Builtin undefined Zip) op') xs') ys')
optA (EApp l (EApp _ (EApp _ (Builtin _ Zip) op) xs) (EApp _ (EApp _ (Builtin _ Map) g) ys))
    | Arrow dom1 _ <- eAnn g
    , Arrow dom0 (Arrow _ cod) <- eAnn op = do
        g' <- optA g
        opA <- optA op
        xs' <- optA xs; ys' <- optA ys
        x0 <- nextU "x" dom0; x1 <- nextU "y" dom1
        let vx0 = Var dom0 x0; vx1 = Var dom1 x1
            opTy = dom0 ~> dom1 ~> cod
            op' = Lam opTy x0 (Lam undefined x1 (EApp undefined (EApp undefined opA vx0) (EApp undefined g' vx1)))
        pure (EApp l (EApp undefined (EApp undefined (Builtin undefined Zip) op') xs') ys')
optA (EApp l (EApp t0 (EApp t1 (Builtin bt b@FoldS) op) seed) arr) = do
    arr' <- optA arr
    seed' <- optA seed
    opA <- optA op
    case arr' of
        (EApp _ (EApp _ (EApp _ (Builtin _ Zip) f) (EApp _ (EApp _ (EApp _ (Builtin _ Gen) gseed0) u0) n)) (EApp _ (EApp _ (EApp _ (Builtin _ Gen) gseed1) u1) _))
            | (Arrow _ (Arrow _ tC)) <- eAnn f -> do
            x0 <- nextU "x₀" u0Ty; x1 <- nextU "x₁" u1Ty
            z <- nextU "z" undefined; y₀ <- nextU "y₀" u0Ty; y₁ <- nextU "y₁" u1Ty
            let opZ=Lam (tC~>u0Ty~>u1Ty~>tC) z (Lam undefined y₀ (Lam undefined y₁ (EApp undefined (EApp undefined opA (Var tC z)) (EApp tC (EApp undefined f (Var u0Ty y₀)) (Var u1Ty y₁)))))
            pure $ Id l $ U2 [gseed0, gseed1] [Lam (u0Ty ~> u0Ty) x0 (EApp u0Ty u0 (Var u0Ty x0)), Lam (u1Ty ~> u1Ty) x1 (EApp u1Ty u1 (Var u1Ty x1))] seed' opZ n
          where u0Ty=eAnn gseed0; u1Ty=eAnn gseed1
        (EApp _ (EApp _ (EApp _ (Builtin _ Zip) f) xs) ys)
            | Arrow dom0 (Arrow dom1 dom2) <- eAnn f
            , Arrow _ (Arrow _ cod) <- eAnn op -> do
                x0 <- nextU "x" cod; x1 <- nextU "y" dom0; x2 <- nextU "z" dom1
                let vx0 = Var cod x0; vx1 = Var dom0 x1; vx2 = Var dom1 x2
                    opTy = cod ~> dom0 ~> dom1 ~> cod
                    op' = Lam opTy x0 (Lam undefined x1 (Lam (dom1 ~> cod) x2 (EApp cod (EApp undefined opA vx0) (EApp dom2 (EApp undefined f vx1) vx2))))
                pure $ Id l $ FoldSOfZip seed' op' [xs,ys]
        (EApp _ (EApp _ (Builtin _ Map) f) xs)
            | Arrow dom fCod <- eAnn f
            , Arrow _ (Arrow _ cod) <- eAnn op -> do
                x0 <- nextU "x" cod; x1 <- nextU "y" dom
                let vx0 = Var cod x0; vx1 = Var dom x1
                    opTy = cod ~> dom ~> cod
                    op' = Lam opTy x0 (Lam (dom ~> cod) x1 (EApp cod (EApp undefined opA vx0) (EApp fCod f vx1)))
                    arrTy = eAnn xs
                optA (EApp l (EApp undefined (EApp (arrTy ~> l) (Builtin (opTy ~> arrTy ~> l) FoldS) op') seed) xs)
        (EApp _ (EApp _ (EApp _ (Builtin _ Gen) u) f) n) -> do
            x <- nextU "x" uTy
            pure $ Id l $ U2 [u] [Lam (uTy ~> uTy) x (EApp uTy f (Var uTy x))] seed' opA n
          where uTy = eAnn u
        _ -> pure (EApp l (EApp t0 (EApp t1 (Builtin bt b) opA) seed') arr')
optA (EApp l e0 e1) = EApp l <$> optA e0 <*> optA e1
optA (ALit l es) = do
    es' <- traverse optA es
    pure $ case unzip <$> traverse mShLit es' of
        Nothing        -> Id l $ AShLit [length es] es'
        Just (ds, esϵ) -> Id l $ AShLit (length ds : head ds) (concat esϵ)
optA (Tup l es) = Tup l <$> traverse optA es
optA (Let l (n, e') e) = do
    e'Opt <- optA e'
    eOpt <- optA e
    pure $ Let l (n, e'Opt) eOpt
optA (LLet l (n, e') e) = do
    e'Opt <- optA e'
    eOpt <- optA e
    pure $ LLet l (n, e'Opt) eOpt
optA (Id l idm) = Id l <$> optI idm
optA (Cond l p e0 e1) = Cond l <$> optA p <*> optA e0 <*> optA e1

optI (FoldSOfZip seed op es) = FoldSOfZip <$> optA seed <*> optA op <*> traverse optA es
optI (FoldOfZip zop op es)   = FoldOfZip <$> optA zop <*> optA op <*> traverse optA es
optI (FoldGen seed f g n)    = FoldGen <$> optA seed <*> optA f <*> optA g <*> optA n
optI (U2 seed f u g n)       = U2 <$> traverse optA seed <*> traverse optA f <*> optA u <*> optA g <*> optA n
optI (AShLit ds es)          = AShLit ds <$> traverse optA es
optI (Aɴ e ix)               = Aɴ <$> optA e <*> traverse optA ix
