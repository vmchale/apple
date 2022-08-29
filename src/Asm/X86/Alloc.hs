{-# LANGUAGE FlexibleContexts #-}

-- | From [Kempe compiler](http://vmchale.com/original/compiler.pdf).
module Asm.X86.Alloc ( allocFrame ) where

import           Asm.X86
import           CF
import           Control.Monad              (when)
import           Control.Monad.Extra        (concatMapM)
import           Control.Monad.State.Strict (State, gets, runState)
import           Data.Bifunctor             (second)
import           Data.Foldable              (traverse_)
import           Data.Functor               (($>))
import qualified Data.IntMap                as IM
import qualified Data.IntSet                as IS
import           Data.Maybe                 (fromMaybe, mapMaybe)
import qualified Data.Set                   as S
import           Lens.Micro                 (Lens')
import           Lens.Micro.Mtl             (modifying, (.=))

data ASt = ASt { allocs    :: IM.IntMap X86Reg
               , allocsF   :: IM.IntMap FX86Reg
               , freeI     :: S.Set X86Reg
               , freeF     :: S.Set FX86Reg
               , clobbered :: S.Set X86Reg
               , active    :: S.Set X86Reg
               , activeF   :: S.Set FX86Reg
               }

allocsLens :: Lens' ASt (IM.IntMap X86Reg)
allocsLens f s = fmap (\x -> s { allocs = x }) (f (allocs s))

allocsFLens :: Lens' ASt (IM.IntMap FX86Reg)
allocsFLens f s = fmap (\x -> s { allocsF = x }) (f (allocsF s))

freeILens :: Lens' ASt (S.Set X86Reg)
freeILens f s = fmap (\x -> s { freeI = x }) (f (freeI s))

freeFLens :: Lens' ASt (S.Set FX86Reg)
freeFLens f s = fmap (\x -> s { freeF = x }) (f (freeF s))

clobberedLens :: Lens' ASt (S.Set X86Reg)
clobberedLens f s = fmap (\x -> s { clobbered = x }) (f (clobbered s))

activeLens :: Lens' ASt (S.Set X86Reg)
activeLens f s = fmap (\x -> s { active = x }) (f (active s))

activeFLens :: Lens' ASt (S.Set FX86Reg)
activeFLens f s = fmap (\x -> s { activeF = x }) (f (activeF s))

type AM = State ASt

allFree :: ASt
allFree = ASt IM.empty IM.empty (S.fromList [Rcx .. Rax]) (S.fromList [XMM1 .. XMM15]) S.empty S.empty S.empty

runAM = second clobbered . flip runState allFree

allocFrame = uncurry frame . allocRegs

frame :: [X86 X86Reg FX86Reg ()] -> S.Set X86Reg -> [X86 X86Reg FX86Reg ()]
frame asms clob = pre++asms++post++[Ret()] where
    pre = Push () <$> clobs
    post = Pop () <$> reverse clobs
    clobs = S.toList (clob `S.intersection` S.fromList [R12 .. Rbx])

callerSave :: S.Set X86Reg
callerSave = S.fromList (Rsp : [Rcx .. R11])

allocRegs :: [X86 AbsReg FAbsReg Interval] -> ([X86 X86Reg FX86Reg ()], S.Set X86Reg)
allocRegs asms@(asm:_) = runAM (do {initUsed (ann asm); concatMapM allocReg asms})

findReg :: Int -> AM X86Reg
findReg i = gets
    (IM.findWithDefault (error ("Internal error in register allocator: unfound register " ++ show i)) i . allocs)

findFReg :: Int -> AM FX86Reg
findFReg i = gets
    (IM.findWithDefault (error ("Internal error in register allocator: unfound register " ++ show i)) i . allocsF)

assignFReg :: Int -> FX86Reg -> AM ()
assignFReg i xr =
    modifying allocsFLens (IM.insert i xr) *> modifying activeFLens (S.insert xr)

assignReg :: Int -> X86Reg -> AM ()
assignReg i xr =
    modifying allocsLens (IM.insert i xr) *> modifying activeLens (S.insert xr)

freeReg :: Int -> AM ()
freeReg i = do
    r <- gets(IM.lookup i.allocs)
    case r of
        Just ir -> modifying allocsLens (IM.delete i) *> freeIϵ ir
        Nothing -> do{fr <- findFReg i; modifying allocsFLens (IM.delete i); freeFϵ fr}
    where freeIϵ r = modifying freeILens (S.insert r) *> modifying activeLens (S.delete r)
          freeFϵ r = modifying freeFLens (S.insert r) *> modifying activeFLens (S.delete r)

fromInt :: Int -> Maybe (Either FX86Reg X86Reg)
fromInt 0  = Just (Right Rdi)
fromInt 1  = Just (Right Rsi)
fromInt 2  = Just (Right Rdx)
fromInt 3  = Just (Right Rcx)
fromInt 4  = Just (Right R8)
fromInt 5  = Just (Right R9)
fromInt 6  = Just (Right Rax)
fromInt 7 = Just (Right Rsp)
fromInt 8  = Just (Left XMM0)
fromInt 9  = Just (Left XMM1)
fromInt 10  = Just (Left XMM2)
fromInt 11 = Just (Left XMM3)
fromInt 12 = Just (Left XMM4)
fromInt 13 = Just (Left XMM5)
fromInt 14 = Just (Left XMM6)
fromInt 15 = Just (Left XMM7)
fromInt _  = Nothing

initUsed :: Interval -> AM ()
initUsed l = traverse_ (either iniF iniI) (mapMaybe fromInt (IS.toList (new l)))
    where iniI r = modifying freeILens (S.delete r)
          iniF r = modifying freeFLens (S.delete r)

freeDone :: Interval -> AM ()
freeDone l = traverse_ freeReg (IS.toList (absRs<>absFRs))
    where absRs = done l
          absFRs = fdone l

newI :: AM X86Reg
newI = do
    st <- gets freeI
    let (res, next) = c $ S.minView st
    freeILens .= next
    modifying clobberedLens (S.insert res)
    pure res
    where c = fromMaybe (error "(internal error) Register spilling not implemented.")

newF :: AM FX86Reg
newF = do
    st <- gets freeF
    let (res, next) = c $ S.minView st
    freeFLens .= next
    pure res
    where c = fromMaybe (error "(internal error) Register spilling not implemented.")

useI :: Interval -> Int -> AM X86Reg
useI l i =
    if i `IS.member` new l
        then do { res <- newI ; assignReg i res $> res }
        else findReg i

useF :: Interval -> Int -> AM FX86Reg
useF l i =
    if i `IS.member` fnew l
        then do { res <- newF ; assignFReg i res $> res }
        else findFReg i

useCArg :: X86Reg -> AbsReg -> AM X86Reg
useCArg rr r = do
    modifying freeILens (S.delete rr)
    modifying activeLens (S.insert rr)
    assignReg (toInt r) rr $> rr

useFArg :: FX86Reg -> FAbsReg -> AM FX86Reg
useFArg rr r = do
    modifying freeFLens (S.delete rr)
    modifying activeFLens (S.insert rr)
    assignFReg (fToInt r) rr $> rr

useX :: Interval -> FAbsReg -> AM FX86Reg
useX l r@FReg{} = useF l (fToInt r)
useX _ r@FArg0{} = useFArg XMM0 r
useX _ r@FArg1{} = useFArg XMM1 r
useX _ r@FArg2{} = useFArg XMM2 r
useX _ r@FArg3{} = useFArg XMM3 r
useX _ r@FArg4{} = useFArg XMM4 r
useX _ r@FArg5{} = useFArg XMM5 r
useX l r@FRet0{}  =
    let rr = XMM0 in do
        -- skip sanity check cause xmm0 is used for the first argument as well
        q <- gets freeF
        when (fToInt r `IS.member` new l && not (rr `S.member` q)) $ error "Sanity check failed."
        modifying freeFLens (S.delete rr)
        modifying activeFLens (S.insert rr)
        assignFReg (fToInt r) rr $> rr

useR :: Interval -> AbsReg -> AM X86Reg
useR l r@IReg{}  = useI l (toInt r)
useR _ r@CArg0{} = useCArg Rdi r
useR _ r@CArg1{} = useCArg Rsi r
useR _ r@CArg2{} = useCArg Rdx r
useR _ r@CArg3{} = useCArg Rcx r
useR _ r@CArg4{} = useCArg R8 r
useR _ r@CArg5{} = useCArg R9 r
useR l r@CRet{}  =
    let rr = Rax in do
        q <- gets freeI
        when (toInt r `IS.member` new l && not (rr `S.member` q)) $ error "Sanity check failed."
        modifying freeILens (S.delete rr)
        modifying activeLens (S.insert rr)
        assignReg (toInt r) rr $> rr
useR _ r@SP =
    let rr = Rsp
    in assignReg (toInt r) rr $> rr

uA :: Interval -> Addr AbsReg -> AM (Addr X86Reg)
uA l = traverse (useR l)

allocReg :: X86 AbsReg FAbsReg Interval -> AM [X86 X86Reg FX86Reg ()]
allocReg Ret{}                    = pure [Ret ()]
allocReg (Label _ l)              = pure [Label () l]
allocReg (J _ l)                  = pure [J () l]
allocReg (Je _ l)                 = pure [Je () l]
allocReg (Jne _ l)                = pure [Jne () l]
allocReg (Jg _ l)                 = pure [Jg () l]
allocReg (Jl _ l)                 = pure [Jl () l]
allocReg (Jge _ l)                = pure [Jge () l]
allocReg (Jle _ l)                = pure [Jle () l]
allocReg (MovRR l r0 r1)          = sequence [MovRR () <$> useR l r0 <*> useR l r1] <* freeDone l
allocReg (IAddRR l r0 r1)         = sequence [IAddRR () <$> useR l r0 <*> useR l r1] <* freeDone l
allocReg (IAddRI l r i)           = sequence [IAddRI () <$> useR l r <*> pure i] <* freeDone l
allocReg (ISubRR l r0 r1)         = sequence [ISubRR () <$> useR l r0 <*> useR l r1] <* freeDone l
allocReg (ISubRI l r i)           = sequence [ISubRI () <$> useR l r <*> pure i] <* freeDone l
allocReg (IMulRR l r0 r1)         = sequence [IMulRR () <$> useR l r0 <*> useR l r1] <* freeDone l
allocReg (And l r0 r1)            = sequence [And () <$> useR l r0 <*> useR l r1] <* freeDone l
allocReg (CmpRR l r0 r1)          = sequence [CmpRR () <$> useR l r0 <*> useR l r1] <* freeDone l
allocReg (MovRI l r i)            = sequence [MovRI () <$> useR l r <*> pure i] <* freeDone l
allocReg (MovqXR l r0 r1)         = sequence [MovqXR () <$> useX l r0 <*> useR l r1] <* freeDone l
allocReg (CmpRI l r i)            = sequence [CmpRI () <$> useR l r <*> pure i] <* freeDone l
allocReg (Vdivsd l r0 r1 r2)      = sequence [Vdivsd () <$> useX l r0 <*> useX l r1 <*> useX l r2] <* freeDone l
allocReg (Movapd l r0 r1)         = sequence [Movapd () <$> useX l r0 <*> useX l r1] <* freeDone l
allocReg (Vmulsd l r0 r1 r2)      = sequence [Vmulsd () <$> useX l r0 <*> useX l r1 <*> useX l r2] <* freeDone l
allocReg (Vaddsd l r0 r1 r2)      = sequence [Vaddsd () <$> useX l r0 <*> useX l r1 <*> useX l r2] <* freeDone l
allocReg (Vsubsd l r0 r1 r2)      = sequence [Vsubsd () <$> useX l r0 <*> useX l r1 <*> useX l r2] <* freeDone l
allocReg (Roundsd l r0 r1 m)      = sequence [Roundsd () <$> useX l r0 <*> useX l r1 <*> pure m] <* freeDone l
allocReg (Cvttsd2si l r0 r1)      = sequence [Cvttsd2si () <$> useR l r0 <*> useX l r1] <* freeDone l
allocReg (Cvtsi2sd l r0 r1)       = sequence [Cvtsi2sd () <$> useX l r0 <*> useR l r1] <* freeDone l
allocReg (Mulsd l r0 r1)          = sequence [Mulsd () <$> useX l r0 <*> useX l r1] <* freeDone l
allocReg (Addsd l r0 r1)          = sequence [Addsd () <$> useX l r0 <*> useX l r1] <* freeDone l
allocReg (Subsd l r0 r1)          = sequence [Subsd () <$> useX l r0 <*> useX l r1] <* freeDone l
allocReg (Divsd l r0 r1)          = sequence [Divsd () <$> useX l r0 <*> useX l r1] <* freeDone l
allocReg (MovRA l r a)            = sequence [MovRA () <$> useR l r <*> uA l a] <* freeDone l
allocReg (MovAR l a r)            = sequence [MovAR () <$> uA l a <*> useR l r] <* freeDone l
allocReg (MovAI32 l a i)          = sequence [MovAI32 () <$> uA l a <*> pure i] <* freeDone l
allocReg (MovqXA l x a)           = sequence [MovqXA () <$> useX l x <*> uA l a] <* freeDone l
allocReg (MovqAX l a x)           = sequence [MovqAX () <$> uA l a <*> useX l x] <* freeDone l
allocReg (Cmovnle l r0 r1)        = sequence [Cmovnle () <$> useR l r0 <*> useR l r1] <* freeDone l
allocReg Fldl2e{}                 = pure [Fldl2e ()]
allocReg Fldln2{}                 = pure [Fldln2 ()]
allocReg Fld1{}                   = pure [Fld1 ()]
allocReg (FldS _ st)              = pure [FldS () st]
allocReg Fprem{}                  = pure [Fprem ()]
allocReg Faddp{}                  = pure [Faddp ()]
allocReg Fscale{}                 = pure [Fscale ()]
allocReg (Fxch _ st)              = pure [Fxch () st]
allocReg (Fld l a)                = sequence [Fld () <$> uA l a] <* freeDone l
allocReg Fyl2x{}                  = pure [Fyl2x ()]
allocReg F2xm1{}                  = pure [F2xm1 ()]
allocReg Fmulp{}                  = pure [Fmulp ()]
allocReg (Fstp l a)               = sequence [Fstp () <$> uA l a] <* freeDone l
allocReg (Vfmadd231sd l r0 r1 r2) = sequence [Vfmadd231sd () <$> useX l r0 <*> useX l r1 <*> useX l r2] <* freeDone l
allocReg (Sal l r e)              = sequence [Sal () <$> useR l r <*> pure e] <* freeDone l
allocReg (Sar l r e)              = sequence [Sar () <$> useR l r <*> pure e] <* freeDone l
allocReg (Sqrtsd l r0 r1)         = sequence [Sqrtsd () <$> useX l r0 <*> useX l r1] <* freeDone l
allocReg (Minsd l r0 r1)          = sequence [Minsd () <$> useX l r0 <*> useX l r1] <* freeDone l
allocReg (Maxsd l r0 r1)          = sequence [Maxsd () <$> useX l r0 <*> useX l r1] <* freeDone l
allocReg (Vmaxsd l r0 r1 r2)      = sequence [Vmaxsd () <$> useX l r0 <*> useX l r1 <*> useX l r2] <* freeDone l
allocReg (Vminsd l r0 r1 r2)      = sequence [Vminsd () <$> useX l r0 <*> useX l r1 <*> useX l r2] <* freeDone l
allocReg (Not l r)                = sequence [Not () <$> useR l r] <* freeDone l
allocReg (Call l f)               = do
    a <- gets active
    aF <- gets activeF
    let cs = S.toList (a `S.intersection` S.delete Rdi callerSave)
        save = fmap (Push ()) cs
        restore = fmap (Pop ()) (reverse cs)
        -- TODO: save/restore aF as well
    save ++ [Call () f] ++ restore <$ freeDone l
