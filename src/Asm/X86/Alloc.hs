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
               , freeI     :: S.Set X86Reg
               , freeF     :: S.Set X86Reg
               , clobbered :: S.Set X86Reg
               , active    :: S.Set X86Reg
               }

allocsLens :: Lens' ASt (IM.IntMap X86Reg)
allocsLens f s = fmap (\x -> s { allocs = x }) (f (allocs s))

freeILens :: Lens' ASt (S.Set X86Reg)
freeILens f s = fmap (\x -> s { freeI = x }) (f (freeI s))

freeFLens :: Lens' ASt (S.Set X86Reg)
freeFLens f s = fmap (\x -> s { freeF = x }) (f (freeF s))

clobberedLens :: Lens' ASt (S.Set X86Reg)
clobberedLens f s = fmap (\x -> s { clobbered = x }) (f (clobbered s))

activeLens :: Lens' ASt (S.Set X86Reg)
activeLens f s = fmap (\x -> s { active = x }) (f (active s))

type AM = State ASt

allFree :: ASt
allFree = ASt IM.empty (S.fromList [Rcx .. Rax]) (S.fromList [XMM1 .. XMM15]) S.empty S.empty

runAM = second clobbered . flip runState allFree

allocFrame = uncurry frame . allocRegs

frame :: [X86 X86Reg ()] -> S.Set X86Reg -> [X86 X86Reg ()]
frame asms clob = pre++asms++post++[Ret()] where
    pre = Push () <$> clobs
    post = Pop () <$> reverse clobs
    clobs = S.toList (clob `S.intersection` S.fromList [R12 .. Rbx])

callerSave :: S.Set X86Reg
callerSave = S.fromList (Rsp : [Rcx .. R11] ++ [XMM1 .. XMM0])

allocRegs :: [X86 AbsReg Interval] -> ([X86 X86Reg ()], S.Set X86Reg)
allocRegs asms@(asm:_) = runAM (do {initUsed (ann asm); concatMapM allocReg asms})

findReg :: Int -> AM X86Reg
findReg i = gets
    (IM.findWithDefault (error ("Internal error in register allocator: unfound register " ++ show i)) i . allocs)

assignReg :: Int -> X86Reg -> AM ()
assignReg i xr =
    modifying allocsLens (IM.insert i xr) *> modifying activeLens (S.insert xr)

freeReg :: Int -> AM ()
freeReg i = do
    r <- findReg i
    modifying allocsLens (IM.delete i)
    case r of
        Rax -> freeIϵ r
        Rbx -> freeIϵ r
        Rcx -> freeIϵ r
        Rdx -> freeIϵ r
        Rsi -> freeIϵ r
        Rdi -> freeIϵ r
        R8  -> freeIϵ r
        R9  -> freeIϵ r
        R10 -> freeIϵ r
        R11 -> freeIϵ r
        R12 -> freeIϵ r
        R13 -> freeIϵ r
        R14 -> freeIϵ r
        R15 -> freeIϵ r
        _   -> freeFϵ r

    where freeIϵ r = modifying freeILens (S.insert r) *> modifying activeLens (S.delete r)
          freeFϵ r = modifying freeFLens (S.insert r) *> modifying activeLens (S.delete r)

fromInt :: Int -> Maybe X86Reg
fromInt 0  = Just Rdi
fromInt 1  = Just Rsi
fromInt 2  = Just Rdx
fromInt 3  = Just Rcx
fromInt 4  = Just R8
fromInt 5  = Just R9
fromInt 6  = Just Rax
fromInt 7  = Just XMM0
fromInt 8  = Just XMM1
fromInt 9  = Just XMM2
fromInt 10 = Just XMM3
fromInt 11 = Just XMM4
fromInt 12 = Just XMM5
fromInt 13 = Just XMM6
fromInt 14 = Just XMM7
fromInt 15 = Just Rsp
fromInt _  = Nothing

initUsed :: Interval -> AM ()
initUsed l = traverse_ ini (mapMaybe fromInt (IS.toList (new l)))
    where ini r =
            case r of
                Rdi  -> iniI r
                Rsi  -> iniI r
                Rdx  -> iniI r
                Rcx  -> iniI r
                R8   -> iniI r
                R9   -> iniI r
                Rax  -> iniI r
                XMM0 -> iniF r
                XMM1 -> iniF r
                XMM2 -> iniF r
                XMM3 -> iniF r
                XMM4 -> iniF r
                XMM5 -> iniF r
                XMM6 -> iniF r
                XMM7 -> iniF r
                Rsp  -> iniI r
          iniI r = modifying freeILens (S.delete r)
          iniF r = modifying freeFLens (S.delete r)

freeDone :: Interval -> AM ()
freeDone l = traverse_ freeReg (IS.toList absRs)
    where absRs = done l

newI :: AM X86Reg
newI = do
    st <- gets freeI
    let (res, next) = c $ S.minView st
    freeILens .= next
    modifying clobberedLens (S.insert res)
    pure res
    where c = fromMaybe (error "(internal error) Register spilling not implemented.")

newF :: AM X86Reg
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

useF :: Interval -> Int -> AM X86Reg
useF l i =
    if i `IS.member` new l
        then do { res <- newF ; assignReg i res $> res }
        else findReg i

useCArg :: X86Reg -> AbsReg -> AM X86Reg
useCArg rr r = do
    modifying freeILens (S.delete rr)
    modifying activeLens (S.insert rr)
    assignReg (toInt r) rr $> rr

useFArg :: X86Reg -> AbsReg -> AM X86Reg
useFArg rr r = do
    modifying freeFLens (S.delete rr)
    modifying activeLens (S.insert rr)
    assignReg (toInt r) rr $> rr

useR :: Interval -> AbsReg -> AM X86Reg
useR l r@IReg{}  = useI l (toInt r)
useR l r@FReg{}  = useF l (toInt r)
useR _ r@CArg0{} = useCArg Rdi r
useR _ r@CArg1{} = useCArg Rsi r
useR _ r@CArg2{} = useCArg Rdx r
useR _ r@CArg3{} = useCArg Rcx r
useR _ r@CArg4{} = useCArg R8 r
useR _ r@CArg5{} = useCArg R9 r
useR _ r@FArg0{} = useFArg XMM0 r
useR _ r@FArg1{} = useFArg XMM1 r
useR _ r@FArg2{} = useFArg XMM2 r
useR _ r@FArg3{} = useFArg XMM3 r
useR _ r@FArg4{} = useFArg XMM4 r
useR _ r@FArg5{} = useFArg XMM5 r
useR l r@CRet{}  =
    let rr = Rax in do
        q <- gets freeI
        when (toInt r `IS.member` new l && not (rr `S.member` q)) $ error "Sanity check failed."
        modifying freeILens (S.delete rr)
        modifying activeLens (S.insert rr)
        assignReg (toInt r) rr $> rr
useR l r@FRet0{}  =
    let rr = XMM0 in do
        -- skip sanity check cause xmm0 is used for the first argument as well
        q <- gets freeF
        when (toInt r `IS.member` new l && not (rr `S.member` q)) $ error "Sanity check failed."
        modifying freeFLens (S.delete rr)
        modifying activeLens (S.insert rr)
        assignReg (toInt r) rr $> rr
useR _ r@SP =
    let rr = Rsp
    in assignReg (toInt r) rr $> rr

uA :: Interval -> Addr AbsReg -> AM (Addr X86Reg)
uA l = traverse (useR l)

allocReg :: X86 AbsReg Interval -> AM [X86 X86Reg ()]
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
allocReg (MovqXR l r0 r1)         = sequence [MovqXR () <$> useR l r0 <*> useR l r1] <* freeDone l
allocReg (CmpRI l r i)            = sequence [CmpRI () <$> useR l r <*> pure i] <* freeDone l
allocReg (Vdivsd l r0 r1 r2)      = sequence [Vdivsd () <$> useR l r0 <*> useR l r1 <*> useR l r2] <* freeDone l
allocReg (Movapd l r0 r1)         = sequence [Movapd () <$> useR l r0 <*> useR l r1] <* freeDone l
allocReg (Vmulsd l r0 r1 r2)      = sequence [Vmulsd () <$> useR l r0 <*> useR l r1 <*> useR l r2] <* freeDone l
allocReg (Vaddsd l r0 r1 r2)      = sequence [Vaddsd () <$> useR l r0 <*> useR l r1 <*> useR l r2] <* freeDone l
allocReg (Vsubsd l r0 r1 r2)      = sequence [Vsubsd () <$> useR l r0 <*> useR l r1 <*> useR l r2] <* freeDone l
allocReg (Roundsd l r0 r1 m)      = sequence [Roundsd () <$> useR l r0 <*> useR l r1 <*> pure m] <* freeDone l
allocReg (Cvttsd2si l r0 r1)      = sequence [Cvttsd2si () <$> useR l r0 <*> useR l r1] <* freeDone l
allocReg (Cvtsi2sd l r0 r1)       = sequence [Cvtsi2sd () <$> useR l r0 <*> useR l r1] <* freeDone l
allocReg (Mulsd l r0 r1)          = sequence [Mulsd () <$> useR l r0 <*> useR l r1] <* freeDone l
allocReg (Addsd l r0 r1)          = sequence [Addsd () <$> useR l r0 <*> useR l r1] <* freeDone l
allocReg (Subsd l r0 r1)          = sequence [Subsd () <$> useR l r0 <*> useR l r1] <* freeDone l
allocReg (Divsd l r0 r1)          = sequence [Divsd () <$> useR l r0 <*> useR l r1] <* freeDone l
allocReg (MovRA l r a)            = sequence [MovRA () <$> useR l r <*> uA l a] <* freeDone l
allocReg (MovAR l a r)            = sequence [MovAR () <$> uA l a <*> useR l r] <* freeDone l
allocReg (MovAI32 l a i)          = sequence [MovAI32 () <$> uA l a <*> pure i] <* freeDone l
allocReg (MovqXA l x a)           = sequence [MovqXA () <$> useR l x <*> uA l a] <* freeDone l
allocReg (MovqAX l a x)           = sequence [MovqAX () <$> uA l a <*> useR l x] <* freeDone l
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
allocReg (Vfmadd231sd l r0 r1 r2) = sequence [Vfmadd231sd () <$> useR l r0 <*> useR l r1 <*> useR l r2] <* freeDone l
allocReg (Sal l r e)              = sequence [Sal () <$> useR l r <*> pure e] <* freeDone l
allocReg (Sar l r e)              = sequence [Sar () <$> useR l r <*> pure e] <* freeDone l
allocReg (Sqrtsd l r0 r1)         = sequence [Sqrtsd () <$> useR l r0 <*> useR l r1] <* freeDone l
allocReg (Minsd l r0 r1)          = sequence [Minsd () <$> useR l r0 <*> useR l r1] <* freeDone l
allocReg (Maxsd l r0 r1)          = sequence [Maxsd () <$> useR l r0 <*> useR l r1] <* freeDone l
allocReg (Vmaxsd l r0 r1 r2)      = sequence [Vmaxsd () <$> useR l r0 <*> useR l r1 <*> useR l r2] <* freeDone l
allocReg (Vminsd l r0 r1 r2)      = sequence [Vminsd () <$> useR l r0 <*> useR l r1 <*> useR l r2] <* freeDone l
allocReg (Not l r)                = sequence [Not () <$> useR l r] <* freeDone l
allocReg (Call l f)               = do
    a <- gets active
    let cs = S.toList (a `S.intersection` S.delete Rdi callerSave)
        save = fmap (Push ()) cs
        restore = fmap (Pop ()) (reverse cs)
    save ++ [Call () f] ++ restore <$ freeDone l
