module Asm.X86.Color ( x86Init
                     ) where

import           Asm.X86
import qualified Data.IntMap as IM
import qualified Data.Map    as M

type Count = Int

-- TODO: collect information on which nodes are related to moves
movIx :: [X86 AbsReg ()] -> IM.IntMap Count
movIx = undefined

type Coloring = M.Map AbsReg X86Reg

x86Init = M.fromList [ (CArg0, Rdi), (CArg1, Rsi), (CArg2, Rdx), (CArg3, Rcx), (CArg4, R9), (CArg5, R9)
                     , (FArg0, XMM0), (FArg1, XMM1), (FArg2, XMM2), (FArg3, XMM3), (FArg4, XMM4), (FArg5, XMM5)
                     , (CRet, Rax), (FRet0, XMM0), (FRet1, XMM1), (SP, Rsp)
                     ]
