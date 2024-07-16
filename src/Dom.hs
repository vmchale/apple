module Dom ( domG ) where

import           Control.Monad.State.Strict (State, gets, modify, state)
import qualified Data.Map                   as M
import           Data.Tuple.Extra           (second3, snd3, third3)
import           IR

type LabelM = State (Int, M.Map Label Int, M.Map Label [Int])

next :: LabelM Int
next = state (\(i,m0,m1) -> (i,(i+1,m0,m1)))

ll :: Label -> LabelM Int
ll l = gets (M.findWithDefault (error "Internal error in control-flow graph: node label not in map.") l . snd3)

data NL = NL { nL :: !Int, preds :: [Int] }

domG :: [Stmt] -> [(Stmt, NL)]
domG (s:ns) = undefined

b3 :: Int -> Label -> LabelM ()
b3 i l = modify (third3 (M.alter (\k -> Just$case k of {Nothing -> [i]; Just is -> i:is}) l))

br :: Int -> Label -> LabelM ()
br i l = modify (second3 (M.insert l i))

brs :: [Stmt] -> LabelM ()
brs [] = pure []
brs (s@(C l):s'@(L retL):ss) = do
    i <- next
    br i retL; b3 i l
    brs ss
brs (s@(L l):ss) = do
    i <- next
    br i l
    brs ss
brs (_:ss) = brs ss
