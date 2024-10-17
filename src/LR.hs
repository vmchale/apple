-- Based on Appel
--
-- live ranges
module LR ( reconstruct, reconstructFlat ) where

import           CF                  hiding (done, liveness)
import           Control.Composition (thread)
import           Data.Copointed
-- this seems to be faster
import qualified Data.IntMap.Lazy    as IM
import qualified Data.IntSet         as IS

emptyLiveness :: Liveness
emptyLiveness = Liveness IS.empty IS.empty IS.empty IS.empty

initLiveness :: Copointed p => [p ControlAnn] -> LivenessMap
initLiveness = IM.fromList . fmap (\asm -> let x = copoint asm in (node x, (x, emptyLiveness)))

type LivenessMap = IM.IntMap (ControlAnn, Liveness)

succNode :: ControlAnn -- ^ 'ControlAnn' associated w/ node @n@
         -> LivenessMap
         -> [Liveness] -- ^ 'Liveness' associated with 'succNode' @n@
succNode x ns =
    let conns = conn x
        in fmap (snd . flip lookupNode ns) conns

lookupNode :: Int -> LivenessMap -> (ControlAnn, Liveness)
lookupNode = IM.findWithDefault (error "Internal error: failed to look up instruction")

done :: LivenessMap -> LivenessMap -> Bool
done n0 n1 = {-# SCC "done" #-} and $ zipWith (\(_, l) (_, l') -> l == l') (IM.elems n0) (IM.elems n1) -- n0, n1 have same length

-- order in which to inspect nodes during liveness analysis
inspectOrder :: Copointed p => [p ControlAnn] -> [Int]
inspectOrder = fmap (node . copoint) -- don't need to reverse because thread goes in opposite order

reconstructFlat isns = let is=inspectOrder isns in reconstruct is (mkLiveness is isns) isns

reconstruct :: (Copointed p) => [Int] -> LivenessMap -> [p ControlAnn] -> [p NLiveness]
reconstruct is li asms = {-# SCC "reconstructL" #-} fmap (fmap lookupL) asms
    where l = liveness is li
          lookupL x = let ni = node x in NLiveness ni (snd $ lookupNode ni l)

{-# SCC mkLiveness #-}
mkLiveness :: Copointed p => [Int] -> [p ControlAnn] -> LivenessMap
mkLiveness is asms = liveness is (initLiveness asms)

liveness :: [Int] -> LivenessMap -> LivenessMap
liveness is nSt =
    if done nSt nSt'
        then nSt
        else liveness is nSt'
    where nSt' = {-# SCC "iterNodes" #-} iterNodes is nSt

iterNodes :: [Int] -> LivenessMap -> LivenessMap
iterNodes is = thread (fmap stepNode is)

stepNode :: Int -> LivenessMap -> LivenessMap
stepNode n ns = {-# SCC "stepNode" #-} IM.insert n (c, Liveness ins' out' fins' fout') ns
    where (c, l) = lookupNode n ns; u = ud c
          ins' = usesNode u <> (out l IS.\\ defsNode u)
          fins' = usesFNode u <> (fout l IS.\\ defsFNode u)
          out' = IS.unions (fmap ins (succNode c ns))
          fout' = IS.unions (fmap fins (succNode c ns))
