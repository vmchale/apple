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
import           Q

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

-- TODO: depth-first sort?
-- dff

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
liveness is l =
    let initWl = IS.fromList is
        preds = mkPred (fst<$>IM.elems l)
    in snd$stepWl preds initWl l

type Preds = IM.IntMap [Int]

mkPred :: [ControlAnn] -> Preds
mkPred cf = thread (g<$>cf) IM.empty where g (ControlAnn n cs _) = thread [ c!:n | c <- cs ]

-- § 17.4 Appel

stepWl :: Preds -> IS.IntSet -> LivenessMap -> (IS.IntSet, LivenessMap)
stepWl preds is l =
    case IS.maxView is of
        Nothing -> (IS.empty, l)
        Just (i,isϵ) ->
            let (w,l') = stepN preds i l
                in stepWl preds (w<>isϵ) l'

-- out-sets will only change if in-sets of predecessor change
-- in-sets will only change if out-set changes

stepN :: Preds -> Int -> LivenessMap -> (IS.IntSet, LivenessMap)
stepN preds n ns = (iSelf wn, IM.insert n (c, Liveness ins' out' fins' fout') ns) where
    (c,l) = lookupNode n ns; (UD u uf d df) = ud c
    ins' = u <> (out l IS.\\ d); fins' = uf <> (fout l IS.\\ df)
    out' = ins @<> succL; fout' = fins @<> succL
    succL = succNode c ns
    wn | ins'/=ins l || fins'/=fins l = IS.fromList$IM.findWithDefault [] n preds
       | otherwise = IS.empty
    iSelf = if out'/=out l || fout'/=fout l then IS.insert n else id
