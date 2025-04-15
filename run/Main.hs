module Main (main) where

import           Control.Monad.Trans.State.Strict (StateT, evalStateT, gets)
import           Data.List                        (isPrefixOf, isSuffixOf)
import qualified Data.Text                        as T
import           Nm
import           REPL
import           System.Console.Haskeline         (Completion, CompletionFunc, completeFilename, defaultSettings, fallbackCompletion, historyFile, runInputT, setComplete,
                                                   simpleCompletion)
import           System.Directory                 (getHomeDirectory)
import           System.FilePath                  ((</>))
import           System.IO                        (stdout)

main :: IO ()
main = runRepl loop

namesStr :: StateT Env IO [String]
namesStr = gets ((++bn) . fmap (T.unpack.name.fst) . ee)

bn :: [String]
bn = ["frange", "itof", "gen.", "di.", "sin.", "cos.", "rand.", "cyc.", "odd.", "even.", "abs.", "ug.", "take#", "drop#"]

runRepl :: Repl x -> IO x
runRepl x = do
    histDir <- (</> ".apple_history") <$> getHomeDirectory
    st <- iSt stdout
    let myCompleter = appleCompletions `fallbackCompletion` completeFilename
    let settings = setComplete myCompleter $ defaultSettings { historyFile = Just histDir }
    flip evalStateT st $ runInputT settings x

appleCompletions :: CompletionFunc (StateT Env IO)
appleCompletions (":","")         = pure (":", cyclicSimple ["help", "h", "ty", "quit", "q", "quickcheck", "qc", "list", "ann", "bench", "y", "yank", "st", "store"])
appleCompletions ("i:", "")       = pure ("i:", cyclicSimple ["r", "nspect", ""])
appleCompletions ("ri:", "")      = pure ("ri:", cyclicSimple [""])
appleCompletions ("c:", "")       = pure ("c:", cyclicSimple ["mm", "ompile"])
appleCompletions ("mc:", "")      = pure ("mc:", cyclicSimple ["m"])
appleCompletions ("mmc:", "")     = pure ("mmc:", cyclicSimple [""])
appleCompletions ("ni:", "")      = pure ("ni:", [simpleCompletion "spect"])
appleCompletions ("sni:", "")     = pure ("sni:", [simpleCompletion "pect"])
appleCompletions ("psni:", "")    = pure ("psni:", [simpleCompletion "ect"])
appleCompletions ("epsni:", "")   = pure ("epsni:", [simpleCompletion "ct"])
appleCompletions ("cepsni:", "")  = pure ("cepsni:", [simpleCompletion "t"])
appleCompletions ("tcepsni:", "") = pure ("tcepsni:", [simpleCompletion ""])
appleCompletions ("t:", "")       = pure ("t:", cyclicSimple ["y"])
appleCompletions ("b:", "")       = pure ("b:", cyclicSimple ["ench", ""])
appleCompletions ("eb:", "")      = pure ("eb:", [simpleCompletion "nch"])
appleCompletions ("neb:", "")     = pure ("neb:", [simpleCompletion "ch"])
appleCompletions ("cneb:", "")    = pure ("cneb:", [simpleCompletion "h"])
appleCompletions ("hcneb:", "")   = pure ("hcneb:", [simpleCompletion ""])
appleCompletions ("oc:", "")      = pure ("oc:", cyclicSimple ["mpile"])
appleCompletions ("moc:", "")     = pure ("moc:", cyclicSimple ["pile"])
appleCompletions ("pmoc:", "")    = pure ("pmoc:", cyclicSimple ["ile"])
appleCompletions ("ipmoc:", "")   = pure ("ipmoc:", cyclicSimple ["le"])
appleCompletions ("lipmoc:", "")  = pure ("lipmoc:", cyclicSimple ["e"])
appleCompletions ("elipmoc:", "") = pure ("elipmoc:", cyclicSimple [""])
appleCompletions ("yt:", "")      = pure ("yt:", cyclicSimple [""])
appleCompletions ("y:", "")       = pure ("y:", cyclicSimple ["ank", ""])
appleCompletions ("ay:", "")      = pure ("ay:", cyclicSimple ["nk"])
appleCompletions ("nay:", "")     = pure ("nay:", cyclicSimple ["k"])
appleCompletions ("knay:", "")    = pure ("knay:", cyclicSimple [""])
appleCompletions ("s:", "")       = pure ("s:", cyclicSimple ["tore", "t"])
appleCompletions ("ts:", "")      = pure ("ts:", cyclicSimple ["ore", ""])
appleCompletions ("ots:", "")     = pure ("ots:", cyclicSimple ["re"])
appleCompletions ("rots:", "")    = pure ("rots:", cyclicSimple ["e"])
appleCompletions ("erots:", "")   = pure ("erots:", cyclicSimple [""])
appleCompletions ("d:", "")       = pure ("d:", [simpleCompletion "isasm", simpleCompletion "elete"])
appleCompletions ("id:", "")      = pure ("id:", [simpleCompletion "sasm"])
appleCompletions ("sid:", "")     = pure ("sid:", [simpleCompletion "asm"])
appleCompletions ("asid:", "")    = pure ("asid:", [simpleCompletion "sm"])
appleCompletions ("sasid:", "")   = pure ("sasid:", [simpleCompletion "m"])
appleCompletions ("msasid:", "")  = pure ("msasid:", [simpleCompletion ""])
appleCompletions ("a:", "")       = pure ("a:", [simpleCompletion "sm", simpleCompletion "nn"])
appleCompletions ("sa:", "")      = pure ("sa:", [simpleCompletion "m"])
appleCompletions ("msa:", "")     = pure ("msa:", [simpleCompletion ""])
appleCompletions ("na:", "")      = pure ("na:", [simpleCompletion "n"])
appleCompletions ("nna:", "")     = pure ("nna:", [simpleCompletion ""])
appleCompletions ("l:", "")       = pure ("l:", cyclicSimple ["ist"])
appleCompletions ("il:", "")      = pure ("il:", cyclicSimple ["st"])
appleCompletions ("sil:", "")     = pure ("sil:", cyclicSimple ["t"])
appleCompletions ("tsil:", "")    = pure ("tsil:", cyclicSimple [])
appleCompletions ("q:", "")       = pure ("q:", cyclicSimple ["uit", "c", ""])
appleCompletions ("cq:", "")      = pure ("cq:", [simpleCompletion ""])
appleCompletions ("uq:", "")      = pure ("uq:", [simpleCompletion "it"])
appleCompletions ("iuq:", "")     = pure ("iuq:", [simpleCompletion "t"])
appleCompletions ("tiuq:", "")    = pure ("tiuq:", [simpleCompletion ""])
appleCompletions ("h:", "")       = pure ("h:", cyclicSimple ["elp", ""])
appleCompletions ("eh:", "")      = pure ("eh:", [simpleCompletion "lp"])
appleCompletions ("leh:", "")     = pure ("leh:", [simpleCompletion "p"])
appleCompletions ("pleh:", "")    = pure ("pleh:", [simpleCompletion ""])
appleCompletions ("ed:", "")      = pure ("ed:", [simpleCompletion "lete"])
appleCompletions ("led:", "")     = pure ("led:", [simpleCompletion "ete"])
appleCompletions ("eled:", "")    = pure ("eled:", [simpleCompletion "te"])
appleCompletions ("teled:", "")   = pure ("teled:", [simpleCompletion "e"])
appleCompletions ("eteled:", "")  = pure ("eteled:", [simpleCompletion ""])
appleCompletions (" eteled:", "") = do {ns <- namesStr; pure (" eteled:", cyclicSimple ns)}
appleCompletions (" yt:", "")     = do {ns <- namesStr; pure (" yt:", cyclicSimple ns)}
appleCompletions (" t:", "")      = do {ns <- namesStr; pure (" t:", cyclicSimple ns)}
appleCompletions ("", "")         = ("",) . cyclicSimple <$> namesStr
appleCompletions (rp, "")         | "y:" `isSuffixOf` rp = pure (rp, [])
appleCompletions (rp, "")         = do {ns <- namesStr; pure (unwords ("" : tail (words rp)), cyclicSimple (namePrefix ns rp))}
appleCompletions _                = pure (undefined, [])

cyclicSimple :: [String] -> [Completion]
cyclicSimple = fmap simpleCompletion

namePrefix :: [String] -> String -> [String]
namePrefix names prevRev = filter (last (words (reverse prevRev)) `isPrefixOf`) names
