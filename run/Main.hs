{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}

module Main (main) where

import           A
import           Control.Monad.IO.Class    (liftIO)
import           Control.Monad.State       (StateT, evalStateT, gets)
import qualified Data.ByteString.Lazy      as BSL
import           Data.List
import qualified Data.Map                  as M
import           Data.Semigroup            ((<>))
import qualified Data.Text                 as T
import qualified Data.Text.Lazy            as TL
import           Data.Text.Lazy.Encoding   (encodeUtf8)
import           Dbg
import           Foreign.LibFFI            (callFFI, retCDouble, retInt64, retPtr)
import           Foreign.Marshal.Alloc     (free)
import           Foreign.Ptr               (Ptr)
import           Foreign.Storable          (peek)
import           Hs.A
import           L
import           Prettyprinter             (hardline, pretty, (<+>))
import           Prettyprinter.Render.Text (putDoc)
import           Sys.DL
import           System.Console.Haskeline  (Completion, CompletionFunc, InputT, defaultSettings, getInputLine, historyFile, runInputT, setComplete, simpleCompletion)
import           System.Directory          (getHomeDirectory)
import           System.FilePath           ((</>))

main :: IO ()
main = runRepl loop

data EE = Fn (E AlexPosn) | CI !AI | CF !AF

namesStr :: StateT Env IO [String]
namesStr = gets (fmap T.unpack . M.keys . ee)

data Env = Env { _lex :: AlexUserState, ee :: M.Map T.Text EE, _mf :: (Int, Int) }

type Repl a = InputT (StateT Env IO)

cyclicSimple :: [String] -> [Completion]
cyclicSimple [] = []
cyclicSimple xs = cycle $ fmap simpleCompletion xs

runRepl :: Repl a x -> IO x
runRepl x = do
    histDir <- (</> ".apple_history") <$> getHomeDirectory
    mf <- mem'
    let initSt = Env alexInitUserState M.empty mf
    let myCompleter = appleCompletions
    let settings = setComplete myCompleter $ defaultSettings { historyFile = Just histDir }
    flip evalStateT initSt $ runInputT settings x

appleCompletions :: CompletionFunc (StateT Env IO)
appleCompletions (":","")        = pure (":", cyclicSimple ["help", "h", "ty", "quit", "q", "list"])
appleCompletions ("i:", "")      = pure ("i:", cyclicSimple ["r", ""])
appleCompletions ("ri:", "")     = pure ("ri:", cyclicSimple [""])
appleCompletions ("t:", "")      = pure ("t:", cyclicSimple ["y", ""])
appleCompletions ("yt:", "")     = pure ("yt:", cyclicSimple [""])
appleCompletions ("d:", "")      = pure ("d:", [simpleCompletion "isasm"])
appleCompletions ("id:", "")     = pure ("id:", [simpleCompletion "sasm"])
appleCompletions ("sid:", "")    = pure ("sid:", [simpleCompletion "asm"])
appleCompletions ("asid:", "")   = pure ("asid:", [simpleCompletion "sm"])
appleCompletions ("sasid:", "")  = pure ("sasid:", [simpleCompletion "m"])
appleCompletions ("msasid:", "") = pure ("msasid:", [simpleCompletion ""])
appleCompletions ("a:", "")      = pure ("a:", [simpleCompletion "ms"])
appleCompletions ("sa:", "")     = pure ("sa:", [simpleCompletion "m"])
appleCompletions ("msa:", "")    = pure ("msa:", [simpleCompletion ""])
appleCompletions ("q:", "")      = pure ("q:", cyclicSimple ["uit", ""])
appleCompletions ("uq:", "")     = pure ("uq:", [simpleCompletion "it"])
appleCompletions ("iuq:", "")    = pure ("iuq:", [simpleCompletion "t"])
appleCompletions ("tiuq:", "")   = pure ("tiuq:", [simpleCompletion ""])
appleCompletions ("h:", "")      = pure ("h:", cyclicSimple ["elp", ""])
appleCompletions ("eh:", "")     = pure ("eh:", [simpleCompletion "lp"])
appleCompletions ("leh:", "")    = pure ("leh:", [simpleCompletion "p"])
appleCompletions ("pleh:", "")   = pure ("pleh:", [simpleCompletion ""])
appleCompletions (" yt:", "")    = do { ns <- namesStr ; pure (" yt:", cyclicSimple ns) }
appleCompletions (" t:", "")     = do { ns <- namesStr ; pure (" t:", cyclicSimple ns) }
appleCompletions ("", "")        = ("",) . cyclicSimple <$> namesStr
appleCompletions (rp, "")        = do { ns <- namesStr ; pure (unwords ("" : tail (words rp)), cyclicSimple (namePrefix ns rp)) }
appleCompletions _               = pure (undefined, [])

namePrefix :: [String] -> String -> [String]
namePrefix names prevRev = filter (last (words (reverse prevRev)) `isPrefixOf`) names

loop :: Repl AlexPosn ()
loop = do
    inp <- getInputLine " > "
    case words <$> inp of
        Just []            -> loop
        Just (":h":_)      -> showHelp *> loop
        Just (":help":_)   -> showHelp *> loop
        Just ("\\l":_)     -> langHelp *> loop
        Just (":ty":e)     -> tyExprR (unwords e) *> loop
        Just [":q"]        -> pure ()
        Just [":quit"]     -> pure ()
        Just (":asm":e)    -> dumpAsm (unwords e) *> loop
        Just (":ir":e)     -> irR (unwords e) *> loop
        Just (":disasm":e) -> disasm (unwords e) *> loop
        Just e             -> printExpr (unwords e) *> loop
        Nothing            -> pure ()

showHelp :: Repl AlexPosn ()
showHelp = liftIO $ putStr $ concat
    [ helpOption ":help, :h" "" "Show this help"
    , helpOption ":ty" "<expression>" "Display the type of an expression"
    , helpOption ":list" "" "List all names that are in scope"
    , helpOption ":quit, :q" "" "Quit REPL"
    , helpOption "\\l" "" "Show reference"
    -- TODO: dump debug state
    ]

langHelp :: Repl AlexPosn ()
langHelp = liftIO $ putStr $ concat
    [ lOption "Λ" "scan" "√" "sqrt"
    , lOption "⋉"  "max" "⋊"  "min"
    , lOption "⍳" "integer range" "⌊" "floor"
    , lOption "ℯ" "exp" "⨳ {m,n}" "convolve"
    , lOption "\\~" "successive application" "\\`n" "dyadic infix"
    , lOption "_." "log" "'n" "map"
    , lOption "`m n" "zip-m" "`{i,j∘[k,l]}" "rank"
    , lOption "𝒻" "range (real)" "𝜋" "pi"
    , lOption "_" "negate" ":" "size"
    ]

lOption op0 desc0 op1 desc1 =
    rightPad 14 op0 ++ rightPad 25 desc0 ++ rightPad 14 op1 ++ desc1 ++ "\n"

rightPad :: Int -> String -> String
rightPad n str = take n $ str ++ repeat ' '

helpOption :: String -> String -> String -> String
helpOption cmd args desc =
    rightPad 15 cmd ++ rightPad 14 args ++ desc ++ "\n"

ubs :: String -> BSL.ByteString
ubs = encodeUtf8 . TL.pack

disasm :: String -> Repl AlexPosn ()
disasm s = liftIO $ do
    res <- pBIO (ubs s)
    case res of
        Left err -> putDoc (pretty err <> hardline)
        Right b  -> putDoc (b <> hardline)

irR :: String -> Repl AlexPosn ()
irR s = case dumpIR (ubs s) of
    Left err -> liftIO $ putDoc (pretty err <> hardline)
    Right d  -> liftIO $ putDoc (d <> hardline)

dumpAsm :: String -> Repl AlexPosn ()
dumpAsm s = case dumpX86 (ubs s) of
    Left err -> liftIO $ putDoc (pretty err <> hardline)
    Right d  -> liftIO $ putDoc (d <> hardline)

tyExprR :: String -> Repl AlexPosn ()
tyExprR s = case tyExpr (ubs s) of
    Left err -> liftIO $ putDoc (pretty err <> hardline)
    Right t  -> liftIO $ putDoc (pretty t <> hardline)

printExpr :: String -> Repl AlexPosn ()
printExpr s = case tyParse bs of
    Left err -> liftIO $ putDoc (pretty err <> hardline)
    Right (e, _) ->
        case eAnn e of
            I -> liftIO $ do
              fp <- funP bs
              print =<< callFFI fp retInt64 []
            F -> liftIO $ do
                fp <- funP bs
                print =<< callFFI fp retCDouble []
            (Arr _ F) -> liftIO $ do
                fp <- funP bs
                p <- callFFI fp (retPtr undefined) []
                putDoc.(<>hardline).pretty =<< (peek :: Ptr AF -> IO AF) p
                free p
            (Arr _ I) -> liftIO $ do
                fp <- funP bs
                p <- callFFI fp (retPtr undefined) []
                putDoc.(<>hardline).pretty =<< (peek :: Ptr AI -> IO AI) p
                free p
            t -> liftIO $ putDoc (pretty e <+> ":" <+> pretty t <> hardline)
    where bs = ubs s
