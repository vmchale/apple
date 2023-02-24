{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}

module Main (main) where

import           A
import           Control.Monad.IO.Class    (liftIO)
import           Control.Monad.State       (StateT, evalStateT, gets, modify)
import           Control.Monad.Trans.Class (lift)
import qualified Data.ByteString.Lazy      as BSL
import           Data.Int                  (Int64)
import           Data.List
import           Data.Semigroup            ((<>))
import qualified Data.Text                 as T
import qualified Data.Text.IO              as TIO
import qualified Data.Text.Lazy            as TL
import           Data.Text.Lazy.Encoding   (encodeUtf8)
import           Dbg
import           Foreign.LibFFI            (callFFI, retCDouble, retInt64, retPtr, retWord8)
import           Foreign.Marshal.Alloc     (free)
import           Foreign.Ptr               (Ptr, castPtr)
import           Foreign.Storable          (peek)
import           Hs.A
import           Hs.FFI
import           L
import           Name
import           Prettyprinter             (hardline, pretty, (<+>))
import           Prettyprinter.Render.Text (putDoc)
import           Sys.DL
import           System.Console.Haskeline  (Completion, CompletionFunc, InputT, completeFilename, defaultSettings, fallbackCompletion, getInputLine, historyFile, runInputT,
                                            setComplete, simpleCompletion)
import           System.Directory          (getHomeDirectory)
import           System.FilePath           ((</>))
import           Ty

main :: IO ()
main = runRepl loop

namesStr :: StateT Env IO [String]
namesStr = gets (fmap (T.unpack.name.fst) . ee)

data Env = Env { _lex :: AlexUserState, ee :: [(Name AlexPosn, E AlexPosn)], mf :: (Int, Int) }

aEe :: Name AlexPosn -> E AlexPosn -> Env -> Env
aEe n e (Env l ees mm) = Env l ((n,e):ees) mm

setL :: AlexUserState -> Env -> Env
setL lSt (Env _ ees mm) = Env lSt ees mm

type Repl a = InputT (StateT Env IO)

cyclicSimple :: [String] -> [Completion]
cyclicSimple = fmap simpleCompletion

runRepl :: Repl a x -> IO x
runRepl x = do
    histDir <- (</> ".apple_history") <$> getHomeDirectory
    mfϵ <- mem'
    let initSt = Env alexInitUserState [] mfϵ
    let myCompleter = appleCompletions `fallbackCompletion` completeFilename
    let settings = setComplete myCompleter $ defaultSettings { historyFile = Just histDir }
    flip evalStateT initSt $ runInputT settings x

appleCompletions :: CompletionFunc (StateT Env IO)
appleCompletions (":","")         = pure (":", cyclicSimple ["help", "h", "ty", "quit", "q", "list", "ann", "y", "yank"])
appleCompletions ("i:", "")       = pure ("i:", cyclicSimple ["r", "nspect", ""])
appleCompletions ("ri:", "")      = pure ("ri:", cyclicSimple [""])
appleCompletions ("ni:", "")      = pure ("ni:", [simpleCompletion "spect"])
appleCompletions ("sni:", "")     = pure ("sni:", [simpleCompletion "pect"])
appleCompletions ("psni:", "")    = pure ("psni:", [simpleCompletion "ect"])
appleCompletions ("epsni:", "")   = pure ("epsni:", [simpleCompletion "ct"])
appleCompletions ("cepsni:", "")  = pure ("cepsni:", [simpleCompletion "t"])
appleCompletions ("tcepsni:", "") = pure ("tcepsni:", [simpleCompletion ""])
appleCompletions ("t:", "")       = pure ("t:", cyclicSimple ["y"])
appleCompletions ("yt:", "")      = pure ("yt:", cyclicSimple [""])
appleCompletions ("y:", "")       = pure ("y:", cyclicSimple ["ank", ""])
appleCompletions ("ay:", "")      = pure ("ay:", cyclicSimple ["nk"])
appleCompletions ("nay:", "")     = pure ("nay:", cyclicSimple ["k"])
appleCompletions ("knay:", "")    = pure ("knay:", cyclicSimple [""])
appleCompletions ("d:", "")       = pure ("d:", [simpleCompletion "isasm"])
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
appleCompletions ("q:", "")       = pure ("q:", cyclicSimple ["uit", ""])
appleCompletions ("uq:", "")      = pure ("uq:", [simpleCompletion "it"])
appleCompletions ("iuq:", "")     = pure ("iuq:", [simpleCompletion "t"])
appleCompletions ("tiuq:", "")    = pure ("tiuq:", [simpleCompletion ""])
appleCompletions ("h:", "")       = pure ("h:", cyclicSimple ["elp", ""])
appleCompletions ("eh:", "")      = pure ("eh:", [simpleCompletion "lp"])
appleCompletions ("leh:", "")     = pure ("leh:", [simpleCompletion "p"])
appleCompletions ("pleh:", "")    = pure ("pleh:", [simpleCompletion ""])
appleCompletions (" yt:", "")     = do { ns <- namesStr ; pure (" yt:", cyclicSimple ns) }
appleCompletions (" t:", "")      = do { ns <- namesStr ; pure (" t:", cyclicSimple ns) }
appleCompletions ("", "")         = ("",) . cyclicSimple <$> namesStr
appleCompletions (rp, "")         = do { ns <- namesStr ; pure (unwords ("" : tail (words rp)), cyclicSimple (namePrefix ns rp)) }
appleCompletions _                = pure (undefined, [])

namePrefix :: [String] -> String -> [String]
namePrefix names prevRev = filter (last (words (reverse prevRev)) `isPrefixOf`) names

loop :: Repl AlexPosn ()
loop = do
    inp <- getInputLine " > "
    case words <$> inp of
        Just []               -> loop
        Just (":h":_)         -> showHelp *> loop
        Just (":help":_)      -> showHelp *> loop
        Just ("\\l":_)        -> langHelp *> loop
        Just (":ty":e)        -> tyExprR (unwords e) *> loop
        Just [":q"]           -> pure ()
        Just [":quit"]        -> pure ()
        Just (":asm":e)       -> dumpAsmG (unwords e) *> loop
        Just (":ann":e)       -> annR (unwords e) *> loop
        Just (":ir":e)        -> irR (unwords e) *> loop
        Just (":disasm":e)    -> disasm (unwords e) *> loop
        Just (":inspect":e)   -> inspect (unwords e) *> loop
        Just (":yank":f:[fp]) -> iCtx f fp *> loop
        Just (":y":f:[fp])    -> iCtx f fp *> loop
        Just e                -> printExpr (unwords e) *> loop
        Nothing               -> pure ()

showHelp :: Repl AlexPosn ()
showHelp = liftIO $ putStr $ concat
    [ helpOption ":help, :h" "" "Show this help"
    , helpOption ":ty" "<expression>" "Display the type of an expression"
    , helpOption ":ann" "<expression>" "Annotate with types"
    , helpOption ":list" "" "List all names that are in scope"
    , helpOption ":quit, :q" "" "Quit REPL"
    , helpOption ":yank, :y" "<fn> <file>" "Read file"
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
    , lOption "`" "zip" "`{i,j∘[k,l]}" "rank"
    , lOption "𝒻" "range (real)" "𝜋" "pi"
    , lOption "_" "negate" ":" "size"
    , lOption "𝓉" "dimension" "}.?" "last"
    , lOption "->n" "select" "**" "power"
    , lOption "gen." "generate" "𝓕" "fibonacci"
    , lOption "re:" "repeat" "}." "typesafe last"
    , lOption "⊲" "cons" "⊳" "snoc"
    , lOption "^:" "iterate" "%." "matmul"
    , lOption "⊗" "outer product" "|:" "transpose"
    , lOption "{.?" "head" "{." "typesafe head"
    , lOption "}.?" "last" "}:" "typesafe init"
    , lOption "⟨z,w⟩" "array literal" "?p,.e1,.e2" "conditional"
    , lOption "/*" "fold all" "ℝ" "i->f conversion"
    , lOption "⧺" "cat" "{:" "typesafe tail"
    , lOption "⊖" "rotate" "sin." "sine"
    , lOption "𝔯" "rand" "⍳" "range (int)"
    , lOption "/ₒ" "fold with seed" "Λₒ" "scan with seed"
    , lOption "{x←y;z}" "let...in" "⊙" "cycle"
    , lOption "˙" "at" "|" "rem"
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
disasm s = do
    st <- lift $ gets _lex
    liftIO $ do
        res <- dtxt st (ubs s)
        case res of
            Left err -> putDoc (pretty err <> hardline)
            Right b  -> TIO.putStr b

irR :: String -> Repl AlexPosn ()
irR s = do
    st <- lift $ gets _lex
    case rwP st (ubs s) of
        Left err -> liftIO $ putDoc (pretty err <> hardline)
        Right (eP, i) -> do
            eC <- eRepl eP
            liftIO $ case eDumpIR i eC of
                Left err -> putDoc (pretty err <> hardline)
                Right d  -> putDoc (d <> hardline)

dumpAsmG :: String -> Repl AlexPosn ()
dumpAsmG s = do
    st <- lift $ gets _lex
    case rwP st (ubs s) of
        Left err -> liftIO $ putDoc (pretty err <> hardline)
        Right (eP, i) -> do
            eC <- eRepl eP
            liftIO $ case eDumpX86 i eC of
                Left err -> putDoc (pretty err <> hardline)
                Right d  -> putDoc (d <> hardline)

tyExprR :: String -> Repl AlexPosn ()
tyExprR s = do
    st <- lift $ gets _lex
    case rwP st (ubs s) of
        Left err -> liftIO $ putDoc (pretty err <> hardline)
        Right (eP, i) -> do
            eC <- eRepl eP
            case tyClosed i eC of
                Left err      -> liftIO $ putDoc (pretty err <> hardline)
                Right (e,c,_) -> liftIO $ putDoc (prettyC (eAnn e, c) <> hardline)

annR :: String -> Repl AlexPosn ()
annR s = do
    st <- lift $ gets _lex
    liftIO $ case tyParseCtx st $ ubs s of
        Left err    -> putDoc (pretty err<>hardline)
        Right (e,_) -> putDoc (prettyTyped e<>hardline)

inspect :: String -> Repl AlexPosn ()
inspect s = do
    st <- lift $ gets _lex
    case rwP st bs of
        Left err -> liftIO $ putDoc (pretty err <> hardline)
        Right (eP, i) -> do
            eC <- eRepl eP
            case tyClosed i eC of
                Left err -> liftIO $ putDoc (pretty err <> hardline)
                Right (e, _, i') -> do
                    let dbgPrint =
                            case eAnn e of
                                (Arr _ (P [F,F])) -> \p -> (dbgAB :: Ptr (Apple (Pp Double Double)) -> IO T.Text) (castPtr p)
                                (Arr _ F)         -> \p -> (dbgAB :: Ptr (Apple Double) -> IO T.Text) (castPtr p)
                                (Arr _ I)         -> \p -> (dbgAB :: Ptr (Apple Int64) -> IO T.Text) (castPtr p)
                    m <- lift $ gets mf
                    liftIO $ do
                        (sz, fp) <- eFunP i' m e
                        p <- callFFI fp (retPtr undefined) []
                        TIO.putStrLn =<< dbgPrint p
                        free p *> freeFunPtr sz fp
        where bs = ubs s

iCtx :: String -> String -> Repl AlexPosn ()
iCtx f fp = do
    st <- lift $ gets _lex
    bs <- liftIO $ BSL.readFile fp
    case tyParseCtx st bs of
        Left err -> liftIO $ putDoc (pretty err <> hardline)
        Right (_,i) ->
            let (st', n) = newIdent (AlexPn 0 0 0) (T.pack f) (setM i st)
                x' = parseE st' bs
            in lift $ do {modify (aEe n x'); modify (setL st')}
    where setM i' (_, mm, im) = (i', mm, im)

printExpr :: String -> Repl AlexPosn ()
printExpr s = do
    st <- lift $ gets _lex
    case rwP st bs of
        Left err -> liftIO $ putDoc (pretty err <> hardline)
        Right (eP, i) -> do
            eC <- eRepl eP
            case tyClosed i eC of
                Left err -> liftIO $ putDoc (pretty err <> hardline)
                Right (e, _, _) -> do
                    m <- lift $ gets mf
                    case eAnn e of
                        I ->
                          liftIO $ do
                              (sz, fp) <- eFunP i m eC
                              print =<< callFFI fp retInt64 []
                              freeFunPtr sz fp
                        F ->
                            liftIO $ do
                                (sz, fp) <- eFunP i m eC
                                print =<< callFFI fp retCDouble []
                                freeFunPtr sz fp
                        (Arr _ F) ->
                            liftIO $ do
                                (sz, fp) <- eFunP i m eC
                                p <- callFFI fp (retPtr undefined) []
                                putDoc.(<>hardline).pretty =<< (peek :: Ptr AF -> IO AF) p
                                free p *> freeFunPtr sz fp
                        (Arr _ I) ->
                            liftIO $ do
                                (sz, fp) <- eFunP i m eC
                                p <- callFFI fp (retPtr undefined) []
                                putDoc.(<>hardline).pretty =<< (peek :: Ptr AI -> IO AI) p
                                free p *> freeFunPtr sz fp
                        B ->
                            liftIO $ do
                                (sz, fp) <- eFunP i m eC
                                cb <- callFFI fp retWord8 []
                                putStrLn (sB cb)
                                freeFunPtr sz fp
                            where sB 1 = "#t"; sB 0 = "#f"
                        (Arr _ (P [F,F])) ->
                            liftIO $ do
                                (sz, fp) <- eFunP i m eC
                                p <- callFFI fp (retPtr undefined) []
                                putDoc.(<>hardline).pretty =<< (peek :: Ptr (Apple (Pp Double Double)) -> IO (Apple (Pp Double Double))) p
                                free p *> freeFunPtr sz fp
                        (Arr _ (P [I,I])) ->
                            liftIO $ do
                                (sz, fp) <- eFunP i m eC
                                p <- callFFI fp (retPtr undefined) []
                                putDoc.(<>hardline).pretty =<< (peek :: Ptr (Apple (Pp Int64 Int64)) -> IO (Apple (Pp Int64 Int64))) p
                                free p *> freeFunPtr sz fp
                        t -> liftIO $ putDoc (pretty e <+> ":" <+> pretty t <> hardline)
    where bs = ubs s

parseE st bs = fst . either (error "Internal error?") id $ rwP st bs

eRepl :: E AlexPosn -> Repl AlexPosn (E AlexPosn)
eRepl e = do { ees <- lift $ gets ee; pure $ foldLet ees e }
    where foldLet = thread . fmap (\b@(_,eϵ) -> Let (eAnn eϵ) b) where thread = foldr (.) id
