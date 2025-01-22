module REPL (Env (..), Repl, loop, iSt) where

import           A
import           Control.Monad                    (zipWithM, zipWithM_)
import           Control.Monad.IO.Class           (liftIO)
import           Control.Monad.Trans.Class        (lift)
import           Control.Monad.Trans.State.Strict (StateT, gets, modify)
import           Criterion                        (benchmark, nfIO)
import qualified Data.ByteString.Lazy             as BSL
import           Data.Foldable                    (traverse_)
import           Data.Functor                     ((<&>))
import           Data.Int                         (Int64)
import           Data.List                        (scanl')
import           Data.List.Split                  (chunksOf)
import           Data.Maybe                       (catMaybes)
import qualified Data.Text                        as T
import qualified Data.Text.IO                     as TIO
import qualified Data.Text.Lazy                   as TL
import           Data.Text.Lazy.Builder           (toLazyTextWith)
import           Data.Text.Lazy.Builder.Int       (hexadecimal)
import           Data.Text.Lazy.Encoding          (encodeUtf8)
import qualified Data.Text.Lazy.IO                as TLIO
import           Data.Traversable                 (forM)
import           Data.Word                        (Word8)
import           Dbg
import           Foreign.LibFFI                   (callFFI, retCDouble, retCUChar, retInt64, retPtr, retWord8)
import           Foreign.Marshal.Alloc            (free)
import           Foreign.Marshal.Array            (peekArray)
import           Foreign.Ptr                      (Ptr, castPtr, plusPtr)
import           Foreign.Storable                 (peek)
import           Hs.A
import           Hs.FFI
import           L
import           Nm
import           Prettyprinter                    (Doc, align, brackets, concatWith, hardline, list, pretty, space, tupled, (<+>))
import           Prettyprinter.Ext
import           Prettyprinter.Render.Text        (putDoc)
import           QC
import           Sys.DL
import           System.Console.Haskeline         (InputT, getInputLine)
import           System.Directory                 (doesFileExist)
import           System.Info                      (arch)
import           Ty
import           Ty.M

data Arch = X64 | AArch64 !MCtx

data Env = Env { _lex :: !AlexUserState, ee :: [(Nm AlexPosn, E AlexPosn)], mf :: CCtx, _arch :: Arch }

lg=lift . gets

aEe :: Nm AlexPosn -> E AlexPosn -> Env -> Env
aEe n e (Env l ees mm a) = Env l ((n,e):ees) mm a

mE f (Env l ees mm a) = Env l (f ees) mm a

setL :: AlexUserState -> Env -> Env
setL lSt (Env _ ees mm a) = Env lSt ees mm a

type Repl a = InputT (StateT Env IO)

iSt :: IO Env
iSt = Env alexInitUserState [] <$> mem' <*> case arch of {"x86_64" -> pure X64; "aarch64" -> AArch64<$>math'; _ -> error "Unsupported architecture!"}

loop :: Repl AlexPosn ()
loop = do
    inp <- getInputLine " > "
    case words <$> inp of
        Just []                -> loop
        Just (":h":_)          -> showHelp *> loop
        Just (":help":_)       -> showHelp *> loop
        Just ("\\l":_)         -> refcard *> loop
        Just (":ty":e)         -> tyExprR (unwords e) *> loop
        Just [":q"]            -> pure ()
        Just [":quit"]         -> pure ()
        Just (":asm":e)        -> dumpAsm (unwords e) *> loop
        Just (":ann":e)        -> annR (unwords e) *> loop
        Just (":b":e)          -> benchE (unwords e) *> loop
        Just (":bench":e)      -> benchE (unwords e) *> loop
        Just ("⏱":e)           -> benchE (unwords e) *> loop
        Just (":ir":e)         -> irR (unwords e) *> loop
        Just (":c":e)          -> cR (unwords e) *> loop
        Just (":cmm":e)        -> cR (unwords e) *> loop
        Just (":disasm":e)     -> disasm (unwords e) *> loop
        Just (":inspect":e)    -> inspect (unwords e) *> loop
        Just (":compile":e)    -> benchC (unwords e) *> loop
        Just (":yank":f:[fp])  -> iCtx f fp *> loop
        Just [":list"]         -> listCtx *> loop
        Just (":y":f:[fp])     -> iCtx f fp *> loop
        Just (":graph":e)      -> graph (unwords e) *> loop
        Just (":qc":e)         -> qc (unwords e) *> loop
        Just (":quickcheck":e) -> qc (unwords e) *> loop
        Just (":delete":[n])   -> del n *> loop
        Just (":st":n:e)       -> n <~ ubs (unwords e) *> loop
        Just (":store":n:e)    -> n <~ ubs (unwords e) *> loop
        Just e                 -> printExpr (unwords e) *> loop
        Nothing                -> pure ()

del :: String -> Repl AlexPosn ()
del s = lift $ modify (mE (filter (\(Nm n _ _, _) -> n /= st))) where st=T.pack s

listCtx :: Repl AlexPosn ()
listCtx = do {bs <- lg ee; liftIO $ putDocLn (prettyLines (pretty.fst<$>bs))}

graph :: String -> Repl AlexPosn ()
graph s = liftIO $ case dumpX86Ass (ubs s) of
    Left err -> putDocLn (pretty err)
    Right d  -> putDocLn d

showHelp :: Repl AlexPosn ()
showHelp = liftIO $ putStr $ concat
    [ helpOption ":help, :h" "" "Show this help"
    , helpOption ":yank, :y" "<fn> <file>" "Read file"
    , helpOption ":store, :st" "<name> <expression>" "Add to environment"
    , helpOption ":ty" "<expression>" "Display the type of an expression"
    , helpOption ":ann" "<expression>" "Annotate with types"
    , helpOption ":list" "" "List all names that are in scope"
    , helpOption ":bench, :b" "<expression>" "Benchmark an expression"
    , helpOption ":qc" "<proposition>" "Property test"
    , helpOption ":quit, :q" "" "Quit REPL"
    , helpOption ":delete" "<name>" "Delete from REPL environment"
    , helpOption "\\l" "" "Reference card"
    -- TODO: dump debug state
    ]

refcard :: Repl AlexPosn ()
refcard = liftIO $ putStr $ concat
    [ lOption "Λ" "scan" "√" "sqrt"
    , lOption "⋉"  "max" "⋊"  "min"
    , lOption "⍳" "integer range" "⌊, ⌈" "floor, ceiling"
    , lOption "e:" "exp" "⨳ {m,n}" "convolve"
    , lOption "\\~" "successive application" "\\`n" "infix"
    , lOption "_." "log" "'" "map"
    , lOption "`" "zip" "`{i,j∘[k,l]}" "rank"
    , lOption "𝒻" "range (real)" "𝜋" "pi"
    , lOption "_" "negate" ":" "size"
    , lOption "𝓉" "dimension" "{x⟜y;z}" "no inline"
    , lOption "->n" "select" "**" "power"
    , lOption "⊂" "scatter" "}." "last"
    , lOption "⊲" "cons" "⊳" "snoc"
    , lOption "^:" "iterate" "%." "matmul"
    , lOption "⊗" "outer product" "⍉, |:" "transpose"
    , lOption "{." "head" "}:" "typesafe init"
    , lOption "⟨z,w⟩" "array literal" "?p,.e1,.e2" "conditional"
    , lOption "/*" "fold all" "ℝ" "i->f conversion"
    , lOption "⧺" "cat" "{:" "typesafe tail"
    , lOption "⊖" "rotate" "sin." "sine"
    , lOption "𝔯" "rand" "⍳" "range (int)"
    , lOption "/ₒ" "fold with seed" "Λₒ" "scan with seed"
    , lOption "{x←y;z}" "let...in" "⊙" "cycle"
    , lOption "˙" "at" "|" "rem"
    , lOption "@." "index of" "/." "idiv"
    , lOption "%:" "vector mul" "odd." "parity"
    , lOption "⍋" "sort" "𝓕" "course-of-value recursion"
    , lOption "~" "reverse" "¬,⊻,∧,∨" "logical"
    , lOption "♭" "flatten" "♯" "add dimension"
    , lOption "⩪" "indices of" "§, #." "filter"
    , lOption "ug." "unfold" "(i × j)" "dimensions"
    , lOption "gen." "generate" "}:?" "init"
    , lOption "{:?" "tail" "∴" "compose"
    , lOption "𝔸" "digit literal" "ᶥ" "vector indices"
    , lOption "〃" "ditto" "𝐒,𝐊" "combinators"
    , lOption "⑂" "fork" "𝞈,𝟘,𝟙,𝟚" "fancy types"
    -- /₊
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

replEPrint x = liftIO $ case x of
    Left err -> putDocLn (pretty err); Right d -> putDocLn d

disasm :: String -> Repl AlexPosn ()
disasm s = do
    st <- lg _lex
    case rwP st (ubs s) of
        Left err -> pErr err
        Right (eP, i) -> do
            eC <- eRepl eP
            a <- lg _arch
            let d=case a of {X64 -> eDtxt; AArch64{} -> edAtxt}
            res <- liftIO $ d i eC
            liftIO $ case res of
                Left err -> putDocLn (pretty err)
                Right b  -> TIO.putStr b

cR :: String -> Repl AlexPosn ()
cR s = do
    st <- lg _lex
    case rwP st (ubs s) of
        Left err -> pErr err
        Right (eP, i) -> do
            eC <- eRepl eP
            replEPrint $ eDumpC i eC

irR :: String -> Repl AlexPosn ()
irR s = do
    st <- lg _lex
    case rwP st (ubs s) of
        Left err -> pErr err
        Right (eP, i) -> do
            eC <- eRepl eP
            replEPrint $ eDumpIR i eC

dumpAsm :: String -> Repl AlexPosn ()
dumpAsm s = do
    st <- lg _lex
    case rwP st (ubs s) of
        Left err -> pErr err
        Right (eP, i) -> do
            eC <- eRepl eP
            a <- lg _arch
            let dump = case a of {X64 -> eDumpX86; AArch64{} -> eDumpAarch64}
            replEPrint $ dump i eC

tyExprR :: String -> Repl AlexPosn ()
tyExprR s = do
    st <- lg _lex
    case rwP st (ubs s) of
        Left err -> pErr err
        Right (eP, i) -> do
            eC <- eRepl eP
            replEPrint $ (\(e,c,_) -> prettyC (eAnn e, c)) <$> tyClosed i eC

annR :: String -> Repl AlexPosn ()
annR s = do
    st <- lg _lex
    case rwP st (ubs s) of
        Left err    -> pErr err
        Right (eP, i) -> do
            eC <- eRepl eP
            replEPrint $ (\(e,_,_) -> prettyTyped e) <$> tyClosed i eC

freeAsm (sz, fp, mp) = freeFunPtr sz fp -- *> traverse_ free mp

dbgAB :: T b -> U a -> IO TL.Text
dbgAB t p = do
    rnk <- peek (castPtr p :: Ptr Int64)
    dims <- forM [1..fromIntegral rnk] $ \o -> peek $ p `plusPtr` (8*o)
    let sz = fromIntegral (8+8*rnk+rSz t*product dims)
    hb <$> peekArray sz (castPtr p :: Ptr Word8)

hb = TL.unwords.map (toLazyTextWith 1.hexadecimal)

inspect :: String -> Repl AlexPosn ()
inspect s = do
    st <- lg _lex
    case rwP st bs of
        Left err -> pErr err
        Right (eP, i) -> do
            eC <- eRepl eP
            case tyC i eC of
                Left err -> pErr err
                Right (e, _, i') -> do
                    a <- lg _arch; c <- lg mf
                    let efp=case a of {X64 -> eFunP i' c; AArch64 m -> eAFunP i' (c,m)}
                    liftIO $ do
                        asm@(_, fp, _) <- efp eC
                        p <- callFFI fp (retPtr undefined) []
                        case eAnn e of
                            (Arr _ t) -> do TLIO.putStrLn =<< dbgAB t p
                                            free p *> freeAsm asm
                            _ -> pErr ("only arrays can be inspected." :: T.Text)
        where bs = ubs s

(<~) :: String -> BSL.ByteString -> Repl AlexPosn ()
f <~ bs = do
    st <- lg _lex
    case tyParseCtx st bs of
        Left err -> pErr err
        Right (_,i) ->
            let (st', n) = newIdent (AlexPn 0 0 0) (T.pack f) (setM i st)
                x' = parseE st bs
            in lift $ do {modify (aEe n x'); modify (setL st')}
    where setM i' (_, mm, im) = (i', mm, im)

iCtx :: String -> String -> Repl AlexPosn ()
iCtx f fp = do
    p <- liftIO $ doesFileExist fp
    if not p
        then liftIO $ putStrLn "file does not exist."
        else do {bs <- liftIO $ BSL.readFile fp; f <~ bs}

benchC :: String -> Repl AlexPosn ()
benchC s = case tyParse bs of
    Left err -> pErr err
    Right _ -> do
        c <- lg mf; a <- lg _arch
        let cfp=case a of {X64 -> ctxFunP c; AArch64 m -> actxFunP (c,m)}
        liftIO $ benchmark (nfIO (do{asm <- cfp bs; freeAsm asm}))
    where bs = ubs s

up :: T a -> Maybe [T a]
up (A.Arrow t0 t1@A.Arrow{}) = (t0:)<$>up t1
up (A.Arrow t A.B)           = Just [t]
up _                         = Nothing

qc :: String -> Repl AlexPosn ()
qc s = do
    st <- lg _lex
    case rwP st bs of
        Left err -> pErr err
        Right (eP, i) -> do
            eC <- eRepl eP
            case tyC i eC of
                Left err -> pErr err
                Right (e, _, i') -> do
                    c <- lg mf; a <- lg _arch
                    let efp=case a of {X64 -> eFunP i' c; AArch64 m -> eAFunP i' (c,m)}
                    case up (eAnn e) of
                        Nothing -> pErr ("must be a proposition." :: T.Text)
                        Just ty -> liftIO $ do
                            asm@(_, fp, _) <- efp eC
                            let loopϵ 0 = pure Nothing
                                loopϵ n = do
                                    (args, es, mps) <- unzip3 <$> gas ty
                                    b <- callFFI fp retCUChar args
                                    (if cb b
                                        then traverse freeP (catMaybes mps) *> loopϵ (n-1)
                                        else Just es <$ traverse_ freeP (catMaybes mps))
                            res <- loopϵ (100::Int)
                            case res of
                                Nothing -> putDocLn "Passed, 100."
                                Just ex -> putDocLn ("Proposition failed!" <> hardline <> pretty ex)
                            freeAsm asm

  where bs = ubs s
        cb 0=False; cb 1=True

benchE :: String -> Repl AlexPosn ()
benchE s = do
    st <- lg _lex
    case rwP st bs of
        Left err -> pErr err
        Right (eP, i) -> do
            eC <- eRepl eP
            case tyC i eC of
                Left err -> pErr err
                Right (e, _, i') -> do
                    c <- lg mf; a <- lg _arch
                    let efp=case a of {X64 -> eFunP i' c; AArch64 m -> eAFunP i' (c,m)}
                    case eAnn e of
                        I -> do
                            liftIO $ do
                                asm@(_, fp, _) <- efp eC
                                benchmark (nfIO $ callFFI fp retInt64 [])
                                freeAsm asm
                        A.F -> do
                            liftIO $ do
                                asm@(_, fp, _) <- efp eC
                                benchmark (nfIO $ callFFI fp retCDouble [])
                                freeAsm asm
                        A.B -> do
                            liftIO $ do
                                asm@(_, fp, _) <- efp eC
                                benchmark (nfIO $ callFFI fp retCUChar [])
                                freeAsm asm
                        P [A.F,A.F] -> error "Haskell support for float ABI is poor :("
                        (Arr _ _) -> do
                            liftIO $ do
                                asm@(_, fp, _) <- efp eC
                                benchmark (nfIO (do{p<- callFFI fp (retPtr undefined) []; free p}))
                                freeAsm asm
                        P{} ->
                            liftIO $ do
                                asm@(_, fp, _) <- efp eC
                                benchmark (nfIO (do{p<- callFFI fp (retPtr undefined) []; free p}))
                                freeAsm asm
                        A.Arrow{} -> liftIO $ putDocLn "Cannot benchmark a function without arguments"
    where bs = ubs s

rSz A.B=1; rSz I=8; rSz A.F=8; rSz (P ts) = sum (rSz<$>ts); rSz Arr{}=8

pD :: [Int64] -> Doc ann
pD [i] = "Vec" <+> pretty i
pD is  = "Arr" <+> tupledBy "×" (pretty<$>is)

pE :: [Int64] -> [Doc ann] -> Doc ann
pE is es = pD is <+> pEs is es

pEs :: [Int64] -> [Doc ann] -> Doc ann
pEs [_, n] xs = align (brackets (space <> concatWith (\x y -> x <> hardline <> ", " <> y) (list<$>chunksOf (fromIntegral n) xs) <> space))
pEs _ xs      = align (list xs)

pR :: T a -> Ptr b -> IO (Doc ann)
pR I p      = do {i <- peek (castPtr p :: Ptr Int64); pure (pretty i)}
pR A.F p    = do {f <- peek (castPtr p :: Ptr Double); pure (pretty f)}
pR A.B p    = do {b <- peek (castPtr p :: Ptr AB); pure (pretty b)}
pR (P ts) p = tupledBy "*" <$> let pds = offs ts in zipWithM pR ts ((p `plusPtr`) <$> pds)

peekInterpret :: T a -> Ptr b -> IO (Doc ann)
peekInterpret (Arr _ t) p = do
    rnk <- peek (castPtr p :: Ptr Int64)
    dims <- forM [1..fromIntegral rnk] $ \o -> peek $ p `plusPtr` (8*o)
    let datOffs = 8+8*fromIntegral rnk
        xsP = [1..fromIntegral (product dims)] <&> \o -> p `plusPtr` (datOffs+elemSz*(o-1))
    xs <- traverse (pR t) xsP
    pure (pE dims xs)
  where
    elemSz :: Integral a => a
    elemSz = rSz t
peekInterpret (P ts) p = tupled <$>
    let ds=offs ts
    in zipWithM (use peekInterpret) ts ((p `plusPtr`)<$>ds)
peekInterpret t p = pR t p

offs = scanl' (\off t -> off+rSz t) 0

πp :: T a -> Ptr a -> IO (Ptr a)
πp Arr{} p = peek (castPtr p)
πp _ p     = pure p

use f t addr = f t =<< πp t addr

freeByT :: T a -> Ptr b -> IO ()
freeByT Arr{} p  = free p
freeByT (P ts) p = let ds = offs ts in zipWithM_ (use freeByT) ts ((p `plusPtr`)<$>ds)
freeByT _ _      = pure ()

x <::> y = x <!> ":" <+> y

printExpr :: String -> Repl AlexPosn ()
printExpr s = do
    st <- lg _lex
    case rwP st bs of
        Left err -> pErr err
        Right (eP, i) -> do
            eC <- eRepl eP
            case tyC i eC of
                Left (RErr MR{}) -> liftIO $ case tyClosed i eC of
                    Left e -> putDocLn (pretty e)
                    Right (e, c, _) ->
                        let t=eAnn e in putDocLn (pretty e <::> prettyC (t, c))
                Left err -> pErr err
                Right (eLi, _, i') -> do
                    c <- lg mf; a <- lg _arch
                    let efp=case a of {X64 -> eFunP i' c; AArch64 ma -> eAFunP i' (c,ma)}
                    case eAnn (fmap rLi eLi) of
                        I ->
                          liftIO $ do
                              asm@(_, fp, _) <- efp eC -- TODO: i after tyClosed gets discarded?
                              print =<< callFFI fp retInt64 []
                              freeAsm asm
                        A.F ->
                            liftIO $ do
                                asm@(_, fp, _) <- efp eC
                                print =<< callFFI fp retCDouble []
                                freeAsm asm
                        A.B ->
                            liftIO $ do
                                asm@(_, fp, _) <- efp eC
                                cb <- callFFI fp retWord8 []
                                putStrLn (sB cb)
                                freeAsm asm
                            where sB 1 = "#t"; sB 0 = "#f"
                        A.Arrow{} -> liftIO $ putDocLn (pretty eLi <::> pretty (eAnn eLi))
                        t ->
                            liftIO $ do
                                asm@(_, fp, _) <- efp eC
                                p <- callFFI fp (retPtr undefined) []
                                putDocLn =<< peekInterpret t p
                                freeByT t p *> freeAsm asm
    where bs = ubs s

parseE st bs = fst . either (error "Internal error?") id $ rwP st bs

mentions :: E a -> Nm a -> Bool
mentions (EApp _ e0 e1) n     = e0 `mentions` n || e1 `mentions` n
mentions (Var _ n1) n         = n==n1
mentions ILit{} _             = False
mentions FLit{} _             = False
mentions BLit{} _             = False
mentions (Cond _ p e0 e1) n   = p `mentions` n || e0 `mentions` n || e1 `mentions` n
mentions Builtin{} _          = False
mentions (Let _ (_, eϵ) e) n  = e `mentions` n || eϵ `mentions` n
mentions (Def _ (_, eϵ) e) n  = e `mentions` n || eϵ `mentions` n
mentions (LLet _ (_, eϵ) e) n = e `mentions` n || eϵ `mentions` n
mentions (ALit _ es) n        = any (`mentions` n) es
mentions (A.Lam _ _ e) n      = e `mentions` n
mentions (Ann _ e _) n        = e `mentions` n
mentions (Tup _ es) n         = any (`mentions` n) es
mentions Dfn{} _              = desugar
mentions ResVar{} _           = desugar
mentions Parens{} _           = desugar
mentions Id{} _               = error "Internal error."

desugar = error "Internal error. Should have been desugared."

eRepl :: E AlexPosn -> Repl AlexPosn (E AlexPosn)
eRepl e = do {ees <- lg ee; pure (flet ees e)}
    where flet = thread . fmap (\b@(n,eϵ) eR -> if eR `mentions` n then Let (eAnn eϵ) b eR else eR) where thread = foldr (.) id

putDocLn = putDoc.(<>hardline)
pErr err = liftIO $ putDocLn (pretty err)
