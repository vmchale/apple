module Main (main) where

import           CGen
import           Control.Exception         (throwIO)
import qualified Data.ByteString.Lazy      as BSL
import           Data.Semigroup            ((<>))
import qualified Data.Text                 as T
import qualified Data.Version              as V
import qualified Nasm
import qualified As
import           Options.Applicative
import           P
import qualified Paths_apple               as P
import           Prettyprinter.Render.Text (hPutDoc)
import           System.IO                 (IOMode (WriteMode), withFile)


fp :: Parser FilePath
fp = argument str
    (metavar "SRC_FILE"
    <> help "Source file")

fun :: Parser T.Text
fun = strOption
    (short 'f'
    <> metavar "FUNCTION"
    <> help "Function name in generated .o")

wrapper :: ParserInfo (FilePath, T.Text)
wrapper = info (helper <*> versionMod <*> p)
    (fullDesc
    <> header "Output object files with Apple array system"
    <> progDesc "writeo - generate object files")
    where p = (,) <$> fp <*> fun

versionMod :: Parser (a -> a)
versionMod = infoOption (V.showVersion P.version) (short 'V' <> long "version" <> help "Show version")

main :: IO ()
main = run =<< execParser wrapper

run :: (FilePath, T.Text) -> IO ()
run (fpϵ, n) = do
    contents <- BSL.readFile fpϵ
    t <- either throwIO (pure.fst) (getTy contents)
    ct <- either throwIO pure $ pCty n t
    -- Nasm.writeO n contents True
    As.writeO n contents True
    withFile (T.unpack n <> ".h") WriteMode $ \h -> hPutDoc h ct
