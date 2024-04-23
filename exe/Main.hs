module Main (main) where

import qualified Data.Text           as T
import qualified Data.Version        as V
import           H
import           Options.Applicative
import qualified Paths_apple         as P
import           System.Info         (arch)

fp :: Parser FilePath
fp = argument str
    (metavar "SRC_FILE"
    <> help "Source file")

fun :: Parser T.Text
fun = strOption
    (short 'f'
    <> metavar "FUNCTION"
    <> help "Function name in generated .o")

farch :: Parser Arch
farch = fmap parseArch $ optional $ strOption
    (long "arch"
    <> metavar "ARCH"
    <> help "Target architecture"
    <> completer (listCompleter ["x64", "aarch64"]))
    where parseArch :: Maybe String -> Arch
          parseArch str' = case (str', arch) of
            (Nothing, "aarch64") -> Aarch64
            (Nothing, "x86_64")  -> X64
            (Just "aarch64", _)  -> Aarch64
            (Just "arm64", _)    -> Aarch64
            (Just "x64", _)      -> X64
            (Just "x86_64", _)   -> X64
            (Just "x86-64", _)   -> X64
            (Just "amd64", _)    -> X64
            _                    -> error "Failed to parse architecture! Try one of x64, aarch64"

wrapper :: ParserInfo (FilePath, Arch, T.Text)
wrapper = info (helper <*> versionMod <*> p)
    (fullDesc
    <> header "Output object files with Apple array system"
    <> progDesc "writeo - generate object files")
    where p = (,,) <$> fp <*> farch <*> fun

versionMod :: Parser (a -> a)
versionMod = infoOption (V.showVersion P.version) (short 'V' <> long "version" <> help "Show version")

main :: IO ()
main = run =<< execParser wrapper
