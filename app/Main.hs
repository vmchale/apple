module Main (main) where

import           Control.Exception    (throwIO)
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Version         as V
import           Options.Applicative
import           P
import qualified Paths_apple          as P

fp :: Parser FilePath
fp = argument str
    (metavar "SRC_FILE"
    <> help "Source file")

wrapper :: ParserInfo FilePath
wrapper = info (helper <*> versionMod <*> fp)
    (fullDesc
    <> progDesc "Editor integration for the Apple language"
    <> header "atc - apple type checker")

versionMod :: Parser (a -> a)
versionMod = infoOption (V.showVersion P.version) (short 'V' <> long "version" <> help "Show version")

main :: IO ()
main = run =<< execParser wrapper

run :: FilePath -> IO ()
run fpϵ = do
    contents <- BSL.readFile fpϵ
    case tyParse contents of
        Left err -> throwIO err
        Right{}  -> pure ()
