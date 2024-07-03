module H ( Arch (..), run ) where

import qualified As
import           CGen
import           Control.Exception         (Exception, throwIO)
import qualified Data.ByteString.Lazy      as BSL
import qualified Data.Text                 as T
import qualified Nasm
import           P
import           Prettyprinter.Render.Text (hPutDoc)
import           System.IO                 (IOMode (WriteMode), withFile)

data Arch = Aarch64 | X64

run :: (FilePath, Arch, T.Text) -> IO ()
run (fpϵ, a, n) = do
    contents <- BSL.readFile fpϵ
    ct <- cS contents
    let asm=case a of {X64 -> Nasm.writeO; Aarch64 -> As.writeO}
    asm n contents True
    withFile (T.unpack n <> ".h") WriteMode $ \h -> hPutDoc h ct
    where cS s = do {t <- yIO (fst<$>getTy s); yIO $ pCty n t}

yIO :: Exception x => Either x a -> IO a
yIO = either throwIO pure
