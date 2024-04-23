module H ( Arch (..), run ) where

import qualified As
import           CGen
import           Control.Exception         (throwIO)
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
    t <- either throwIO (pure.fst) (getTy contents)
    ct <- either throwIO pure $ pCty n t
    let asm=case a of {X64 -> Nasm.writeO; Aarch64 -> As.writeO}
    asm n contents True
    withFile (T.unpack n <> ".h") WriteMode $ \h -> hPutDoc h ct
