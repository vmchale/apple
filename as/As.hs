module As ( writeO ) where

import qualified Data.ByteString.Lazy      as BSL
import           Data.Functor              (void)
import qualified Data.Text                 as T
import qualified Data.Text.Lazy.IO         as TLIO
import           P
import           Prettyprinter             (layoutCompact)
import           Prettyprinter.Render.Text (renderLazy)
import           System.Info               (arch, os)
import           System.IO                 (hFlush)
import           System.IO.Temp            (withSystemTempFile)
import           System.Process            (CreateProcess (..), StdStream (Inherit), proc, readCreateProcess)

writeO :: T.Text -- ^ Function name
       -> BSL.ByteString
       -> Bool -- ^ Debug symbols?
       -> IO ()
writeO f contents dbg = withSystemTempFile "apple.S" $ \fp h -> do
    let txt = renderLazy $ layoutCompact (as f contents)
    TLIO.hPutStr h txt
    hFlush h
    let debugFlag = if dbg then ("-g":) else id
    {-# SCC "as" #-} void $ readCreateProcess ((proc exe (debugFlag [fp, "-o", fpO])) { std_err = Inherit }) ""
    where fpO = T.unpack f <> ".o"
          exe=case (arch, os) of {("aarch64",_) -> "as"; (_,"linux") -> "aarch64-linux-gnu-as"}
