module Main (main) where

import           Control.DeepSeq                  (NFData (..), rwhnf)
import           Control.Exception                (Exception, throw)
import           Criterion.Main
import qualified Data.ByteString.Lazy             as BSL
import           Data.Functor                     (($>))
import           Data.Int                         (Int64)
import           Data.Number.Erf                  (erf, normcdf)
import           Foreign.ForeignPtr               (ForeignPtr, mallocForeignPtrBytes, withForeignPtr)
import           Foreign.Marshal.Alloc            (free, mallocBytes)
import           Foreign.Ptr                      (FunPtr, Ptr)
import           Foreign.Storable                 (Storable (..))
import           Hs.A
import           I
import qualified Math.Hypergeometric              as Hyper
import qualified Math.SpecialFunction             as Math
import           P
import           Statistics.Distribution          (cumulative)
import           Statistics.Distribution.StudentT (studentT)
import           System.Info                      (arch)
import           Ty

risingFactorial :: Integral a => a -> a -> a
risingFactorial x n = product [x..(x+n-1)]
{-# SPECIALIZE risingFactorial :: Int -> Int -> Int #-}

hsEntropy :: Floating a => [a] -> a
hsEntropy xs = sum [ x * log x | x <- xs ]

kl :: Floating a => [a] -> [a] -> a
kl xs ys = sum (zipWith (\x y -> x * log (x/y)) xs ys)

aA :: Storable a => Apple a -> IO (U a)
aA x = do
    p <- mallocBytes (sizeOf x)
    poke p x $> p

leakFp = fmap fst.case arch of {"aarch64" -> aFunP; "x86_64" -> funP}

aAF :: Storable a => Apple a -> IO (ForeignPtr (Apple a))
aAF x = do {p <- mallocForeignPtrBytes (sizeOf x); withForeignPtr p (`poke` x) $> p}

instance NFData (ForeignPtr a) where
    rnf = rwhnf

main :: IO ()
main = do
    xsPtr <- aA (AA 1 [500] xs)
    ysPtr <- aA (AA 1 [500] ys)
    fp <- fmap iii . leakFp =<< BSL.readFile "test/examples/risingFactorial.🍎"
    entropyFp <- fmap af . leakFp =<< BSL.readFile "test/examples/entropy.🍏"
    klFp <- fmap aaf . leakFp =<< BSL.readFile "test/examples/kl.🍎"
    erfFp <- fmap ff . leakFp =<< BSL.readFile "math/erf.🍏"
    ncdfFp <- fmap ff . leakFp =<< BSL.readFile "math/ncdf.🍎"
    scanFp <- fmap aa . leakFp =<< BSL.readFile "bench/apple/scanmax.🍏"
    scanfFp <- fmap aa . leakFp =<< BSL.readFile "bench/apple/scanmaxf.🍏"
    wMax <- fmap aa . leakFp =<< BSL.readFile "bench/apple/maxWindow.🍎"
    cMax <- fmap aa.leakFp =<< BSL.readFile "bench/apple/convMax.🍏"
    filt <- fmap aa.leakFp =<< BSL.readFile "bench/apple/evens.🍎"
    ixfilt <- fmap aa.leakFp =<< BSL.readFile "bench/apple/evenIx.🍎"
    ᴀFp <- fmap aaf . leakFp =<< BSL.readFile "test/examples/offset.🍏"
    gammaFp <- fmap ff . leakFp =<< BSL.readFile "math/gamma.🍏"
    tcdfFp <- fmap fff . leakFp =<< BSL.readFile "math/tcdf.🍎"
    xorFp <- fmap aaafp4 . leakFp =<< BSL.readFile "test/data/trainXor.🍎"
    v'izeFp <- fmap aa . leakFp =<< BSL.readFile "bench/apple/vize.🍏"
    dp <- fmap aaf . leakFp =<< BSL.readFile "test/examples/dotprod.🍏"
    v <- fmap aaa . leakFp =<< BSL.readFile "test/data/vb.🍏"
    mul <- fmap aaa.leakFp =<< BSL.readFile "test/data/mul.🍏"
    sm10 <- fmap aaa.leakFp =<< BSL.readFile "test/data/m10.🍎"
    sm6 <- fmap aaa.leakFp =<< BSL.readFile "test/data/m6.🍎"
    mulT <- fmap aaa.leakFp =<< BSL.readFile "test/data/mulT.🍏"
    vr <- fmap aaa . leakFp =<< BSL.readFile "test/data/vmul.🍏"
    vb6 <- fmap aaa . leakFp =<< BSL.readFile "test/data/v6.🍎"
    vb9 <- fmap aaa . leakFp =<< BSL.readFile "test/data/v9.🍎"
    mulrank <- fmap aaa . leakFp =<< BSL.readFile "test/examples/mul.🍏"
    catFp <- fmap aaa . leakFp =<< BSL.readFile "bench/apple/cat.🍏"
    softmax <- fmap aa . leakFp =<< BSL.readFile "test/data/softmax.🍎"
    amgm <- fmap fff.leakFp =<< BSL.readFile "math/amgm.🍏"
    amgmG <- fmap fff.leakFp =<< BSL.readFile "test/data/amgmGen.🍏"
    defaultMain [ env files $ \ ~(t, x, 𝛾, ꜰ, ᴀ) ->
                  bgroup "pipeline"
                      [ bench "tyParse (tcdf)" $ nf tyParse t
                      , bench "tyParse (xor)" $ nf tyParse x
                      , bench "x86asm (gamma)" $ nf x86G 𝛾
                      , bench "x86asm (fcdf)" $ nf x86G ꜰ
                      -- , bench "x86asm (A)" $ nf x86G ᴀ
                      , bench "arm (fcdf)" $ nf aarch64 ꜰ
                      , bench "arm (tcdf)" $ nf aarch64 t
                      , bench "arm (A)" $ nf aarch64 ᴀ
                      ]
                      -- TODO: thunks after type checking?
                , env (fmap yeet erfParsed) $ \ast ->
                  bgroup "ty"
                      [ bench "tyClosed" $ nf (\(e, m) -> tyClosed m e) ast
                      ]
                , env (fmap yeet erfTy) $ \e ->
                  bgroup "inline"
                      [ bench "inline" $ nf (\(ast, i) -> fst (inline i ast)) e
                      ]
                , bgroup "erf"
                      [ bench "erf (libm)" $ nf erf (1 :: Double)
                      , bench "erf (hypergeometric)" $ nf Hyper.erf (1 :: Double)
                      , bench "erf (jit)" $ nfIO (pure $ erfFp 1)
                      ]
                , bgroup "risingFactorial"
                      [ bench "hs" $ nf (risingFactorial 5) (15 :: Int64)
                      , bench "jit" $ nf (fp 5) 15
                      ]
                , bgroup "entropy"
                      [ bench "hs" $ nf hsEntropy xs
                      , bench "jit" $ nfIO (pure $ entropyFp xsPtr)
                      ]
                , bgroup "k-l"
                      [ bench "hs" $ nf (kl xs) ys
                      , bench "jit" $ nfIO (pure $ klFp xsPtr ysPtr)
                      ]
                , bgroup "ncdf"
                      [ bench "lib" $ nf normcdf (2 :: Double)
                      , bench "jit" $ nf ncdfFp 2
                      ]
                , bgroup "tcdf"
                      [ bench "hs" $ nf (Math.tcdf (12::Double)) (2::Double)
                      , bench "stat" $ nf (cumulative (studentT 12)) 2
                      , bench "jit" $ nf (tcdfFp 2) 12
                      ]
                , bgroup "Γ"
                      [ bench "hs" $ nf Math.gamma (1.5 :: Double)
                      , bench "jit" $ nf gammaFp 1.5
                      ]
                , env big $ \ ~(i,f) ->
                  bgroup "scanmax"
                      [ bench "apple" $ nfIO (do {p<- withForeignPtr i scanFp;free p})
                      , bench "applef" $ nfIO (do {p<- withForeignPtr f scanfFp;free p})
                      ]
                , env simdEnv $ \isp ->
                  env big $ \ ~(_,f) ->
                  bgroup "simd"
                      [ bench "dotprod" $ nfIO (withForeignPtr f $ \fPtr -> pure $ dp fPtr fPtr)
                      , bench "++" $ nfIO (do {p <- withForeignPtr isp $ \iSmallPtr -> catFp iSmallPtr iSmallPtr; free p})
                      , bench "window" $ nfIO (do {p <- withForeignPtr f wMax; free p})
                      ]
                , env matEnv $ \ ~(m6,m9,m10,v6,v9) ->
                  bgroup "mat"
                      [ bench "vmul (sized) (2^6)" $ nfIO (do {p <- withForeignPtr m6 $ \mPtr -> withForeignPtr v6 $ \vPtr -> vb6 mPtr vPtr; free p})
                      , bench "vmul (sized) (2^9)" $ nfIO (do {p <- withForeignPtr m9 $ \mPtr -> withForeignPtr v9 $ \vPtr -> vb9 mPtr vPtr; free p})
                      , bench "vmul (2^6)" $ nfIO (do {p <- withForeignPtr m6 $ \mPtr -> withForeignPtr v6 $ \vPtr -> v mPtr vPtr; free p})
                      , bench "vmul (2^9)" $ nfIO (do {p <- withForeignPtr m9 $ \mPtr -> withForeignPtr v9 $ \vPtr -> v mPtr vPtr; free p})
                      , bench "mul (2^6)" $ nfIO (do {p <- withForeignPtr m6 $ \mPtr -> mul mPtr mPtr; free p})
                      , bench "mul (2^10)" $ nfIO (do {p <- withForeignPtr m10 $ \mPtr -> mul mPtr mPtr; free p})
                      , bench "mul (sized) (2^10)" $ nfIO (do {p <- withForeignPtr m10 $ \mPtr -> sm10 mPtr mPtr; free p})
                      , bench "mul (sized) (2^6)" $ nfIO (do {p <- withForeignPtr m10 $ \mPtr -> sm6 mPtr mPtr; free p})
                      , bench "vmul (rank) (2^9)" $ nfIO (do {p <- withForeignPtr m9 $ \mPtr -> withForeignPtr v9 $ \vPtr -> vr mPtr vPtr; free p})
                      , bench "mul (rank) (2^6)" $ nfIO (do {p <- withForeignPtr m6 $ \mPtr -> mulrank mPtr mPtr; free p})
                      , bench "mul-of-transp (2^6)" $ nfIO (do {p <- withForeignPtr m6 $ \mPtr ->mulT mPtr mPtr; free p})
                      , bench "mul-of-transp (2^9)" $ nfIO (do {p <- withForeignPtr m9 $ \mPtr ->mulT mPtr mPtr; free p})
                      ]
                , env big $ \ ~(i, f) ->
                  bgroup "idioms"
                      [ bench "conv (1-d)" $ nfIO (do {p <- withForeignPtr f cMax; free p})
                      , bench "even (filt)" $ nfIO (do {p <- withForeignPtr i filt; free p})
                      , bench "even (map-ix)" $ nfIO (do {p <- withForeignPtr i ixfilt; free p})
                      , bench "amgm" $ nf (amgm 1) 2
                      , bench "amgm (gen.)" $ nf (amgmG 1) 2
                      ]
                , env eEnv $ \ ~(p0,p1) ->
                  bgroup "elliptic"
                      [ bench "A" $ nfIO (withForeignPtr p0 $ \p0Ptr -> withForeignPtr p1 $ \p1Ptr -> pure $ ᴀFp p0Ptr p1Ptr) ]
                , env xorEnv $ \ ~(wh, wo, bh) ->
                  bgroup "xor"
                      [ bench "train" $ nfIO $
                          withForeignPtr wh $ \whPtr ->
                          withForeignPtr wo $ \woPtr ->
                          withForeignPtr bh $ \bhPtr ->
                          xorFp whPtr woPtr bhPtr 0.57823076
                      ]
                , env simdEnv $ \ isp ->
                  env matEnv $ \ ~(m6,_,_,_,_) ->
                  bgroup "mnist"
                      [ bench "vize" $ nfIO (do {p <- withForeignPtr isp $ \iSmallPtr -> v'izeFp iSmallPtr; free p})
                      , bench "softmax" $ nfIO (do {p <- withForeignPtr m6 $ \mPtr -> softmax mPtr; free p})
                      ]
                ]
    where erfSrc = BSL.readFile "math/erf.🍏"
          gamma = BSL.readFile "math/gamma.🍏"
          tcdf = BSL.readFile "math/tcdf.🍎"
          xor = BSL.readFile "test/examples/xor.🍎"
          fcdf = BSL.readFile "math/fcdf.🍎"
          offA = BSL.readFile "test/examples/ellipticFourier.🍎"
          files = (,,,,) <$> tcdf <*> xor <*> gamma <*> fcdf <*> offA
          erfParsed = parseRename <$> erfSrc
          erfTy = tyParse <$> erfSrc
          yeet :: (Exception e) => Either e a -> a
          yeet = either throw id
          xs = replicate 500 (0.002 :: Double)
          ys = replicate 500 (0.002 :: Double)
          big = do
              iPtr <- aAF (AA 1 [10000000] (replicate 10000000 (1::Int64)))
              fPtr <- aAF (AA 1 [10000000] (replicate 10000000 (1::Double)))
              pure (iPtr,fPtr)
          simdEnv = aAF (AA 1 [100000] (replicate 100000 (1::Int64)))
          matEnv = do
              m6Ptr <- aAF (AA 2 [64,64] (replicate 4096 (0.002::Double)))
              m9Ptr <- aAF (AA 2 [512,512] (replicate 262144 (0.002::Double)))
              m10Ptr <- aAF (AA 2 [1024,1024] (replicate 1048576 (0.002::Double)))
              v6Ptr <- aAF (AA 1 [64] (replicate 64 (2::Double)))
              v9Ptr <- aAF (AA 1 [512] (replicate 512 (3::Double)))
              pure (m6Ptr, m9Ptr, m10Ptr, v6Ptr, v9Ptr)
          xorEnv = do
              whPtr <- aAF (AA 2 [2,2] [0.51426693,0.56885825,0.48725347,0.15041493::Double])
              woPtr <- aAF (AA 1 [2] [0.14801747,0.37182892::Double])
              bhPtr <- aAF (AA 1 [2] [0.79726405,0.67601843::Double])
              pure (whPtr, woPtr, bhPtr)
          eEnv = do
              p0 <- aAF (AA 1 [3] [0.0::Double,4,4])
              p1 <- aAF (AA 1 [3] [0.0::Double,0.3])
              pure (p0,p1)

foreign import ccall "dynamic" iii :: FunPtr (Int -> Int -> Int) -> Int -> Int -> Int
foreign import ccall "dynamic" ff :: FunPtr (Double -> Double) -> Double -> Double
foreign import ccall "dynamic" fff :: FunPtr (Double -> Double -> Double) -> Double -> Double -> Double
foreign import ccall "dynamic" aaf :: FunPtr (U a -> U b -> Double) -> U a -> U b -> Double
foreign import ccall "dynamic" af :: FunPtr (U a -> Double) -> U a -> Double
foreign import ccall "dynamic" aa :: FunPtr (U a -> IO (U a)) -> U a -> IO (U a)
foreign import ccall "dynamic" aaa :: FunPtr (U a -> U b -> IO (U c)) -> U a -> U b -> IO (U c)
foreign import ccall "dynamic" aaafp4 :: FunPtr (U a -> U b -> U c -> Double -> IO (Ptr (P4 (U d) (U e) (U f) g))) -> U a -> U b -> U c -> Double -> IO (Ptr (P4 (U d) (U e) (U f) g))
