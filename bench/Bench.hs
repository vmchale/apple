module Main (main) where

import           Control.Exception     (Exception, throw)
import           Criterion.Main
import qualified Data.ByteString.Lazy  as BSL
import           Data.Functor          (($>))
import           Data.Int              (Int64)
import           Data.Number.Erf       (erf, normcdf)
import           Foreign.Marshal.Alloc (free, mallocBytes)
import           Foreign.Ptr           (FunPtr, Ptr)
import           Foreign.Storable      (Storable (..))
import           Hs.A
import           I
import qualified Math.Hypergeometric   as Hyper
import qualified Math.SpecialFunction  as Math
import           P
import           System.Info           (arch)
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

main :: IO ()
main = do
    -- this sucks but using env segfaults idk
    xsPtr <- aA (AA 1 [500] xs)
    ysPtr <- aA (AA 1 [500] ys)
    iPtr <- aA (AA 1 [10000000] (replicate 10000000 (1::Int)))
    fPtr <- aA (AA 1 [10000000] (replicate 10000000 (1::Double)))
    p0Ptr <- aA (AA 1 [3] [0.0::Double,4,4])
    p1Ptr <- aA (AA 1 [3] [0.0::Double,0.3])
    whPtr <- aA (AA 2 [2,2] [0.51426693,0.56885825,0.48725347,0.15041493::Double])
    woPtr <- aA (AA 1 [2] [0.14801747,0.37182892::Double])
    bhPtr <- aA (AA 1 [2] [0.79726405,0.67601843::Double])
    fp <- fmap iii . leakFp =<< BSL.readFile "test/examples/risingFactorial.🍎"
    entropyFp <- fmap af . leakFp =<< BSL.readFile "test/examples/entropy.🍏"
    klFp <- fmap aaf . leakFp =<< BSL.readFile "test/examples/kl.🍎"
    erfFp <- fmap ff . leakFp =<< BSL.readFile "math/erf.🍏"
    ncdfFp <- fmap ff . leakFp =<< BSL.readFile "math/ncdf.🍎"
    scanFp <- fmap aa . leakFp =<< BSL.readFile "bench/apple/scanmax.🍏"
    scanfFp <- fmap aa . leakFp =<< BSL.readFile "bench/apple/scanmaxf.🍏"
    ᴀFp <- fmap aaf . leakFp =<< BSL.readFile "test/examples/offset.🍏"
    gammaFp <- fmap ff . leakFp =<< BSL.readFile "math/gamma.🍏"
    tcdfFp <- fmap fff . leakFp =<< BSL.readFile "math/tcdf.🍎"
    xorFp <- fmap aaafp4 . leakFp =<< BSL.readFile "test/data/trainXor.🍎"
    defaultMain [ env files $ \ ~(t, x, 𝛾, ꜰ, ᴀ) ->
                  bgroup "pipeline"
                      [ bench "tyParse (tcdf)" $ nf tyParse t
                      , bench "tyParse (xor)" $ nf tyParse x
                      , bench "x86asm (gamma)" $ nf x86G 𝛾
                      , bench "x86asm (fcdf)" $ nf x86G ꜰ
                      , bench "x86asm (A)" $ nf x86G ᴀ
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
                      , bench "jit" $ nfIO $ (pure $ entropyFp xsPtr)
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
                      , bench "jit" $ nf (tcdfFp 2) 12
                      ]
                , bgroup "Γ"
                      [ bench "hs" $ nf Math.gamma (1.5 :: Double)
                      , bench "jit" $ nf gammaFp 1.5
                      ]
                , bgroup "scanmax"
                      [ bench "apple" $ nfIO (do {p<- scanFp iPtr;free p})
                      , bench "applef" $ nfIO (do {p<- scanfFp fPtr;free p})
                      ]
                , bgroup "elliptic"
                      [ bench "A" $ nfIO (pure $ ᴀFp p0Ptr p1Ptr) ]
                , bgroup "xor"
                      [ bench "train" $ nfIO (xorFp whPtr woPtr bhPtr 0.57823076) ]
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

foreign import ccall "dynamic" iii :: FunPtr (Int -> Int -> Int) -> Int -> Int -> Int
foreign import ccall "dynamic" ff :: FunPtr (Double -> Double) -> Double -> Double
foreign import ccall "dynamic" fff :: FunPtr (Double -> Double -> Double) -> Double -> Double -> Double
foreign import ccall "dynamic" aaf :: FunPtr (U a -> U a -> Double) -> U a -> U a -> Double
foreign import ccall "dynamic" af :: FunPtr (U a -> Double) -> U a -> Double
foreign import ccall "dynamic" aa :: FunPtr (U a -> IO (U a)) -> U a -> IO (U a)
foreign import ccall "dynamic" aaafp4 :: FunPtr (U a -> U b -> U c -> Double -> IO (Ptr (P4 (U d) (U e) (U f) g))) -> U a -> U b -> U c -> Double -> IO (Ptr (P4 (U d) (U e) (U f) g))
