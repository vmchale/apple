{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import qualified Data.ByteString.Lazy  as BSL
import           Data.Int              (Int64)
import           Foreign.C.Types       (CUChar (..))
import           Foreign.Marshal.Alloc (allocaBytes)
import           Foreign.Ptr           (FunPtr, Ptr)
import           Foreign.Storable      (Storable (..))
import           Hs.A
import           Math.Hypergeometric   (erf, hypergeometric, ncdf)
import           Math.SpecialFunction  (agm, bessel1, chisqcdf, completeElliptic, gamma, tcdf)
import           Numeric.NumberTheory  (isPrime, radical, tau)
import           P
import           System.Info           (arch)
import           Test.QuickCheck       (property)
import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Tasty.QuickCheck (testProperty)

kl :: Floating a => [a] -> [a] -> a
kl xs ys = sum [ x * log (x/y) | x <- xs, y <- ys ]

infixl 1 .?=

(.?=) :: (Show a, Ord a, Floating a) => a -> a -> Assertion
x .?= y = assertBool ("expected " ++ show y ++ ", got " ++ show x) ((x-y)/y<1e-15&&(y-x)/y<1e-15)

main :: IO ()
main = do
    pjit <- fpn =<< BSL.readFile "test/examples/isPrime.🍏"
    rjit <- fpn =<< BSL.readFile "math/numbertheory/radical.🍎"
    τjit <- fpn =<< BSL.readFile "math/numbertheory/𝜏.🍏"
    defaultMain $ testGroup "All" $ pTest pjit rjit τjit:[rTy,tyT,allT]

pTest :: FunPtr (Int64 -> CUChar)
      -> FunPtr (Int64 -> Int64)
      -> FunPtr (Int64 -> Int64)
      -> TestTree
pTest pfn rfn τfn = testGroup "property tests"
    [ testProperty "isPrime" $ property $ \n -> n < 2 || isPrime n == cb (ib pfn (fromIntegral n))
    , testProperty "radical" $ property $ \n -> n < 3 || radical n == hsIi rfn n
    , testProperty "τ" $ property $ \n -> n < 1 || tau n == hsIi τfn n
    -- TODO: consSum,
    ]
  where
    cb 0=False; cb 1=True

rTy :: TestTree
rTy = testGroup "Regression tests"
    [ tyF "test/data/polymorphic.🍎"
    , tyF "test/examples/regress.🍎"
    , tyF "test/examples/convolve.🍎"
    , tyF "test/examples/xor.🍎"
    ]

tyT :: TestTree
tyT = testGroup "Type system" [ tyS "((-)\\~)" ]

allT :: TestTree
allT = testGroup "jit"
    [ testCase "exp (series)" $ do { res <- jitExp 20 1 ; res .?= exp 1 }
    , testCase "dotprod" $ do { res <- fpAaf "test/examples/dotprod.🍏" [1,2,3] [2,4,6] ; res @?= 28 }
    , testCase "euclidean" $ do { res <- fpAaf "test/examples/dist.🍎" [0,0,0] [3,4,5] ; res @?= sqrt 50 }
    , testCase "ncdf" $ do { res <- ncdfJit 2 ; res .?= ncdf 2 }
    , testCase "erf" $ do { res <- erfJit 2 ; res .?= erf 2 }
    , testCase "primes" $ do { res <- fpIa "test/data/primes.🍏" 30; res @?= [T,T,F,T,F,T,F,F,F,T,F,T,F,F,F,T,F,T,F,F,F,T,F,F,F,F,F,T,F] }
    , testCase "primes-up-to" $ do { res <- fpIa "test/examples/primes.🍎" 100; res @?= [2::Int64,3,5,7,11,13,17,19,23,29,31,37,41,43,47,53,59,61,67,71,73,79,83,89,97] }
    , testCase "shoelace" $ do { res <- fpAaf "test/examples/shoelace.🍎" [0,1,1] [0,0,1] ; res @?= 0.5 }
    , testCase "maxscan" $ do { res <- aaFp "bench/apple/scanmax.🍏" [4::Int64,6,1] ; res @?= [0::Int64,4,6,6] }
    , testCase "b" $ do { res <- jitB [1,2,3] [2,4,6] ; res @?= 2 }
    , testCase "fib" $ do { res <- fpIa "test/examples/fib.🍎" 6; res @?= [1::Int64,1,2,3,5,8,13] } --
    , testCase "oeis (A000081)" $ do { res <- fpIa "math/oeis/A000081.🍏" 12; res @?= [0::Int64,1,1,2,4,9,20,48,115,286,719,1842,4766] }
    , testCase "7-day sliding average" $ do { res <- aaFp "test/examples/weekMean.🍎" [0..7::Double] ; res @?= [3,4::Double] }
    , testCase "bessel1" $ do { res <- fpIff "math/bessel.🍏" 1 3 ; res @?= bessel1 1 3 }
    , testCase "amgm (iter)" $ do { res <- fpFff "math/amgm.🍏" 1 (sqrt 2) ; res @?= agm 1 (sqrt 2) }
    , testCase "transpose" $ do { (AA 2 [2, 3] res) <- fpAa "test/data/T.🍏" (AA 2 [3,2] [1,2,3,4,5,6::Double]); res @?= [1,3,5,2,4,6::Double] }
    , testCase "whiten" $ do { (AA 2 [3,3] res) <- fpAa "math/stats/whiten.🍏" (AA 2 [3,3] [1.9,2.3,1.7,1.5,2.5,2.2,0.8,0.6,1.7::Double]); res @?= [4.179442778108569,2.698113510169924,7.212489168102781,3.299560087980449,2.9327320762716567,9.333809511662423,1.7597653802562396,0.7038556983051976,7.212489168102781::Double] }
    , testCase "vmul builtin" $ do { (AA 1 [3] res) <- fpAaa "test/data/vb.🍏" (AA 2 [3,2] [1,2,3,4,5,6::Double]) (AA 1 [2] [1,1::Double]); res @?= [3,7,11::Double] }
    , testCase "vmul builtin" $ do { (AA 1 [2] res) <- fpAaa "test/data/vb.🍏" (AA 2 [2,3] [1,2,3,4,5,6::Double]) (AA 1 [3] [1,1,1::Double]); res @?= [6,15::Double] }
    , testCase "vmul" $ do { (AA 1 [3] res) <- fpAaa "test/data/vmul.🍏" (AA 2 [3,2] [1,2,3,4,5,6::Double]) (AA 1 [2] [1,1::Double]); res @?= [3,7,11::Double] }
    -- 3,4,5 instead of 4,5,6!
    , testCase "vmul" $ do { (AA 1 [2] res) <- fpAaa "test/data/vmul.🍏" (AA 2 [2,3] [1,2,3,4,5,6::Double]) (AA 1 [3] [1,1,1::Double]); res @?= [6,15::Double] }
    , testCase "matmul builtin" $ do { (AA 2 [2, 2] res) <- fpAaa "test/data/mul.🍏" (AA 2 [2,3] [2,1,1,5,4,1::Double]) (AA 2 [3,2] [2,0,2,0,7,3::Double]); res @?= [13,3,25,3::Double] }
    , testCase "mul-of-transp" $ do { (AA 2 [2, 2] res) <- fpAaa "test/data/mulT.🍏" (AA 2 [2,3] [2,1,1,5,4,1::Double]) (AA 2 [2,3] [2,2,7,0,0,3::Double]); res @?= [13,3,25,3::Double] }
    , testCase "matmul" $ do { (AA 2 [2, 2] res) <- fpAaa "test/examples/mul.🍏" (AA 2 [2,3] [2,1,1,5,4,1::Double]) (AA 2 [3,2] [2,0,2,0,7,3::Double]); res @?= [13,3,25,3::Double] }
    , testCase "map" $ do { (AA 2 [2, 2] res) <- fpAaa "test/data/map.🍏" (AA 2 [2,2] [1,2,3,4::Double]) (AA 1 [2] [3,5::Double]); res @?= [4,7,6,9::Double] }
    , testCase "luhn check" $ do { res <- fpAi "test/examples/luhn.🍎" [4,0,1,2,8,8,8,8,8,8,8,8,1,8,8,1]; res @?= 1 }
    , testCase "conv with stride" $ do
        (AA 2 [2,2] res) <- fpAa "test/data/strideConv.🍏"
            (AA 2 [4,4] [20::Double,200,-5,23,-13,134,119,100,120,32,49,25,-120,12,9,23])
        res @?= [200,119,120,49::Double]
    , testCase "ISBN-13" $ do
        res <- tfpAi "test/examples/isbn13.🍎"
            [ [9,7,8,0,5,9,6,5,2,8,1,2,6]
            , [9,7,8,0,5,9,6,5,2,8,1,2,0]
            , [9,7,8,1,7,8,8,3,9,9,0,8,1]
            , [9,7,8,1,7,8,8,3,9,9,0,8,3] ]
        res @?= [1,0,1,0]
    , testCase "mapAa" $ do { (AA 1 [2] res) <- fpAa "test/data/maa.🍎" (AA 2 [2,2] [1,2,3,4::Double]); res @?= [3,7::Double] }
    , testCase "mapAa" $ do { (AA 2 [3,2] res) <- fpAa "test/data/mfa.🍎" (AA 1 [3] [1,2,3::Double]); res @?= [1,1,2,2,3,3::Double] }
    , testCase "consSum" $ do { (AA 1 [3] res) <- fpAaa "test/data/consSum.🍏" (AA 1 [3] [1,0,0::Double]) (AA 2 [3,2] [2,3,4,5,6,9::Double]); res @?= [6,9,15::Double] }
    , testCase "cross" $ do { (AA 1 [3] res) <- fpAaa "test/data/cross.🍏" (AA 1 [3] [3,4,5::Double]) (AA 1 [3] [4,3,5::Double]); res @?= [5,5,-7::Double] }
    , testCase "gen." $ do { res <- fpFfa "test/data/gen.🍎" 1 (sqrt 2) ; last (hs2 <$> res) @?= (1.1981402347355923 :: Double, 1.1981402347355923 :: Double ) }
    , testCase "completeElliptic" $ do { res <- fpFf "math/completeElliptic.🍎" 0.8 ; res .?= completeElliptic 0.8 }
    , testCase "trainXor" $ do
        (AA 2 [2,2] res0, AA 1 [2] res1, AA 1 [2] res2, x) <- fpAaafp4 "test/data/trainXor.🍎" (AA 2 [2,2] [0.51426693,0.56885825,0.48725347,0.15041493]) (AA 1 [2] [0.14801747,0.37182892]) (AA 1 [2] [0.79726405,0.67601843]) 0.57823076
        res0 @?= [0.5130108836813994,0.563839153826952,0.48606794571593476,0.1463165649068566]
        res1 @?= [1.0692017538688703e-2,0.24098107852780348]
        res2 @?= [0.7927996818471371, 0.6633059586618876]
        x @?= 0.3988611249884681
    , testCase "elliptic fourier" $ do
        (AA 1 [2] coeffs, a, c) <- fpAaip3 "test/examples/ellipticFourier.🍎" [0,4,4::Double] [0,0,3::Double] 2
        a @?= 2.5000000000000004
        c @?= 1.0
        last coeffs @?= (-0.28876537338066266,-0.02632401569273178,0.10638724282445484,0.342212204005514)
    , testCase "ℯ_" $ do { fp <- fpn "[e:(_x)]"; ff fp 1 @?= exp (-1) }
    , testCase "ℯ" $ do { f <- fpn "e:"; ff f 2.5 @?= exp 2.5 }
    , testCase "k-l" $ do { res <- jitKl [0.25, 0.25, 0.5] [0.66, 0.33, 0] ; res @?= kl [0.25, 0.25, 0.5] [0.66, 0.33, 0] }
    , testCase "fizzbuzz" $ do { (AA 1 [10] res) <- fpAa "test/examples/fizzbuzz.🍎" (AA 1 [10] [0..9::Double]); res @?= [15.0,3.0,0.0,3.0,5.0,3.0,0.0,0.0,3.0,0.0::Double] }
    , testCase "filt" $ do { (AA 1 [10] res) <- fpAa "test/examples/partition.🍏" (AA 1 [10] [0..9::Double]); res @?= [F,F,F,F,F,F,T,T,T,T] }
    , testCase "softmax" $ do { (AA 2 [2,2] res) <- fpAa "test/data/softmax.🍎" (AA 2 [2,2] [0.25,0.75,0.3,0.5::Double]); res @?= [0.4875026035157896,0.5621765008857981,0.5124973964842103,0.4378234991142019::Double] }
    , testCase "gamma" $ do { res <- gammaJit (-3.5) ; res @?= gamma (-3.5) }
    , testCase "tcdf" $ do { res <- fpFff "math/tcdf.🍎" 2 12 ; res ≈ tcdf 12 2 }
    , testCase "fcdf" $ do { res <- fpFfff "math/fcdf.🍎" 5 2 2 ; res .?= 0.6339381452606089 }
    , testCase "chi-squared cdf" $ do { res <- fpFff "math/chisqcdf.🍎" 2 2 ; res @?= chisqcdf 2 2 }
    , testCase "chi-squared cdf" $ do { res <- fpFff "math/chisqcdf.🍎" 10 28 ; res @?= chisqcdf 10 28 }
    , testCase "ramanujan" $ do { res <- fpFf "test/examples/ramanujanFact.🍎" 7 ; res ≈ 5040 }
    , testCase "conv (1-d)" $
        let v = AA 1 [10] [1..10::Double]
        in do { res0 <- fpAa "bench/apple/maxWindow.🍎" v; res1 <- fpAa "bench/apple/convMax.🍏" v; (res0 :: AF) @?= res1 }
    , testCase "ix" $
        let v = AA 1 [20] [1..20::Int64]
        in do { res0 <- fpAa "bench/apple/evens.🍎" v; res1 <- fpAa "bench/apple/evenIx.🍎" v; (res0 :: AI) @?= res1 }
    , testCase "hypergeo" $ do { res <- fpAaff "math/hypergeometric.🍏" [1] [3/2] 1; res @?= hypergeometric [1] [3/2] 1 }
    , testCase "pearson r" $ do { res <- fpAaf "math/stats/r.🍎" [1,2,3,4,5,6,7] [10,9,2.5,6,4,3,2]; res @?= -0.8285038835884277 }
    , testCase "cosim" $ do { res <- fpAaf "math/cosim.🍏" [2,45,7,2] [2,54,13,15]; res @?= 0.9726896390141451 }
    , testCase "foldl" $ do { res <- fpAf "test/data/cfLeft.🍏" (4:replicate 5 8); res ≈ sqrt 17 }
    , testCase "cov" $
        let x = AA 2 [2,3] [-2.1,-1,4.3,3,1.1,0.12::Double]
        in do { (AA 2 [2,2] res) <- fpAa "math/stats/covar.🍏" x ; res @?= [11.71,-4.286,-4.286,2.144133::Double] }
    , rfTest
    ]

(≈) :: (Show a, Ord a, Floating a) => a -> a -> Assertion
x ≈ y = assertBool ("expected " ++ show y ++ ", got " ++ show x) ((x-y)/y<1e-4&&(y-x)/y<1e-4)

asN :: Storable a => U a -> IO [a]
asN = fmap asV.peek
asV (AA _ _ xs) = xs

fpAa fp x = wA x $ \pX -> do
    f <- fmap aa.fpn =<< BSL.readFile fp
    peek (f pX)

fpAaa fp x y =
    wA x $ \pX ->
        wA y $ \pY -> do
            f <- fmap aaa.fpn =<< BSL.readFile fp
            peek (f pX pY)

aaFp fp xs =
    let xA = v1 xs in
    wA xA $ \p -> do
        f <- fmap aa.fpn =<< BSL.readFile fp
        asN (f p)

tyS :: BSL.ByteString -> TestTree
tyS s = testCase "(expr)" $
    case tyExpr s of
        Left err -> assertFailure(show err)
        Right{}  -> assertBool "passed" True

tyF :: FilePath -> TestTree
tyF fp = testCase fp $ do
    res <- tyExpr <$> BSL.readFile fp
    case res of
        Left err -> assertFailure (show err)
        Right{}  -> assertBool "Passes" True

rfTest :: TestTree
rfTest = testCase "rising factorial" $ do
    res <- jitRF 5 15
    res @?= 5068545850368000

fpAf :: FilePath -> [Double] -> IO Double
fpAf fp xs = do
    f <- fmap af.fpn =<< BSL.readFile fp
    wA a $ \p -> do
        pure $ f p
  where
    a = v1 xs

jitKl = fpAaf "test/examples/kl.🍎"
jitB = fpAaf "test/examples/b.🍎"

tfpAi :: FilePath -> [[Int64]] -> IO [Int64]
tfpAi fp aas = do
    f <- fpn =<< BSL.readFile fp
    traverse (\arr -> let a=v1 arr in wA a $ \p -> pure $ ai f p) aas

fpAi :: FilePath -> [Int64] -> IO Int64
fpAi fp bs = the<$>tfpAi fp [bs] where the [x]=x; the _ = error "the"

v1 :: [a] -> Apple a
v1 xs = AA 1 [fromIntegral (length xs)] xs

fpAaafp4 :: FilePath -> AF -> AF -> AF -> Double -> IO (AF, AF, AF, Double)
fpAaafp4 fp xs ys zs w = do
    f <- fpn =<< BSL.readFile fp
    wA xs $ \pX -> wA ys $ \pY -> wA zs $ \pZ -> do
        (P4 pa0 pa1 pa2 x) <- peek (aaafp4 f pX pY pZ w)
        (,,,) <$> peek pa0 <*> peek pa1 <*> peek pa2 <*> pure x

fpAaip3 :: FilePath -> [Double] -> [Double] -> Int -> IO (Apple (Double, Double, Double, Double), Double, Double)
fpAaip3 fp xs ys n = do
    f <- fpn =<< BSL.readFile fp
    let a=v1 xs; b=v1 ys
    wA a $ \p ->
        wA b $ \q -> do
            (P3 pa x0 x1) <- peek (aaip3 f p q n)
            c <- peek pa
            pure (hs4<$>c, x0, x1)

-- leaks memory
fpn = fmap fst . case arch of {"aarch64" -> aFunP; "x86_64" -> funP}

fpAaf :: FilePath -> [Double] -> [Double] -> IO Double
fpAaf fp xs ys = do
    f <- fpn =<< BSL.readFile fp
    jitAaf f xs ys

fpAaff :: FilePath -> [Double] -> [Double] -> Double -> IO Double
fpAaff fp xs ys z = do
    f <- fmap aaff.fpn =<< BSL.readFile fp
    wA a $ \p -> wA b $ \q -> do
        pure $ f p q z
  where
    a=v1 xs; b=v1 ys

jitAaf :: FunPtr (U Double -> U Double -> Double) -> [Double] -> [Double] -> IO Double
jitAaf fp xs ys =
    let a=v1 xs; b=v1 ys in
    wA a $ \p -> wA b $ \q -> do
        pure $ aaf fp p q

jitExp :: Int64 -> Double -> IO Double
jitExp = fpIff "test/examples/exp.🍏"

fpFf :: FilePath -> Double -> IO Double
fpFf fp x = do
    f <- fpn =<< BSL.readFile fp
    pure $ ff f x

fpIff :: FilePath -> Int64 -> Double -> IO Double
fpIff fp x y = do
    f <- fpn =<< BSL.readFile fp
    pure $ iff f x y

fpIa :: Storable a => FilePath -> Int64 -> IO [a]
fpIa fp n = do
    f <- fpn =<< BSL.readFile fp
    asN (ia f n)

fpFfa :: Storable a => FilePath -> Double -> Double -> IO [a]
fpFfa fp x y = do
    f <- fpn =<< BSL.readFile fp
    asN (ffa f x y)

fpFff :: FilePath -> Double -> Double -> IO Double
fpFff fp x y = do
    f <- fpn =<< BSL.readFile fp
    pure $ fff f x y

fpFfff :: FilePath -> Double -> Double -> Double -> IO Double
fpFfff fp x y z = do
    f <- fpn =<< BSL.readFile fp
    pure $ ffff f x y z

gammaJit = fpFf "math/gamma.🍏"
ncdfJit = fpFf "math/ncdf.🍎"

erfJit :: Double -> IO Double
erfJit = fpFf "math/erf.🍏"

jitRF :: Int -> Int -> IO Int
jitRF m n = do
    fp <- fpn =<< BSL.readFile "test/examples/risingFactorial.🍎"
    pure $ runRF fp m n

wA :: Storable a => Apple a -> (U a -> IO b) -> IO b
wA x act =
    allocaBytes (sizeOf x) $ \p ->
        poke p x *> act p

hsIi :: FunPtr (Int64 -> Int64) -> Int -> Int
hsIi f = fromIntegral.ii f.fromIntegral

foreign import ccall "dynamic" ib :: FunPtr (Int64 -> CUChar) -> Int64 -> CUChar
foreign import ccall "dynamic" ii :: FunPtr (Int64 -> Int64) -> Int64 -> Int64
foreign import ccall "dynamic" ia :: FunPtr (Int64 -> U a) -> Int64 -> U a
foreign import ccall "dynamic" ai :: FunPtr (U a -> Int64) -> U a -> Int64
foreign import ccall "dynamic" af :: FunPtr (U a -> Double) -> U a -> Double
foreign import ccall "dynamic" aaf :: FunPtr (U a -> U a -> Double) -> U a -> U a -> Double
foreign import ccall "dynamic" aaff :: FunPtr (U a -> U a -> Double -> Double) -> U a -> U a -> Double -> Double
foreign import ccall "dynamic" ff :: FunPtr (Double -> Double) -> Double -> Double
foreign import ccall "dynamic" fff :: FunPtr (Double -> Double -> Double) -> Double -> Double -> Double
foreign import ccall "dynamic" ffff :: FunPtr (Double -> Double -> Double -> Double) -> Double -> Double -> Double -> Double
foreign import ccall "dynamic" ffa :: FunPtr (Double -> Double -> U a) -> Double -> Double -> U a
foreign import ccall "dynamic" iff :: FunPtr (Int64 -> Double -> Double) -> Int64 -> Double -> Double
foreign import ccall "dynamic" runRF :: FunPtr (Int -> Int -> Int) -> (Int -> Int -> Int)
foreign import ccall "dynamic" aa :: FunPtr (U a -> U b) -> U a -> U b
foreign import ccall "dynamic" aaa :: FunPtr (U a -> U b -> U c) -> U a -> U b -> U c
foreign import ccall "dynamic" aaafp4 :: FunPtr (U a -> U b -> U c -> Double -> Ptr (P4 (U d) (U e) (U f) g)) -> U a -> U b -> U c -> Double -> Ptr (P4 (U d) (U e) (U f) g)
foreign import ccall "dynamic" aaip3 :: FunPtr (U a -> U b -> Int -> Ptr (P3 c d e)) -> U a -> U b -> Int -> Ptr (P3 c d e)
