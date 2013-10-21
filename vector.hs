{-# LANGUAGE BangPatterns #-}
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed.Mutable as VM
import Control.Monad

--
-- Pure Vector
--
t0 = V.replicate 1000 0
t1 = V.enumFromN 1 1000
t2 = V.generate 100 (^2)
t3 = V.empty
t4 = V.singleton 42
t5 = V.take 12 t1
t6 = t2 V.++ t2
t7 = V.map (*2) t1
t8 = V.filter (<50) t1
t9 = V.foldl (+) 0 t1
t10 = V.zip t1 t2
t11 = t1 V.! 4
t12 = V.head t1
t14 = V.tail t1
t15 = V.last t1
t16 = V.init t1
t17 = V.length t1

--
-- Mutable Vector
-- USE UNBOXED AND forM_ - UNDERSCORE VERSION for speed
--
mvec8 = do
    let m = 1000
        n = 5000
    vm <- VM.new n :: IO (VM.IOVector Int)
    forM_ [0..n-1] $ \j -> VM.unsafeWrite vm j (j)
    forM_ [1..m] $ \i -> do
        forM_ [0..n-1] $ \j -> do
            x<-VM.unsafeRead vm j
            VM.unsafeWrite vm j (x+1)
    VM.read vm (n-1) >>= print

main = do
    mvec8
