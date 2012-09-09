{-# LANGUAGE BangPatterns #-}
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed.Mutable as VM
import Control.Monad

--
-- Mutable Vector
--
mvec = do
    let n = 10000000 :: Int
	x = 0 :: Int
    vm <- VM.new n :: IO (VM.IOVector Int)
    mvec2 vm 0 n x
    VM.read vm (n-1) >>= print

mvec2 vm !i !n !x = do
    if i<n then do
	VM.unsafeWrite vm i x --(rem i 100) x
	mvec2 vm (i+1) n (i+1)
    else return ()

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

mvec4 = do -- SLOW
    let m = 1000
	n = 5000 -- outer loop
    vm <- VM.new m :: IO (VM.IOVector Int)
    {-
    forM [1..n] $ \j -> do
	forM [0..(m-1)] $ \i -> do
    -}
    forM [(i,j)| i<-[0..m-1],j<-[1..n]] $ \ (i,j) -> do
	    i `seq` j `seq` VM.unsafeWrite vm i (j*i) --(rem i 100) x
    VM.read vm (m-1) >>= print

mvec5  = do
    let n = 1000
	m = 5000
    vm <- VM.new m :: IO (VM.IOVector Int)
    mvec5' vm [(i,j)|i<-[0..m-1],j<-[1..n]]
    VM.read vm (m-1) >>= print

mvec5' _ [] = return ()
mvec5' vm ((i,j):inds) = do
    VM.unsafeWrite vm i (j*i)
    mvec5' vm inds


main = do
    mvec4
