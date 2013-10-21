{-# LANGUAGE BangPatterns #-}
import Control.Monad
import Control.Monad.ST
import Data.Array.ST
import Data.Array
--import Data.Array.Unboxed

--a1 :: Int -> STArray Int Int
a1 n = runSTArray $ do
    a <- newArray (1,n) 0
    forM_ [1..n] $ \i -> do
	writeArray a i (i)
    forM_ [1..1000] $ \j -> do
        forM_ [1..n] $ \i -> do
	    x <- readArray a i
	    writeArray a i (x+1)
    return a

speedy = do
    ans <-speedy' [(i,j)|i<-[1..10000],j<-[1..1000]] 0
    print ans

speedy' [] ans = return ans
speedy' ((i,j):is) !ans = speedy' is (ans+1)

speed2 = do
    ans <-speed2' 10000000 0
    print ans

speed2' 0 ans = return ans
speed2' n !ans = speed2' (n-1) (ans+1)

speed3 = do
    let ans = speed3' [(i,j)|i<-[1..10000],j<-[1..1000]] 0
    print ans

speed3' [] ans = ans
speed3' ((i,j):is) !ans = speed3' is (ans+1)

main = do
    let n = 5000 -- slow
	a = a1 n
    print $ a ! n
   -- speed3 >>= print
