{-# LANGUAGE BangPatterns #-}

import System.Random
import Control.Monad.State


------------------------------
-- Random in state monad
------------------------------
getr :: State StdGen Int
getr = state (\st -> randomR (1,10) st)

getr2 = do
    a<-getr
    b<-getr
    return (a,b)

get2 = do
    g <-getStdGen
    print $ runState getr2 g
------------------------------

r2 :: Int -> StdGen -> Int
r2 0 g =  fst $ randomR (1,10) g
r2 n !g = r2 (n-1) g' where
    (!r,!g') = random g :: (Int,StdGen)
-------------------------------------------

r1 !g !0 = random g :: (Int,StdGen)
r1 !g !n = r1 g' (n-1) where
    (!r',!g') = random g  :: (Int,StdGen)

newg (x,!y) = (x+1,y+1) :: (Int,Int)  -- minimum you must make y strict
speedtest 0 g = fst g
speedtest n g = speedtest (n-1) (newg g)

main = do

    let n=12345678
    g<-newStdGen
    print $ r1 g n

    {-
    let x=speedtest n (0,0)
    putStrLn $ "Speedtest for n=" ++ show x
    -}
    return ()
