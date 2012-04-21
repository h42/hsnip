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

newg (x,!y) = (x+1,y+1) :: (Int,Int)  -- minimum you must make y strict
speedtest 0 g = fst g
speedtest n g = speedtest (n-1) (newg g)

main = do
    get2
    --rs <- getStdGen >>= return . r2 10
    g <- getStdGen
    let r = r2 100000 g
    print r

    {-
    let n=123456789
    let x=speedtest n (0,0)
    putStrLn $ "Speedtest for n=" ++ show x
    -}
    return ()
