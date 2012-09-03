import Control.Monad
--import Control.Monad.ST
import Data.Array.ST
import Data.Array.Unboxed

a1 :: Int -> UArray Int Int
a1 n = runSTUArray $ do
    a <- newArray (1,n) 0
    forM [1..n] $ \i -> do
	writeArray a i (i)
    forM [1..10] $ \j -> do
	forM [1..n] $ \i -> do
	    x <- readArray a i
	    writeArray a i (x+1)
    return a

main = do
    let n = 500000 -- slow
	a = a1 n
    print $ a ! n
