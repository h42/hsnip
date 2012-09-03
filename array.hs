import Data.Array
import Data.Array.IO

-- STArray is in starray.hs

aproc = do
    let a=listArray ((1,1),(2,5)) [1..10]
    --print a
    print $ a ! (2,1)

ma = newArray ((1,1),(2,10)) 0 :: IO (IOArray (Int,Int) Int)

maproc = do
    a <-ma
    x <- readArray a (2,2)
    print x
    writeArray a (2,2) 42
    readArray a (2,2) >>= print


main = do
    st
    --aproc
    --maproc
