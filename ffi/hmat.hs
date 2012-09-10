import Control.Monad
import Foreign
import Foreign.C

data Hmat = Hmat {
    rows :: Int
    ,cols :: Int
    ,buf :: (Ptr CDouble)
}

fromList :: Int -> Int -> [Double] -> IO Hmat
fromList r c xs = do
    ptr <- mallocArray (r*c) :: IO (Ptr CDouble)
    when (not.null $ xs) $ pokeArray ptr (map realToFrac (take (r*c) xs))
    return $ Hmat r c ptr

toList :: Hmat -> IO [Double]
toList (Hmat r c ptr) = do
    xs <- (peekArray (r*c) ptr)
    return $ map realToFrac xs

get :: Hmat -> Int -> Int -> IO Double
get (Hmat r c ptr) row col = fmap realToFrac (peekElemOff ptr (row*c + col))

put :: Hmat -> Int -> Int -> Double -> IO ()
put (Hmat r c ptr) row col x = pokeElemOff ptr (row*c + col) (realToFrac x)

foreign import ccall "addMat" addMat_c
    :: Int -> Int -> Ptr CDouble -> Ptr CDouble -> IO ()

addMat m1 m2 = addMat_c (rows m1) (cols m1) (buf m1) (buf m2)

main = do
    m <- fromList 3 4 [1,3..]
    put m 2 2 27.1
    get m 0 3 >>= print
    get m 2 2 >>= print
    m2 <- fromList 3 4 [2,4..]
    addMat m m2
    toList m >>= print
    return ()
