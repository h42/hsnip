{-#  LANGUAGE ForeignFunctionInterface #-}

import Control.Monad
import Foreign
import Foreign.C
import MY.Show

data Hmat = Hmat {
    rows  :: Int
    ,cols :: Int
    ,buf  :: (Ptr CDouble)
}

foreign import ccall "newMat" newMat_c  :: CInt -> CInt ->  IO (Ptr a)
newMat :: Int -> Int -> IO (Ptr a)
newMat r c = newMat_c (fromIntegral r) (fromIntegral c)

foreign import ccall "getMatBuf" getMatBuf   :: Ptr a ->  IO (Ptr Double)
foreign import ccall "getMatRows" getMatRows :: Ptr a ->  IO CInt
foreign import ccall "getMatCols" getMatCols :: Ptr a ->  IO CInt
foreign import ccall "addMat" addMat         :: Ptr a -> Ptr a -> IO ()
foreign import ccall "idMat" idMat           :: Ptr a -> IO ()
foreign import ccall "transMat" transMat       :: Ptr a -> IO ()
foreign import ccall "zeroMat" zeroMat       :: Ptr a -> IO ()

fromList :: Int -> Int -> [Double] -> IO (Ptr a)
fromList r c xs = do
    m <- newMat r c
    ptr <- getMatBuf m
    --ptr <- mallocArray (r*c) :: IO (Ptr CDouble)
    when (not.null $ xs) $ pokeArray ptr (map realToFrac (take (r*c) xs))
    return m

toList :: Ptr a -> IO [Double]
toList m = do
    r <- getMatRows m
    c <- getMatCols m
    ptr <- getMatBuf m
    xs <- (peekArray (fromIntegral (r*c)) ptr)
    return $ map realToFrac xs

{-
get :: Hmat -> Int -> Int -> IO Double
get (Hmat r c ptr) row col = fmap realToFrac (peekElemOff ptr (row*c + col))

put :: Hmat -> Int -> Int -> Double -> IO ()
put (Hmat r c ptr) row col x = pokeElemOff ptr (row*c + col) (realToFrac x)

foreign import ccall "addMat" addMat_c
    :: Int -> Int -> Ptr CDouble -> Ptr CDouble -> IO ()
addMat m1 m2 = addMat_c (rows m1) (cols m1) (buf m1) (buf m2)

foreign import ccall "idMat" idMat_c :: Int -> Int -> Ptr CDouble -> IO ()
idMat m1 = idMat_c (rows m1) (cols m1) (buf m1)

foreign import ccall "transMat" transMat_c
    :: Int -> Int -> Ptr CDouble -> IO ()
transMat (Hmat r c ptr) = do
    transMat_c r c ptr
    return $ Hmat c r ptr

showMat (Hmat r c ptr) dplaces = do
    a <- peekArray (r*c) ptr
    let l = 7 -- dp + 1 + (length $ show $ ceiling $ maximum a)
	a2 = map (fshow (l,dplaces)) a
    return $ concat $ showCR a2 1 c

showCR [] i c = []
showCR (x:xs) i c
    | i /= c = x : showCR xs (i+1) c
    | otherwise = x: "\n" : showCR xs 1 c
-}

main = do
    m <- fromList 4 4 [1,3..]
    addMat m m
    ds <- toList m
    print ds
    {-
    put m 2 2 27.1
    get m 0 3 >>= print
    get m 2 2 >>= print
    m2 <- fromList 4 4 [2,4..]
    addMat m m2
    toList m >>= print
    showMat m 0 >>= putStrLn
    transMat m
    showMat m 0 >>= putStrLn
    idMat m
    showMat m 0 >>= putStrLn
    -}
    return ()
