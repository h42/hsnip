{-#  LANGUAGE ForeignFunctionInterface #-}

module Main (
    main
    ,newMat
    ,fromList
    ,toList
    ,getMatElem
    ,putMatElem
    ,addMat
    ,idMat
    ,zeroMat
) where

import Control.Monad
import Foreign
import Foreign.C
import MY.Show

{-data Hmat = Hmat {
    rows  :: Int
    ,cols :: Int
    ,buf  :: (Ptr CDouble)
}-}

foreign import ccall "getMatBuf" getMatBuf   :: Ptr a ->  IO (Ptr Double)
foreign import ccall "getMatElem" c_getElem
    :: Ptr a ->  CInt -> CInt -> IO Double
foreign import ccall "putMatElem" c_putElem
    :: Ptr a ->  CInt -> CInt -> CDouble -> IO ()
foreign import ccall "getMatRows" getMatRows :: Ptr a ->  IO CInt
foreign import ccall "getMatCols" getMatCols :: Ptr a ->  IO CInt
foreign import ccall "addMat" addMat         :: Ptr a -> Ptr a -> IO ()
foreign import ccall "idMat" idMat           :: Ptr a -> IO ()
foreign import ccall "newMat" newMat_c  :: CInt -> CInt ->  IO (Ptr a)
foreign import ccall "transMat" transMat       :: Ptr a -> IO ()
foreign import ccall "zeroMat" zeroMat       :: Ptr a -> IO ()

getMatElem m r c =
    fmap realToFrac $ c_getElem m (fromIntegral r) (fromIntegral c)

putMatElem m r c d =
    c_putElem m (fromIntegral r) (fromIntegral c) (realToFrac d)

newMat :: Int -> Int -> IO (Ptr a)
newMat r c = newMat_c (fromIntegral r) (fromIntegral c)

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

showMat m dplaces = do
    r <- getMatRows m
    c <- getMatCols m
    ptr <- getMatBuf m
    a <- peekArray (fromIntegral $ r*c) ptr
    let l = 7 -- dp + 1 + (length $ show $ ceiling $ maximum a)
	a2 = map (fshow (l,dplaces)) a
    return $ concat $ showCR a2 1 c

showCR [] i c = []
showCR (x:xs) i c
    | i /= c = x : showCR xs (i+1) c
    | otherwise = x: "\n" : showCR xs 1 c

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
