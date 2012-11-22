{-#  LANGUAGE ForeignFunctionInterface #-}

module Main (
    main
    ,newMat
    ,fromList
    ,toList
    ,getBuf
    ,getRows
    ,getCols
    ,getElem
    ,putElem
    ,showMat

    ,addMat
    ,eliminate
    ,idMat
    ,incMat
    ,multMat
    ,scaleMat
    ,transMat
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

foreign import ccall "getBuf" getBuf   :: Ptr a ->  IO (Ptr CDouble)
foreign import ccall "getRows" getRows :: Ptr a ->  IO CInt
foreign import ccall "getCols" getCols :: Ptr a ->  IO CInt
foreign import ccall "getElem" c_getElem  :: Ptr a ->  CInt -> CInt -> IO Double
foreign import ccall "putElem" c_putElem
    :: Ptr a ->  CInt -> CInt -> CDouble -> IO ()
foreign import ccall "addMat" addMat         :: Ptr a -> Ptr a -> IO ()
foreign import ccall "determinate" c_determinate :: Ptr a -> IO CInt
foreign import ccall "eliminate" c_eliminate :: Ptr a -> IO CInt
foreign import ccall "multMat" c_multMat
    :: Ptr a -> Ptr a -> Ptr a -> IO (CInt)
foreign import ccall "incMat" c_incMat       :: Ptr a -> CDouble -> IO ()
foreign import ccall "scaleMat" c_scaleMat   :: Ptr a -> CDouble -> IO ()
foreign import ccall "solve" c_solve :: Ptr a -> Ptr CDouble -> IO CInt
foreign import ccall "idMat" idMat           :: Ptr a -> IO ()
foreign import ccall "newMat" newMat_c  :: CInt -> CInt ->  IO (Ptr a)
foreign import ccall "transMat" transMat       :: Ptr a -> IO ()
foreign import ccall "zeroMat" zeroMat       :: Ptr a -> IO ()

getElem m r c =
    fmap realToFrac $ c_getElem m (fromIntegral r) (fromIntegral c)

putElem m r c d =
    c_putElem m (fromIntegral r) (fromIntegral c) (realToFrac d)

newMat :: Int -> Int -> IO (Ptr a)
newMat r c = newMat_c (fromIntegral r) (fromIntegral c)

fromList :: Int -> Int -> [Double] -> IO (Ptr a)
fromList r c xs = do
    m <- newMat r c
    ptr <- getBuf m
    --ptr <- mallocArray (r*c) :: IO (Ptr CDouble)
    when (not.null $ xs) $ pokeArray ptr (map realToFrac (take (r*c) xs))
    return m

toList :: Ptr a -> IO [Double]
toList m = do
    r <- getRows m
    c <- getCols m
    ptr <- getBuf m
    xs <- (peekArray (fromIntegral (r*c)) ptr)
    return $ map realToFrac xs

showMat m dplaces = do
    r <- getRows m
    c <- getCols m
    ptr <- getBuf m
    a <- peekArray (fromIntegral $ r*c) ptr
    let l = 7 -- dp + 1 + (length $ show $ ceiling $ maximum a)
	a2 = map (fshow (l,dplaces)) a
    return $ concat $ showCR a2 1 c

showCR [] i c = []
showCR (x:xs) i c
    | i /= c = x : showCR xs (i+1) c
    | otherwise = x: "\n" : showCR xs 1 c

multMat m m1 m2 = do
    rc <- c_multMat m m1 m2
    let ans = if rc /=0 then Nothing else Just rc where
    return ans

incMat m x = c_incMat m (realToFrac x)
scaleMat m x = c_scaleMat m (realToFrac x)

eliminate m = do
    rc <- fmap fromIntegral $ c_eliminate m
    return $ if rc/=0 then Nothing else Just rc

solve m = do
    rc <- fmap fromIntegral $ c_eliminate m
    if rc /= 0 then return Nothing
    else do
	r <- fmap fromIntegral $ getRows m
	xv <- mallocArray r :: IO (Ptr CDouble)
	rc' <- c_solve m xv
	if rc'/=0 then return Nothing
	else do
	    xl <- peekArray r xv
	    free xv
	    return   $ Just $ map realToFrac xl

main = do
    m <- fromList 2 2 [1,3..]
    addMat m m
    --ds <- toList m
    --print ds
    showMat m 1 >>= putStrLn
    scaleMat m 0.5 >> showMat m 1 >>= putStrLn
    incMat m (-2) >> showMat m 1 >>= putStrLn
    transMat m >> showMat m 1 >>= putStrLn
    zeroMat m >> showMat m 1 >>= putStrLn
    idMat m >> showMat m 1 >>= putStrLn
    m1 <- fromList 2 2 [1..]
    m2 <- fromList 2 2 [1..]
    transMat m2
    m3 <- newMat 2 2
    showMat m1 1 >>= putStrLn
    showMat m2 1 >>= putStrLn
    multMat m3 m1 m2  >> showMat m3 1 >>= putStrLn

    putStrLn "Eliminate ---------"
    m4<-fromList 2 3 [4,3,10, 6,2,10]
    eliminate m4 >> showMat m4 1 >>= putStrLn
    solve m4 >>= print
    putStrLn ""
    m5<-fromList 2 2 [1..4]
    eliminate m5 >> showMat m5 1 >>= putStrLn

    putStrLn "Determinate ---------"
    m6<-fromList 4 4 [1,4,-2,3, 2,2,0,4, 3,0,-1,2, 1,2,2,-3]
    c_determinate m6 >>= print
    showMat m6 1 >>= putStrLn
    {-
    put m 2 2 27.1
    get m 0 3 >>= print
    get m 2 2 >>= print
    -}
    return ()
