{-# LANGUAGE ForeignFunctionInterface #-}
import Control.Applicative
import Foreign
import Foreign.C.Types
import Foreign.C.String
import Foreign.Ptr
-- import Foreign.Marshal.Array

--
--
foreign import ccall "dcadd"
     c_dcadd :: CDouble -> CDouble -> CDouble

dcadd :: Double -> Double -> Double
dcadd x y = realToFrac (c_dcadd (realToFrac x) (realToFrac y))

--
--
foreign import ccall "dcmul"
     c_dcmul :: CInt -> CDouble -> CInt

dcmul :: CInt -> Double -> Int
dcmul x y = fromIntegral (c_dcmul (fromIntegral x) (realToFrac y))

--
--
foreign import ccall "intptr"
    c_intptr :: Ptr CInt -> IO Int

intptr :: IO (Int, Int, Int)
intptr = do
    ptr <- mallocArray 3  :: IO (Ptr CInt)
    rc  <- c_intptr ptr
    xp2 <- peekArray 2 ptr
    free ptr
    return (rc,fromIntegral $ head xp2,fromIntegral $ xp2 !! 1)

--
--
foreign import ccall "doubleptr" c_doubleptr :: Ptr CDouble -> IO Int
doubleptr rows cols = do
    ptr <-mallocArray (rows*cols) :: IO (Ptr CDouble)
    return 0



--
--
main = do
    print $ dcadd 2.0 4.5
    print $ dcmul 3 4
    t<-intptr
    print t
