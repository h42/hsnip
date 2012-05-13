{-# LANGUAGE ForeignFunctionInterface #-}
import Foreign
import Foreign.C.Types
import Foreign.C.String
-- import Foreign.Ptr
import Foreign.Marshal.Array

import Text.Printf

--
-- foreign import ccall <"external c file name"> 
--                      <internal c call name> :: type signature
-- Haskell function can have same name as external c function if you rename
-- it on the import call
--
foreign import ccall "getfilemode" c_getfilemode :: CString -> IO CInt
foreign import ccall "setfilemode" c_setfilemode :: CString -> CInt -> IO CInt

getfilemode :: String -> IO Int
getfilemode fn = do
    s <- newCString fn
    m <- c_getfilemode s
    free s --Foreign.Marshal.Alloc.free 
    return (fromIntegral m)

setFileMode :: String -> Int -> IO Int
setFileMode fn m = do
    s <- newCString fn
    rc <- c_setfilemode s (fromIntegral m)
    free s --Foreign.Marshal.Alloc.free 
    return (fromIntegral rc)

-- Floating Point
foreign import ccall "math.h sin" c_sin :: CDouble -> CDouble
fastSin :: Double -> Double
fastSin x = realToFrac (c_sin (realToFrac x))

--Array Input
foreign import ccall "print_array" c_print_array :: CInt -> Ptr (CInt) -> IO ()
h_print_array = do
    let a = [1..6] :: [CInt]
	n = fromIntegral (length a)  :: CInt
    ptr <- newArray a
    c_print_array n ptr

-- String Output
foreign import ccall "get_string" c_get_string :: IO CString
h_get_string :: IO String
h_get_string = do
    cs <- c_get_string
    peekCString cs

-- MallocBytes
foreign import ccall "get_bytes" c_get_bytes :: CString -> IO CString
h_get_bytes :: String -> IO String
h_get_bytes hs = do
    cs <- newCString hs
    p1 <- mallocBytes 1000 :: IO (Ptr CChar)
    copyArray p1 cs (length hs) 
    s1 <- c_get_bytes p1
    s <- peekCString s1
    free p1
    return s

main = do
    rc <- setFileMode "temp" 0o444
    print rc
    m <- getfilemode "temp"
    printf "%o\n" m
    rc <- setFileMode "temp" 0o664
    print rc
    m <- getfilemode "temp"
    printf "%o\n" m

    -- Test fastSin
    putStrLn $ show (fastSin(30/180*pi))

    
    h_print_array
    h_get_string >>= putStrLn
    h_get_bytes "byte test - " >>= putStrLn
