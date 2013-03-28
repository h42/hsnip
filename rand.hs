import Data.List
import Foreign.C.Types

{-
    I had to use foldl' instead of sum to avoid blown stack.
    Also, fromIntegral was not needed if I used fold instead of sum.
    If you need fromIntegral, map it over array, not over each element.
-}


foreign import ccall "stdlib.h srand" srand :: CInt -> IO ()
foreign import ccall "stdlib.h rand" rand :: IO CInt

ls =  reverse . sort $ [x | x<-[1..1000000]];

rands :: Int -> [CInt] -> IO [CInt]
rands 0 rs = return rs
rands n rs = do
    r<-rand
    rands (n-1) (r:rs)

main = do
    srand 5
    --rs<-fmap (map fromIntegral) (rands 1000000 [])
    rs<-rands 1000000 []
    print $ foldl' (+) 0 rs
