import Foreign
import Foreign.C

data Hmat = Hmat {
    rows :: Int
    ,cols :: Int
    ,buf :: (Ptr CDouble)
}

newHmat :: Int -> Int -> IO Hmat
newHmat r c = do
    ptr <- mallocArray (r*c) :: IO (Ptr CDouble)
    return $ Hmat r c ptr

get :: Hmat -> Int -> Int -> IO Double
get (Hmat r c ptr) row col = do
    rc <- fmap realToFrac (peekElemOff ptr (row*c + col))
    --let rc =2.1
    print "hey"
    print rc
    return rc

put :: Hmat -> Int -> Int -> Double -> IO ()
put (Hmat r c ptr) row col x = do
    pokeElemOff ptr (row*c + col) (realToFrac x)
    return ()

main = do
    putStrLn "hey"
    m <- newHmat 3 4
    put m 2 2 27.1
    get m 2 2 >>= print
