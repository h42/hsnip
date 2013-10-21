import Control.Concurrent

xxx = do
    mv <- newEmptyMVar
    forkIO (yyy mv)
    mv2<-takeMVar mv
    return mv2

yyy mv = do
    c <- getLine
    putMVar mv (read c :: Int)

main = do
    mv <- xxx
    print mv

