import Control.Concurrent

initLogger = do
    mv <- newEmptyMVar
    forkIO (logger mv)
    return mv

logger mv = loop where
    loop = do
        sx <- takeMVar mv
        case sx of
            "" -> return ()
            _ -> do
                putStrLn sx
                loop

logMessage mv s = do
    putMVar mv s

logQuit mv = logMessage mv ""

main = do
    mv <- initLogger
    logMessage mv "hey"
    logMessage mv "now"
    logQuit mv
