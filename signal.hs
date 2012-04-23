import System.Posix.Signals
import Control.Concurrent
import Data.IORef
import Foreign

zsignal :: IORef Int
zsignal = unsafePerformIO $ newIORef 0

handler :: IO ()
handler = writeIORef zsignal 1

initSignal :: [Signal] -> IO ()
initSignal [] = return ()
initSignal (s:sx) = do
    installHandler s (Catch handler) Nothing
    initSignal sx

gotSignal :: IO Int
gotSignal = readIORef zsignal

mloop = do
    threadDelay 1
    x <- gotSignal
    if x==0 then mloop
    else do
	putStrLn "Shutting down"
	return ()

main = do
    initSignal [sigQUIT,sigINT]
    mloop
