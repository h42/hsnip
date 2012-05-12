import System.Posix.Signals
import Control.Concurrent
import Data.IORef
import Foreign

data Sigs = Sigs {sigint::Int, sigquit::Int}

zsignal :: IORef Sigs
zsignal = unsafePerformIO $ newIORef (Sigs 0 0)

handler :: IO ()
handler = writeIORef zsignal (Sigs 1 1)

initSignal :: [Signal] -> IO ()
initSignal sx = mapM_ (\s->installHandler s (Catch handler) Nothing) sx

gotSignal :: IO (Sigs)
gotSignal = readIORef zsignal

mloop = do
    threadDelay 1
    Sigs x y <- gotSignal
    if x==0 then mloop
    else do
	putStrLn "Shutting down"
	return ()

main = do
    initSignal [sigQUIT,sigINT]
    mloop
