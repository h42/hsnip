import Control.Concurrent
import Control.Exception

-- SEE CHAN.HS FOR CHANNELS EXAMPL

-- +RTS -N ?? -RTS must be include and prog compiled with -threaded to use xtra cores

primes :: Int -> [Int]
primes nn = 2 : filter (isPrime $ primes nn) [3,5..nn]  where
  isPrime (p:ps) n
	| mod n p == 0 = False
	| p*p > n      = True
	| otherwise    = isPrime ps n
  isPrime [] _ = False -- never used, but avoids compiler warning

cproc n = do
	let x = sum $ primes n
	putStrLn $ "Sum is " ++ (show x)
	return ()

myForkIO :: IO () -> IO (MVar ())
myForkIO f = do
	mvar <- newEmptyMVar
	forkIO (f `finally` putMVar mvar ())
	return mvar

main = do
	let n = 5000000
	-- same answer because n is how far to look ; not how many primes
	-- inless we have twin primes around n
	m <- myForkIO (cproc n)
	m2 <- myForkIO (cproc (n+1))
	m3 <- myForkIO (cproc (n+2))
	m4 <- myForkIO (cproc (n+3))
	-- GOES REAL SLOW WITHOUT +RTS -N n -RTS
	takeMVar m
	takeMVar m2
	takeMVar m3
	takeMVar m4
	return ()
