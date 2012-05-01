module Logger (
    Logger
    ,Log
    ,record
    ,runLogger
) where

type Log = [String]
newtype Logger a = Logger { execLogger :: (a, Log) } deriving (Show)

runLogger :: Logger a -> (a, Log)
runLogger = execLogger

record :: String -> Logger ()
record s = Logger ((), [s])

instance Monad Logger where
    return a = Logger (a, [])
    m >>= k = let (a, w) = execLogger m
		  n = k a
		  (b, x) = execLogger n
	      in Logger (b, w ++ x)

------------------
--  TEST
------------------

test1 :: Int -> Int -> Logger Int
test1 a b = do
    let r = mod a b
    if r == 0 
	then do 
	    record $ "The ans is " ++ show b
	    return b
	else do
	    record $ show a ++ " " ++ show b
	    test1 b r


main = do
    let 
	t = return True :: Logger Bool
	f = return False :: Logger Bool

	fn :: Bool -> Logger Bool 
	fn b = Logger ( b, if b==True then ["True"] else ["False"] )

	l1 = fn False >> fn True >> fn True
    print l1

    let l2 = do
	    record "Initial"
	    fn True
	    fn False
	    fn True
    print $ snd $ runLogger l2

    return () :: IO ()
