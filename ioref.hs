import Data.IORef
import Foreign

data III = III String deriving (Eq,Show)

test :: IORef String
test = unsafePerformIO $ newIORef ""
--test = newIORef ""

main2 = do
	writeIORef test "42"
	bang <- readIORef test
	print bang --(bang :: [Char])
	putStrLn bang


main1 = do
	ref <- newIORef "hey"
	s <- readIORef ref
	putStrLn s
	writeIORef ref "now"
	s2 <- readIORef ref
	putStrLn s2

