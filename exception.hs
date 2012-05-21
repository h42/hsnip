{-# LANGUAGE ScopedTypeVariables #-}
-- Old catch in prelude. Should use Control.Exception version - below
import Control.Exception
import Control.Monad.Error
import qualified System.IO.Error as OLD

--
-- OLD deprecated routines in Prelude and System.IO.Error
--
oldCatch = do
    OLD.catch
	(do
	    file <- readFile "xxx"
	    return ()
	)
	(\e ->
	    if OLD.isDoesNotExistError e then putStrLn "DoesNotExist"
	    else if OLD.isEOFError e then putStrLn "EOF"
	    else putStrLn "Misc error"
	)

--
-- New stuf the hard way
--
hardway = do
    handle
	-- (\e -> putStrLn ("Caught "++ show (e :: IOException)))
	( (\e -> putStrLn "Caught Exception") :: IOException -> IO () )
	(do
	    s <- readFile "xxx"
	    return ()
	)

    rc <- try (readFile "xxx") :: IO (Either SomeException String)
    putStrLn $ show rc

--
-- New routines made easy
--
type Etype a = Either String a

myhandle :: String -> IO (Etype a) -> IO (Etype a)
myhandle emsg func = do
    handle (\(e :: IOException) -> return (Left emsg))
	func -- should return (Right a)

mytest :: IO ()
mytest = do
    a <- myhandle "readFile failed" $ do
	--s <- readFile "xxx"
	s <- fmap (length.lines) (readFile "either.hs")
	return (Right s)
    print a

main = mytest
