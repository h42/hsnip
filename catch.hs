{-
-- Should use Control.Exception veraion - below
import System.IO.Error  -- catch is already in prelude
oldCatch = do
    catch
	(do
	    file <- readFile "xxx"
	    return ()
	)
	(\e ->
	    if isDoesNotExistError e then putStrLn "DoesNotExist"
	    else if isEOFError e then putStrLn "EOF"
	    else putStrLn "Misc error"
	)
  -}

import Prelude hiding(catch)
import Control.Exception

main = do
    catch
	(do
	    --s <- readFile "xxx"
	    return ()
        )
	(\e -> putStrLn ("Caught "++ show (e :: IOException)))

    rc <- try (readFile "xxx") :: IO (Either SomeException String)
    putStrLn $ show rc
