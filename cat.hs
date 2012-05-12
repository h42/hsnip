import IO
import System.Environment

pfile [] = return ()
pfile (fn:fns) = do
    catch (readFile fn >>= putStr)
	  (\e -> putStrLn $ "*** Error reading File " ++ fn)
    pfile fns

main = do
    getArgs >>= pfile
