import Control.Monad
import Data.Char
import System.IO

withTest = do
    s2 <- withFile "writer.hs" ReadMode $ \h -> do
	      s <- hGetContents h
	      --putStr s
	      seq s (return s)
    putStr (unlines $ take 10 $ lines s2)
    rs <- r
    mapM putStrLn (map (show.(`mod` 100)) rs)
    return ()

tWhen = do
    putStrLn "Enter number"
    ns <- getLine
    when (all isDigit ns) $ do
	let n=read ns :: Int
	putStrLn $ show n
    unless (all isDigit ns) $ do
	putStrLn "Bad boy"
    mapM print [1..5]

main = do
    tWhen
