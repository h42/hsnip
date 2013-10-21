module Main (
    main
) where

import Control.Monad.Trans.Either
import Data.List (stripPrefix)
import Data.Char(isUpper)
import System.Exit (exitFailure)
import Test.QuickCheck.All (quickCheckAll)

getName :: IO (Either Int String)
getName = do
    putStr "Hurp: "
    s <- getLine
    return $ if isUpper (head s) then Right s
				 else Left 0

m :: IO (Either Int String)
m = runEitherT $ do
    n1 <- EitherT getName
    n2 <- EitherT getName
    return (n2 ++ n1)

main = do
    name <- m
    case name of
	Left n -> putStrLn $ "Left " ++ (show n)
	Right n -> putStrLn n


