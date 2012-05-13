import System.Random
import Data.List

quicksort :: Ord a => [a] -> [a]
quicksort []     = []
quicksort (p:xs) = (quicksort lesser) ++ [p] ++ (quicksort greater) where
    lesser  = filter (< p) xs
    greater = filter (>= p) xs

main = do
    g <- getStdGen
    let rs = take 1000000 (randomRs (1,100000) g) :: [Int]
    print $ length $ sort rs
