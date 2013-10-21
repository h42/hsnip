-- Pattern Guards included in Haskewll 2010
import Data.List (nub)

strangeOperation :: [Int] -> Ordering
strangeOperation xs | 7  <- sum xs
                    , n  <- length xs
                    , n  >= 5
                    , n  <= 20
                      = EQ
                    | 1  <- sum xs
                    , 18 <- length xs
                    , r  <- nub xs `compare` [1,2,3]
                    , r /= EQ
                      = r
                    | otherwise
                      = [3,1,2] `compare` xs

--main = print $ strangeOperation ([5,7..21] ++ [20,19..4])


main = do
    print $ 42

