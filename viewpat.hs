{-# LANGUAGE ViewPatterns #-}
import qualified Data.Text as T
import qualified Data.Text.IO as T

t1 :: T.Text
t1 = "hey"

pat :: T.Text -> Int -> Char
pat (T.uncons -> Nothing) i = 'e'
pat (T.uncons -> Just (x,bs)) ((*2)-> 4) = x

main = do
    putStrLn $ T.unpack t1
    T.putStrLn t1
    T.putStrLn "hey"
