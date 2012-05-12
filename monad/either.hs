instance Monad (Either l) where
    Left  x >>= f  = Left x
    Right x >>= f  = f x
    return = Right

fe :: (Num a, Ord a) => a -> Either [Char] a
fe x = if x<15 then Right (2*x+1) else Left ( (show x) ++ " is to big")

main = do
    putStrLn "hey"
