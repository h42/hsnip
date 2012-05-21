instance Monad (Either l) where
    Left  x >>= f  = Left x
    Right x >>= f  = f x
    return = Right

fe :: (Num a, Ord a) => a -> Either [Char] a
fe x = if x<15 then Right (2*x+1) else Left ( (show x) ++ " is to big")

--
-- Recursion with either
--
evens [] = Right []
evens (13:xs) = Left "13"
evens (x:xs) =
    case (evens xs) of
	Right res -> Right (if even x then (x : res)  else res)
	Left res  -> Left res

--
-- Early Exit
--
e2 xs = e2' xs (Right [])
e2' _ (Left res)   = Left res
e2' [] (Right res) = Right res
e2' (13:xs) _ = Left "13"
e2' (x:xs) (Right res) =
    e2' xs (if even x then (Right (x:res)) else Right res)

main = do
    putStrLn "hey"
