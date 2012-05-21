import Control.Monad.Writer
import Debug.Trace

{-
newtype Writer w a = Writer { runWriter :: (a, w) }
instance (Monoid w) => Monad (Writer w) where
    return x = Writer (x, mempty)
    Writer (x,v) >>= f = let Writer (y, v') = f x
			      in Writer (y, v `mappend` v')
-}

logNumber :: Int -> Writer [String] Int
logNumber x = writer (x, ["Got number: " ++ show x])

multWithLog :: Writer [String] Int
multWithLog = do
    a <- logNumber 3
    b <- logNumber 5
    c <- logNumber 7
    traceShow (a,b,c)  tell ["product of a,b,c"]
    return (a*b*c) >>= incNumber  -- increments product fo 1st 3 nums
    -- above return just boxes (a*b*c) - nothing to with return type
    -- incNumber result is returned result
    -- TELL can not be last function as it will return wrong type


incNumber :: Int -> Writer [String] Int
incNumber x = writer (x+1, ["incremented " ++ (show x)] )

main = do
    let x = multWithLog
    print $ runWriter x

    print $ fst $ runWriter multWithLog
    --mapM_ print $ snd $ runWriter multWithLog

mygcd :: Int -> Int -> Int
mygcd a b
    | b == 0 = a
    | otherwise = mygcd b $ mod a b

wrgcd :: Int -> Int -> Writer [String] Int
wrgcd a b
    | b == 0 = do
	tell ["The gcd is " ++ (show a)]
	return a
    | otherwise = do
	tell ["xxx"]
	wrgcd b $ mod a b

-- Without do
wrgcd' :: Int -> Int -> Writer [String] Int
wrgcd' a b
    | b == 0 = writer (a, ["The answer is " ++ (show a)] )
    | otherwise =
	tell ["gcd' of " ++ (show a) ++ " and " ++ (show b)] >>=
	\x -> wrgcd' b $ mod a b

--mapM_ putStrLn $ snd $ runWriter $ wrgcd' 128 120 -- will print the log
