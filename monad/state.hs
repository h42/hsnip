import Control.Monad.State

push :: Int -> State [Int] ()
push x = state $ \xs -> ((),(x:xs))
push2 x = do
    xs <- get2
    put (x:xs)

pop :: State [Int] Int
pop = state $ \(x:xs) -> (x,xs)

pop2 :: State [Int] Int
pop2 = do
    (x:xs) <- get2
    put xs
    return x

get2 :: State s s
get2 = state $ \s -> (s,s)

put2 :: s -> State s ()
put2 x = state $ \s -> ((),s)

doit = do
    mapM_ push [1..10]
    a<-pop
    pop2
    pop2
    pop2
    return a

main = do
    print $ runState doit []
