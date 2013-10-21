import Control.Monad.Writer
import Control.Monad.State

{-
newtype Logger a = Logger {
    runLogger :: (a,[String])
    }

instance Monad Logger where
    return a = Logger (a, [])
    Logger (a,xs) >>= f = let Logger (a',xs') = f a
			  in Logger (a',xs++xs)
-}
-----------------------------------------------------
newtype Logger a = Logger {runLogger :: (a,[String])}

instance Monad Logger where
    return a = Logger (a,[])
    Logger (a,xs) >>= f = let Logger (a',xs') = f a
			  in Logger (a',xs++xs')

lf x = Logger (x^2,[show x])
lf2 = do
    lf 2
    lf 3


{-
newtype State s a = State { runState :: s -> (a,s) }

instance Monad (State s) where
    return x = State $ \s -> (x,s)
    (State h) >>= f = State $ \s -> let (a, newState) = h s
					(State g) = f a
				    in g newState
-}

doit = do
    push 2
    push 3
    pop
    mapM push [1..5]

--push :: Int -> State [Int] Int
--push x = state $ \xs -> (x,x:xs)
push x = do
    xs <- get
    put (x:xs)
    return x

--pop :: State [Int] Int
pop = do
    (x:xs) <- get
    put xs
    return x

test5 = do
    modify (+ 1)
    --a <- get
    --lift (print a)
    modify (+ 1)
    b <- get
    lift (print b)

go5 = evalStateT test5 0

main = do
    print 42
    print $ runLogger lf2
    print $ runState doit []
    go5
