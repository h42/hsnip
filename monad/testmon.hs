import Control.Monad.Writer
import Control.Monad.State

{-
f :: Int -> Writer [String] Int
f x = do
    tell [show x]
    return (x^2)
    --writer (x,[show x])

push :: Int -> State [Int] ()
push x = state (\s -> ((),(x:s)))
pop = state (\(x:xs) -> (x,xs))

stfunc = do
    push 1
    push 2
    push 3
    pop
-}

main = do
    print $ runWriter $f 3 >> f 4 >> f 5
    print $ runState stfunc []


