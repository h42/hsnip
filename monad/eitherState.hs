
import Control.Monad
import Data.Monoid

{-
    data State s a = State { runState :: s -> (a,s) }
    instance Monad (State st) where
    return x = State $ \st -> (x,st)
    (State h) >>= f = State $ \st -> let (a, newState) = h st
					 (State g) = f a
				     in g newState
    execState m st = snd (runState m st)
-}


newtype State s a = State { runState :: s -> Either String (a,s) }

instance Monad (State st) where
    return x = State $ \st -> Right (x,st)

    (State h) >>= f = State newf where
	newf st =
	    --case (runState (State h) st) of
	    case (h st) of
		    Left s -> Left s
		    Right (a, newState) -> runState (f a) newState

push :: Int -> State [Int] Int
push 13 = State $ \st -> Left "Bad Luck"
push x = State $ \st -> Right (x,x:st)

pop :: State [Int] Int
pop = State $ \st -> case st of
    [] -> Left "Out of stack"
    (x:xs) -> Right (x,xs)

map_st :: State [Int] Int
map_st = do
    mapM_ push [1..7]
    return 0

test_state = do
    print $ runState pop [1,2,3,4,5]
    print $ runState pop [1]
    print $ runState pop []
    print $ runState (push 3 >> push 4) [1..5]
    print $ runState (map_st >> push 13 >> map_st) []


