
-- IN PROGRESS

newtype State s a = State { runState :: s -> (Maybe a,s) }

instance Monad (State s) where
    return x = State $ \s -> (x,s)
    (State h) >>= f = State $
	\s -> let (a, newState) = h s

		  (State g) = f a
	      in g newState


{-
evalState st st0 = snd $ runState st st0
execState st st0 = fst $ runState st st0

push :: Int -> State [Int] ()
push x = State (\st -> ((),x:st))
-}

main = do
    print 42
    --print $ execState (push 2) []
    --print $ evalState (push 2) []

{-
newtype State s a = State { runState :: s -> (a,s) }

instance Monad (State s) where
    return x = State $ \s -> (x,s)
    (State h) >>= f = State $ \s -> let (a, newState) = h s
					(State g) = f a
				    in g newState

evalState st st0 = snd $ runState st st0
execState st st0 = fst $ runState st st0

push :: Int -> State [Int] ()
push x = State (\st -> ((),x:st))

main = do
    print $ execState (push 2) []
    print $ evalState (push 2) []
-}
