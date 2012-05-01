newtype State s a = State { runState :: s -> (a,s) }

instance Monad (State s) where
    return x = State $ \s -> (x,s)
    (State h) >>= f = State $ \s -> let (a, newState) = h s
					(State g) = f a
				    in g newState

type Stack = [String]

pop :: State Stack String
pop = State $ \(x:xs)-> (x,xs)

push :: String -> State Stack String
push a = State $ \xs -> (a,a:xs)

xxx :: State Stack String
xxx = do
    push "hey"
    push "now"

main = do
    --let y = s1 4 (s2 3  (s1 2 initJ) )
    print $ runState xxx []
    print $ runState (push "xxx") []
