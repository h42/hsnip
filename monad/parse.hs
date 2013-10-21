newtype Parse s a = Parse { parse :: s -> (a,s) }

instance Monad (Parse s) where
    return x = Parse $ \s -> (x,s)
    (Parse h) >>= f = Parse $ \s -> let (aa, newParse) = h s
                                        (Parse g) = f aa
                                    in g newParse

evalParse st st0 = snd $ parse st st0

get :: Parse s s
get = Parse $ \s -> (s,s)

put :: s -> Parse s ()
put x = Parse $ \s -> ((),s)

eof = '\NUL'

char :: Parse String Char
char = Parse $ \xxs -> do
    case xxs of
        (x:xs) -> (x,xs)
        _      -> (eof,[])

doit = do
    x<-char
    y<-char
    z<-char
    zero<-char
    return (x,y,z,zero)

main = do
    print $ parse doit "hey"

{-

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

doit = do
    mapM_ push [1..10]
    a<-pop
    pop2
    pop2
    pop2
    return a
-}

