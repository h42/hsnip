
newtype Logger a = Logger {runLogger :: (Maybe a,[String])} deriving Show

instance Monad Logger where
    return a = Logger (Just a,[])
    Logger (Nothing, w) >>= f = Logger (Nothing, w)
    Logger (Just a,w) >>= f =
        let (a',w') = case f a of
                          Logger (Just a'', w'') -> (Just a'',w++w'')
                          Logger (Nothing, w'')     -> (Nothing,w++w'')
        in Logger (a',w')

tell s = Logger (Just 0,[s])

lf 0 = Logger (Nothing, ["Nothing"])
lf x = Logger (Just (x*x), [show x])

lf2 = do
    lf 1
    lf 2
    lf 3
    do
        lf 31
        lf 32
    tell "hey man"
    lf 4
    lf 0
    lf 5

main = do
    print $ runLogger lf2
