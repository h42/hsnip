
import Data.Monoid

{-
newtype Logger a = Logger {runLogger :: (a,[String])}

instance Monad Logger where
    return a = Logger (a,[])
    Logger(a,w) >>= f =
	let Logger (b,w') = f a
	in Logger (b,w++w')

infixr 0 .-
-}

(.-) f x = f x

newtype Logger a = Logger {runLogger :: (a,[String])}
instance Monad Logger where
    return a = Logger (a,[])
    Logger (a,w) >>= f =
        let Logger (b,w') = f a
        in Logger (b,w++w')

tell s = Logger ((),[s])

lf x = Logger (x*x, [show x])

lf2 = do
    lf 2
    do
	lf 4
	lf 5
    tell "hey"
    lf 3

p x = "<p>" ++ x ++ "</p>"
q x = "<q>" ++ x ++ "</q>"
r x = Logger (0, ["<r>" ++ x ++ "</r>"])

{-
newtype  Jpd b a = Jpd {runJpd :: (a,b)}
instance (Monoid b) => Monad (Jpd b) where
    return a = Jpd (a,mempty)
    Jpd (a,b) >>= f = f a
-}

newtype  Jpd b a = Jpd {runJpd :: (a,b)} deriving Show
instance (Num b) => Monad (Jpd b) where
    return a = Jpd (a,0)
    Jpd (a,w) >>= f =
        let Jpd (b,w') = f a
        in Jpd (b,w+w')

j1 x = Jpd (3*x,3*x)

jj = do
    Jpd (3,3)
    j1 2
    j1 1

main = do
    print $ jj
    {-
    print $ runLogger lf2
    print $ snd $ runLogger lf2
    -}
