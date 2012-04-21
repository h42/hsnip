
{-  FUNCTOR & APPLICATIVE FUNCTOR stuf

class Functor f where
    fmap :: (a -> b) -> f a -> f b

class (Functor f) => Applicative f where
    pure :: a -> f a
    (<*>) :: f (a -> b) -> f a -> f b
-}

import Control.Applicative

data Jpd a = Jpd a deriving (Eq,Ord,Show)

instance Functor Jpd where
    fmap f (Jpd a) = Jpd (f a)

instance Applicative Jpd where
    pure a = Jpd a
    (Jpd f) <*> a = fmap f a
    -- !!!!!  fmap f a   and not   fmap f (Jpd a)

hypot x y = sqrt $ x^2 + y^2
h2 = hypot <$> Jpd 3 <*> Jpd 4

cc = putStrLn "enter 2 lines of data" >> (++) <$> getLine <*> getLine

