{-# LANGUAGE GADTs #-}

data T a where
    R :: (Real a) => a -> T a
    I :: Int -> T Int
    D :: Double -> T Double
    T :: a -> T a

instance Show (T a) where
    show (T a)  = "hey now"
    show (I x)  = "I " ++ show x
    show (D x)  = "D " ++ show x

main = do
    let x = T "string"
        x2 = T 23.22
    print $ I 17
    print $ D 17
    print $ x
    print $ x2

{-
  NO GADTs => data Bag = Oranges Int | Apples Int

data Orange = ...
data Apple = ....

data Bag a where
    OrangeBag :: [Orange] -> Bag [Orange]
    AppleBag :: [Apple] -> Bag [Apple]

Now I can define a function which only works with bag of apples.

giveMeApples :: Bag [Apple] -> ...
-}
