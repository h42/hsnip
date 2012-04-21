import Data.Monoid

newtype Jpd =  Jpd {getx :: Int}  deriving (Eq,Ord,Show,Read)

instance Monoid Jpd where
    mempty = Jpd 0
    mappend (Jpd x) (Jpd y) = Jpd (x+y)

j1 = [Jpd x | x<-[1..5]]
--mconcat j1 works

{-
instance Monoid Ordering where
	mempty = EQ
	LT `mappend` _ = LT
	EQ `mappend` y = y
	GT `mappend` _ = GT
-}

lengthCompare :: String -> String -> Ordering
lengthCompare x y = (length x `compare` length y) `mappend`  (x `compare` y)

{-
instance Monoid a => Monoid (Maybe a) where
	mempty = Nothing
	Nothing `mappend` m = m
	m `mappend` Nothing = m
	Just m1 `mappend` Just m2 = Just (m1 `mappend` m2)
-}
