class YesNo a where
	yesno :: a -> Bool
	yesno2 :: a -> Int

instance YesNo Int where
	yesno 0 = False
	yesno _ = True
	yesno2 0 = 0
	yesno2 _ = 1

instance YesNo [a] where
	yesno [] = False
	yesno _ = True
	yesno2 [] = 0
	yesno2 _ = 1

instance YesNo Bool where
	yesno = id
	yesno2 False = 0
	yesno2 True = 1

instance YesNo (Maybe a) where
	yesno Nothing = False
	yesno (Just a) = True
	--yesno _ = True
	yesno2 Nothing = 0
	yesno2 (Just a) = 1

data Jpd = Jpd Int Int deriving (Show)

instance YesNo Jpd where
	yesno (Jpd 0 0) = False
	yesno _ = True
	yesno2 (Jpd 0 0) = 0
	yesno2 _ = 1
