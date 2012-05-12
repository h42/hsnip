//
// Copied from internet - do not know author
//

data Year = Y Int

data MonthEnum = January | February | March | April | May | June | July |
		 August  | September | October | November | December
		 deriving (Show, Eq, Enum, Bounded, Ord)

data DayEnum = Sunday | Monday | Tuesday | Wednesday |
	       Thursday | Friday |Saturday
	       deriving (Show, Eq, Enum, Bounded)

data Month = M MonthEnum Year
data Day   = Dm Int Month

years = [Y y | y <- [2007..]]

months (Y y) = [M m (Y y) | m <- [January .. December]]

days (M m yy@(Y y))
	| m `elem` [January,March,May,July,August,October,December] =
	    [Dm d (M m yy) | d <- [1..31]]
	| m == February =
	    [Dm d (M m yy) | d <- [1..if y `mod` 4 == 0 then 29 else 28]]
	| otherwise = [Dm d (M m yy) | d <- [1..30]]

{-days (M m (Y yy))
	| m `elem` [January,March,May,July,August,October,December] =
	    [Dm d (M m (Y yy)) | d <- [1..31]]
	| m == February =
	    [Dm d (M m (Y yy)) | d <- [1..if yy `mod` 4 == 0 then 29 else 28]]
	| otherwise = [Dm d (M m (Y yy)) | d <- [1..30]]
-}

instance Show Month where
    show (M m t) = show m ++ " " ++ show t

instance Show Year where
    show (Y y) = " " ++ show y

instance Show Day where
    show (Dm d t) = " " ++ show d ++ " " ++ show t

-- With the above addition, and using the
-- fact that List in Haskell is a monad, we
-- can easily represent the following,

-- all days of all months of all years (only the first 3 items are shown)
-- *DateStream> take 3 (years >>= months >>= days)
-- [ 1 January  2007, 2 January  2007, 3 January  2007]

-- first day of every month of every year
-- *DateStream> take 3 (years >>= months >>= return.(Dm 1))
-- [ 1 January  2007, 1 February  2007, 1 March  2007]

-- all days of january of every year
-- *DateStream> take 3 (years >>= return.(M January) >>= days)
-- [ 1 January  2007, 2 January  2007, 3 January  2007]

-- all mondays in June of any year starting from 2007
-- *DateStream> take 6 (filter (isDayOfWeek Monday) (years >>= return.(M June) >>=days))
-- [ 4 June  2007, 11 June  2007, 18 June  2007, 25 June  2007, 2 June  2008, 9 June  2008]


