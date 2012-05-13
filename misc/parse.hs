import Char

data Jtok = Jpnum Double | Jpchar [Char] | Jperr [Char] deriving (Show,Eq,Ord)

jgettok :: [Char] -> [Jtok]
jgettok [] = []
jgettok xs = state_0 xs [] where
    state_0 [] ans =  ans
    state_0 (x:xs) ans
	| isDigit x = state_1 xs [x] ans
	| x == '.' = state_2 xs [x] ans
	| isAlpha x = state_10 xs [x] ans
	| x == '"' = state_20 xs [] ans
	| otherwise = state_0 xs ans

    state_1 [] y ans = ans ++ [Jpnum (read y :: Double)]
    state_1 (x:xs) y ans
	| isDigit x = state_1 xs (y++[x]) ans
	| x == '.' = state_2 xs (y++[x]) ans
	| otherwise = state_0 (x:xs) (ans ++ [Jpnum (read y :: Double)])

    state_2 [] y ans = ans ++ [Jpnum (read y::Double)]
    state_2 (x:xs) y ans
	| isDigit x = state_2 xs (y++[x]) ans
	| otherwise = state_0 (x:xs) (ans ++ [Jpnum (read y :: Double)])

    state_10 [] y ans = ans ++ [Jpchar y]
    state_10 (x:xs) y ans
	| isAlphaNum x = state_10 xs (y++[x]) ans
	| otherwise = state_0 (x:xs) (ans ++ [Jpchar y])

    state_20 [] y ans = [Jperr "Missing quote"]
    state_20 (x:xs) y ans
	| x == '"' = state_0 xs (ans ++ [Jpchar y])
	| otherwise = state_20 (xs) (y++[x]) ans

--main = do
--    putStrLn "Hey"
