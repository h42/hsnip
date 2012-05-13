import System.Environment

mynub [] = []
mynub (x:xs) = x:mynub (filter (\x' -> x /= x') xs)

bm :: Int -> [Int] -> Int
bm n [] = bm n [1]
bm n (x:xs) 
    | x==n = x
    | otherwise =  bm n (x+1:x:xs)

state0 :: Int -> Int -> Int
state0 n m 
    | n <= m = m
    | otherwise = state1 n (m+1)

state1 n m 
    | n <= m = m
    | otherwise = state2 n (m+1)

state2 n m 
    | n <= m = m
    | otherwise = state3 n (m+1)

state3 n m 
    | n <= m = m
    | otherwise = state0 n (m+1)

data Rec = Rec {zn :: Int, zm :: Int, zx1 :: Int, zx2 :: Int} deriving (Show)

state_a rec
    | zn rec <= zm rec = rec
    | otherwise = state_a rec{zm=zm rec + 1, zx1=zx1 rec + 1}

state_d rec@(Rec n m !x1 x2)
    | zn rec <= zm rec = rec
    | otherwise = state_d rec{zm=zm rec + 1, zx1=zx1 rec + 1}

state_b rec@(!n,!m,!x1,x2)
    | n > m = rec
    | otherwise = state_b (n+1,m,x1+1,0)

state_c !n !m !x1 !x2
    | n > m = (n,m,x1,x2)
    | otherwise = state_c (n+1) m (x1+1) (if n==m-10 then x2+1 else x2)

main = do
    --print $ bm 5000000 []
    args <- getArgs
    let cnt = 12345678
    case args of
	["a"] -> print $ state_a (Rec {zn=cnt,zm=0,zx1=0,zx2=0})
	["b"] -> print $ state_b (0,cnt,0,0)
	["c"] -> print $ state_c  0 cnt 0 0
	["d"] -> print $ state_d (Rec {zn=cnt,zm=0,zx1=0,zx2=0})
	_  -> print $ state0 cnt 0
