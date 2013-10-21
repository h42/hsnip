import Control.Monad
import Data.List

l1 = do
    a <- [1..3]
    b <- ['a'..'b']
    guard (a/=2)
    return (a,b)

type KnightPos = (Int,Int)

moveKnight :: KnightPos -> [KnightPos]
moveKnight (c,r) = do
    (c',r') <- [(c+1,r+2),(c+1,r-2),(c-1,r+2),(c-1,r-2),
                 (c+2,r+1),(c+2,r-1),(c-2,r+1),(c-2,r-1)
                ]
    guard (c' >= 1 && c' <=8 && r'>=1 && r'<=8)
    return (c',r')

nightMoves = do
    m1 <- moveKnight (1,1)
    moveKnight m1

main = do
    print $ l1
    print $ nub $ sort $ nightMoves
    print $ (nub.sort) $ moveKnight (1,1) >>= moveKnight >>= moveKnight

