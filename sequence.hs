{-# LANGUAGE BangPatterns #-}
import qualified Data.Sequence as S
import Data.Sequence (ViewL(..),ViewR(..),(<|),(|>))

import System.Random

-----------------------------
-- QUEUE
-----------------------------
enq :: a -> S.Seq a -> S.Seq a
enq x s = x <| s

deq :: S.Seq a -> Maybe (a, S.Seq a)
deq s =  case (S.viewr s) of
    s' :> x -> Just (x,s')
    S.EmptyR -> Nothing

test_q = do
    let s = (enq 5 . enq 4 . enq 3 . enq 2) S.empty -- :: S.Seq Int
    putStrLn "--\n--Queue Test\n--"
    print s
    print $ deq s

-----------------------------
-- Sort
-----------------------------
f1 = s2 where
    s = -42 <| (S.fromList [1..6] |> 42 )
    s2 = case (S.viewl s) of
       x :< s' -> s'
       S.EmptyL -> S.empty

test_sort = do
    g <- getStdGen
    let n = 100000
	rs = randomRs (0,n-1) g :: [Int]
	s = S.sort $ S.fromList (take n rs) -- approx same speed as list
    putStrLn "--\n--Sort Test\n--"
    print $ S.take 10 s
    print $ S.drop (n-10) s

-----------------------------
-- Index / Update
-----------------------------

main = do
    --test_q
    test_sort
