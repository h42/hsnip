{-# LANGUAGE BangPatterns #-}
import qualified Data.Foldable as F
import qualified Data.Sequence as S
import Data.Sequence (ViewL(..),ViewR(..),(<|),(|>),(><))

import System.Random

-- Data.Foldable used for toList and all other folds

-----------------------------
-- QUEUE
-----------------------------
enq :: a -> S.Seq a -> S.Seq a
enq x s = x <| s

deq :: S.Seq a -> Maybe (a, S.Seq a)
deq s =  case (S.viewr s) of
    s' :> x -> Just (x,s')
    S.EmptyR -> Nothing

test_queue = do
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
    let n = 10000
	rs = randomRs (0,n-1) g :: [Int]
	s = S.sort $ S.fromList (take n rs) -- approx same speed as list
    putStrLn "--\n--Sort Test\n--"
    print $ S.take 10 s
    print $ S.drop (n-10) s

-----------------------------
-- Sequential Access
-----------------------------
test_sequential = do
    let n' = 1000000
	mkseq i n s
	    | i < n = mkseq (i+1) n (s |> i)
	    | otherwise = s

	--myseq = S.fromList [0..n'-1]
	myseq = mkseq 0 n' S.empty
    print $ S.length myseq

    print $ F.foldl' (+) 0 myseq -- USES FUNCTION FROM DATA.FOLDABLE

    -- sequences are functors so use fmap
    print $ S.length $ fmap (\x -> x*2) myseq

-----------------------------
-- Random Access
-----------------------------
test_rand = do
    g <- getStdGen
    let n' = 1000000
	rs' = randomRs (0,n'-1) g :: [Int]
	mkseq i n s
	    | i < n = mkseq (i+1) n (s |> i)
	    | otherwise = s
	--myseq = S.fromList [0..n'-1]
	myseq = mkseq 0 n' S.empty

	testseq i n (r:rs) s
	    | i >= n = "Good"
	    | r == y = testseq (i+1) n rs s
	    | otherwise = "Bad"
	  where
	    y = S.index s r
    print $ testseq 0 n' rs' myseq


-----------------------------
-- Index / Update
-----------------------------

main = do
    test_rand
    --test_sequential
    --test_sort
    --test_queue
