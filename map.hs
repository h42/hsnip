{-# LANGUAGE BangPatterns #-}
import Data.List
import Data.Maybe
import qualified Data.Foldable as F (toList,all)
import qualified Data.Map as M
import System.Random

--
-- INSERT
--
mkmap :: [Int] -> (M.Map Int String)
mkmap xs = foldl' (\a (k,v) -> M.insert k v a )
	M.empty (map (\x -> (x,show x)) xs)

--
-- INSERTWITH
--
mkdupmap xs = foldl' insfunc  M.empty vs   where
    insfunc d (k,v)  =  M.insertWith dupfunc k v d
    dupfunc (news,newcnt) (olds,oldcnt)  =  (olds,oldcnt+newcnt)
    vs =(map (\x -> (x, (show x,1))) xs)
--
-- F.ALL
--
chkmap xs m = F.all (\x -> M.member x m) xs

--
-- TEST M.FOLD TO FIND MAX DUPS
--
getmaxdups :: M.Map Int (String,Int) -> ([Int],Int)
getmaxdups m = (keys,cnt) where
    (keys,cnt) = M.foldrWithKey maxdupfunc ([],0) m
    --accumuatorhas different type then value
    maxdupfunc k (xstr,xcnt) (akey,acnt)  =
	if acnt > xcnt then (akey, acnt)
	else if acnt == xcnt then (k:akey, acnt)
	else ([k],xcnt)

--
-- LOOKUP, ADJUST TEST
--
norm :: Int -> M.Map Int String -> IO ()
norm n m =
    case (getfirst 0 n m >>= \k -> M.lookup k m >>= \v -> return (k,v)) of
	Nothing -> putStrLn "getfirst/lookup failed"
	Just (k,v) -> norm2 k v m

norm2 k v m = do
    putStrLn $ "Key=" ++ show k ++ " - " ++ v
    let m' = M.adjust (\s->s ++" adjusted") k m
    case M.lookup k m' of
	Nothing -> print "adjusted lookup failed"
	Just s -> print s

getfirst i n m
    | i>=n          = Nothing
    | M.member i m  = Just i
    | otherwise     = getfirst (i+1) n m

--
-- Practice
--
practice = do
    let xs = [7,14..50] :: [Int]
	m = foldl (\m' x -> M.insert x (show x) m' ) M.empty xs
	(n,n2) = (14,14)
	m2 = M.delete n m
	mm3 = if M.member n2 m2  then Just $ M.adjust (++" adjusted") n2 m2
	      else Nothing
    case mm3 of
	Just m3 -> print m3
	_ -> print "not adjusted"
    print m
    print m2

--
-- MAIN
--
main = do
    practice
    {-
    g <- getStdGen
    let n2 = 10000
	n  = n2 * 5
	xs = take n $ randomRs (1,n2) g :: [Int]
	m  = mkmap xs
	m2  = mkdupmap xs
	m3  = M.filterWithKey (\k v -> even k) m

    print $ take 10 (F.toList m)
    print $ chkmap xs m

    putStrLn ""
    print $ take 10 (F.toList m2)
    print $ chkmap xs m2

    putStrLn ""
    print $ getmaxdups m2

    putStrLn "\n\nFiltered map"
    print $ take 10 (F.toList m3)

    putStrLn "\n\nLookup, adjust"
    norm n m
    -}
