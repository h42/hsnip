{-# LANGUAGE BangPatterns #-}
import Data.List
import Data.Maybe
import qualified Data.Foldable as F (toList,all)
import qualified Data.Map as M
import System.Random

mkmap :: [Int] -> (M.Map Int String)
mkmap xs = foldl' (\a (k,v) -> M.insert k v a )
	M.empty (map (\x -> (x,show x)) xs)

mkdupmap xs = foldl' insfunc  M.empty vs   where
    insfunc d (k,v)  =  M.insertWith dupfunc k v d
    dupfunc (news,newcnt) (olds,oldcnt)  =  (olds,oldcnt+newcnt)
    vs =(map (\x -> (x, (show x,1))) xs)

-- F.all
chkmap xs m = F.all (\x -> M.member x m) xs

-- Test M.fold to find max dups
getmaxdups :: M.Map Int (String,Int) -> ([Int],Int)
getmaxdups m = (keys,cnt) where
    (keys,cnt) = M.foldrWithKey maxdupfunc ([],0) m
    --accumuatorhas different type then value
    maxdupfunc k (xstr,xcnt) (akey,acnt)  =
	if acnt > xcnt then (akey, acnt)
	else if acnt == xcnt then (k:akey, acnt)
	else ([k],xcnt)

-- Lookup, delete, adjust test
norm :: Int -> M.Map Int String -> IO ()
norm n m = do
    let rc = getfirst 0 n m >>= \k -> M.lookup k m >>= \v -> return (k,v)
    case rc of
	Nothing -> putStrLn "getfirst/lookup failed"
	Just (k,v) -> norm2 k v m

norm2 k v m = do
    putStrLn $ "Key=" ++ show k ++ " - " ++ v
    let m' = M.adjust (\s->s ++" adjusted") k m
	rc = M.lookup k m'
    case rc of
	Nothing -> print "adjusted lookup failed"
	Just s -> print s

getfirst i n m
    | i>=n          = Nothing
    | M.member i m  = Just i
    | otherwise     = getfirst (i+1) n m

main = do
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
