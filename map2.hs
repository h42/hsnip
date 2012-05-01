import qualified Data.IntMap as M
import System.Random

--
-- fastmap2 is 10% faster than fastmap (fold with list of randoms).
-- It is probably list creation that adds overhead
--
fastmap2 i n g m
    | i >= n = m
    | otherwise = fastmap2 (i+1) n g' m'
  where
    (r,g') = randomR (1,n) g :: (Int,StdGen)
    m' = M.insert r (show r) m

fastmap xs = foldl fastmap' M.empty xs where
    fastmap' m x = M.insert x (show x) m

main = do
    g <- getStdGen
    let n = 1000000
	xs = take n (randomRs (1,n) g :: [Int])
	--m = fastmap xs
	m = fastmap2 0 1000000 g M.empty
    print $ M.size m
