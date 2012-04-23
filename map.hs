import Data.List
import qualified Data.Map as M

-- m is first because of foldl
tadd :: M.Map Int Int -> Int -> M.Map Int Int
tadd m 0 = m
tadd m n = tadd m' (n-1)  where
    m' = seq m M.insert n (n) m

main = do
    let cnt = 1000000
    --let m2 = foldl (\m n -> M.insert n (show n) m) M.empty [1..cnt]
    let m2 = tadd M.empty cnt
    print $ M.size m2
    {-
    let m3 = M.filterWithKey (\k _ -> k < floor((fromIntegral cnt)/2.0)) m2
    print $ M.size m3
    print $M.lookup 17 m3
    print $M.lookup (cnt+2) m3
    -}
