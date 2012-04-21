import Data.List
import qualified Data.Map as Map

-- m is first because of foldl
tadd :: Map.Map Int Int -> Int -> Map.Map Int Int
tadd m 0 = m
tadd m n = tadd m' (n-1)  where
    m' = seq m Map.insert n (n) m

main = do
    let cnt = 1000000
    --let m2 = foldl (\m n -> Map.insert n (show n) m) Map.empty [1..cnt]
    let m2 = tadd Map.empty cnt
    print $ Map.size m2
    {-
    let m3 = Map.filterWithKey (\k _ -> k < floor((fromIntegral cnt)/2.0)) m2
    print $ Map.size m3
    print $Map.lookup 17 m3
    print $Map.lookup (cnt+2) m3
    -}
