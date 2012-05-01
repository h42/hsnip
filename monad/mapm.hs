import Control.Monad
import Control.Monad.Instances
import System.Directory

map1 = mapM_ putStrLn $ map show [1..5]

fold1 = foldM (\a x -> if x<7 then Just (x+a) else Nothing) 0 [1..7]

filt1 = filterM doesDirectoryExist [".", "..", "xxx"]
filt2 = getDirectoryContents "../.."
    >>= (\ds -> return $
	map ("../../" ++)
	   (filter (\d -> if d/="." && d/=".." then True else False) ds))
    >>=    filterM doesDirectoryExist

ef a x = if x<7 then Right (x+a) else Left ("hey "++ show x)

e = foldM ef 0 [1,2,3,4,5,5]
e' = foldM ef 0 [1,2,3,4,7,5,8]

ff x = if even x then Right True else if x>20 then Left "Hay" else Right False
f = filterM ff [1..10]
f' = filterM ff [1..21]
