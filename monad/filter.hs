import Control.Monad
import qualified Data.IntMap as M

m = M.fromList [(x,x^2) | x<-[2,4..10]]

checker mm x = case M.lookup x mm of
	Just y -> Just True
	_      -> Just False

main = do
    let m' = filterM (checker m) [1..7]
    print m'
    print 42
