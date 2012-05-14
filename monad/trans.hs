import Data.List
import Data.Char
import System.Directory -- (doesDirectoryExist)
import Control.Monad (forM_, when)
import Control.Monad.Trans (liftIO)
import Control.Monad.Writer -- (WriterT, tell)

wt :: WriterT [String] IO ()
wt = do
    cont <- liftIO (getDirectoryContents ".")
    mapM_ tell [cont]
    tell ["hey"]

main = do
    --w <- runWriterT wt
    w <- execWriterT wt
    mapM_ putStrLn (sortBy (\x y -> compare (map toLower x) (map toLower y)) w)
    return ()

