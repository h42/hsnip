import System.IO.Error
import Control.Monad

fio :: Int -> IO Int
fio i = do
    if i == 13 then fail "oops!"
    else if i==10 then return (-1)
    else return i

main = do
    flip catchIOError
	(\e->print $ show e ++ "hey")
	(loop 0)

loop i = do
    x <- fio i
    when (x >= 0) $ do
	print x
	loop (i+if i==9 then 2 else 1)
