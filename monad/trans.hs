import Control.Monad.State

main = do
    evalStateT t1 [] >>= print

t1 :: StateT [Char] IO ()
t1 = do
    s <- lift getLine
    lift (putStrLn s)
    modify (s++)
    modify (s++)
    modify (s++)
    st <- get
    lift (print st)
    put "xxx"
    modify (t2 "yyy")
    st<-get
    lift (print st)

t2 parm st = st ++ parm
