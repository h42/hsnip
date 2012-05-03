import Control.Monad.Reader

type Env = (Int,Int)

useReader :: Int -> Reader Env Int
useReader x = do
    (e,e2) <- ask
    --s1 <- r2
    --s2 <- r2
    s12 <- liftM2 (+) r2 r2
    return (e^2 + e2 + s12 +x)

r2 :: Reader Env Int
r2 = do
    (e,e2) <- ask
    return (e+e2+2)

main = do
    print $ runReader (useReader 2) (7,2)

