*Control.Monad.Loops

*monadic loops go fast with _ functions. ie forM_


loop :: Monad m => (b -> m (Either a b)) -> b -> m a
loop k x = either return (loop k) =<< k x

main = do
    print $ 42

