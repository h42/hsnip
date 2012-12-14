repeatM :: (Monad m) => Int -> m a -> (a -> m a) -> m a

repeatM n ma f = foldl (>>=) ma (replicate n f)
