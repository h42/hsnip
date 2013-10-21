{-# LANGUAGE DeriveDataTypeable #-}

import Prelude hiding (catch)
import Data.Typeable
import Control.Exception


data MyException = MyException String  deriving (Show, Typeable)
instance Exception MyException

data MyException2 = MyException2 String  deriving (Show, Typeable)
instance Exception MyException2

xxx = do
    throw (MyException "hey")

main = do
    catch xxx
        $
        \e -> do
            case e of
                MyException s -> putStrLn s -- (e:: MyException)
            print e
    print $ 42

