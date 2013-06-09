{-# LANGUAGE TemplateHaskell #-}
import Control.Lens
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.State

data Point = Point
    { _zx :: Double
    , _zy :: Double
    , _zxs :: [Double]
} deriving (Show)

makeLenses ''Point

initPoint = Point 1 1 [1..5]

f1 :: StateT Point IO ()
f1 = do
    s<-get
    let n1 = 7
    zx+=n1
    s2<-get
    lift $ print $ s2^.zx
    lift $ putStrLn ("Point=" ++ show s ++ "\n")

f2 :: StateT Point IO ()
f2 = do
    zxs.traversed -= 2

f3 :: StateT Point IO ()
f3 = zxs.element 1 .= 17.2


f4 :: StateT Point IO ()
f4 = forM_ [0..4] $ \i -> zxs . element i .= (23.0 + fromIntegral i)

main = do
    rs<-execStateT (f1>>f2>>f1>>f3>>f4) initPoint
    print $ rs^.zx
    print rs

