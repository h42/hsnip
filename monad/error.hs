import Control.Monad.Instances

f1 ::(Integral a) => a -> Either String a
f1 x
    | even x = Right x
    | otherwise = Left "Boo Boo"


main = do
    let mx = f1 2 >> f1 4 >> f1 6
	--dx :: Either String Int
	dx = do
	    a<-f1 2
	    b<-f1 4
	    c<-f1 6
	    return (a+b+c)
    print mx
    print dx
