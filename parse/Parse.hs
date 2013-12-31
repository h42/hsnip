import Data.Char
import Control.Monad.State
import Debug.Trace

newtype Parse s a = Parse { runParse :: s -> Either String (a,s) }

instance Monad (Parse st) where
    return x = Parse $ \st -> Right (x,st)
    (Parse h) >>= f = Parse newf where
        newf st =
            --case (runParse (Parse h) st) of
            case (h st) of
                    Left s -> Left s
                    Right (a, newParse) -> runParse (f a) newParse

char :: Char -> Parse String Char
char c = Parse $ \st -> case st of
    (x:xs) -> if x==c then Right (c,xs)
                      else Left ("char: Bad match on " ++ [c,x])
    []     -> Left "char: EOS"

anyChar :: Parse String Char
anyChar = Parse $ \st -> case st of
    (x:xs) -> Right (x,xs)
    []     -> Left "char: EOS"

space :: Parse String Bool
space = Parse $ \st -> case st of
            (x:xs) -> Right $ (isSpace x, (x:xs))
            []     -> Left "char: EOS"

spaces :: Parse String ()
spaces = Parse $ \xs -> Right ((), dropWhile isSpace xs)

count ::Int -> Parse String a -> Parse String a
count 1 p = p
count n p = p >> count (n-1) p

eol :: Parse String ()
eol = Parse $ \st -> case st of
    ('\n':xs)      ->  Right ((), xs)
    ('\r':'\n':xs) ->  Right ((), xs)
    ('\r':xs)      ->  Right ((), xs)
    _              ->  Left ("char: Bad match")

doit = do
    char 'a'
    char 'b'
    y<-anyChar
    --traceShow y anyChar
    spaces
    count 3 anyChar
    return ()

main = print $ runParse (doit) "abc   defg"
