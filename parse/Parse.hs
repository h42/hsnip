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

infixl 3 <|>

(Parse h) <|> f = Parse newf where
    newf st = case (h st) of
                Left _ -> runParse (f  ) st
                Right (a, newParse) -> Right (a, newParse)

r1 :: Parse Int Bool
r1 = Parse $ \st -> case st of
         3 -> Left "all done"
         _ -> Right (True, st+1)

r2 :: Parse Int Bool
r2 = Parse $ \st -> Right (True, st+1)

r3 :: Parse Int Bool
r3 = Parse $ \st -> Left "all done"

main = do
    print $ runParse (r1 <|> r2 <|> r3) 3

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

line :: Parse String String
line = Parse $ \st ->
    let (xs,ys') = break (\x -> x == '\r' || x == '\n') st
        ans = case ys' of
            ('\n':ys)      ->  Right (xs, ys)
            ('\r':'\n':ys) ->  Right (xs, ys)
            ('\r':ys)      ->  Right (xs, ys)
            _              ->  Right (xs, [])
    in ans

doit = do
    char 'a'
    char 'b'
    y<-anyChar
    --traceShow y anyChar
    spaces
    count 3 anyChar
    return ()

--main = print $ runParse (doit) "abc   defg"
--main = print $ runParse (line>>line>>line) "abc   defg\nline 2\r\nline 3\rline4"
