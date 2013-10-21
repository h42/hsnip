import Data.Char

newtype Parser a = Parser {parse :: String -> [(a,String)]}

instance Monad Parser where
    return x = Parser $ \s -> [(x,s)]
    m >>= k =
        Parser $ \s -> [(x,y) | (u,v) <- parse m s, (x,y) <- parse (k u) v]

zero :: Parser a
zero = Parser $ \s -> []

(<|>) = bchoice

bchoice :: Parser a -> Parser a -> Parser a
bchoice m n = Parser $ \s -> if null (parse m s) then parse n s else parse m s

filt :: Parser a -> (a -> Bool) -> Parser a
m `filt` p = do
    t <- m
    if p t then return t else zero

manyOf :: Parser a -> Parser [a]
manyOf m = multiple `bchoice` return []  where
    multiple = do
        t <- m
        ts <- manyOf m
        return $ t : ts

char :: Parser Char
char = Parser char' where
    char' [] = []
    char' (a : xs) = [(a,xs)]

oneOf :: [Char] -> Parser Char
oneOf x = char `filt` ((flip elem) x)

digit :: Parser Char
digit = char `filt` isDigit

space :: Parser Char
space = char `filt` isSpace

number :: Parser Int
number = do
    ds <- manyOf digit
    return (read ds :: Int)

numbers = do
    n <- manyOf digit
    if null n then return zero
    else do
        return zero


main2 = do
    x <- digit <|> space
    y <- char
    return (x,y)

main = do
    print $ parse main2 "qa 1 2 3"
    print $ parse (oneOf "abc") "qb b b"
    print $ parse number "12 14 "
    --print $ parse (main2) "123 1234 \n"
