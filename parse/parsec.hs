import Text.ParserCombinators.Parsec

{-
ps="@11 222 333 444"

symbol :: Parser Char
symbol = oneOf "!@#"

pit xs = case parse ((skipMany space) >> symbol) "11" xs of
    Left err -> "Bad " ++ show err
    Right val -> "Good " ++ [val]

ident :: Parser String

ident = do c <- letter <|> char '_'
	   cs <- many (letter <|> digit <|> char '_')
	   return (c:cs)
	<?> "identifier"
-}

------------------------
--
------------------------
passwd = do
    ps <- (readFile "/etc/passwd")
    case parse line "" ps of
	Left e -> print e
	Right g -> mapM_ print g

line :: Parser [(String,String,String)]
line = do
    ls <- many vals
    eof
    return ls

vals = do
    v1 <- val
    char ':'
    v2 <- val
    char ':'
    v3 <- val
    many (noneOf "\n")
    eol
    return (v1,v2,v3)

val = many (noneOf ":")

eol :: Parser Char
eol = char '\n'

main = do
    return ()
    passwd
    {-
    print $ pit ps
    print $ 42
    print $ parse (ident >> many (char ' ') >> ident) "" "a123 b234"
    -}
