import Control.Monad
import Text.Parsec
import Control.Applicative hiding ((<|>))

integer = rd <$> (plus <|> minus <|> number)
    where rd = read :: String -> Integer
	  plus = char '+' *> number
	  minus = (:) <$> char '-' <*> number
	  number = many1 digit

--input = "123 +1234 -12345"

main = forever $ do
	   putStrLn "Enter an integer: "
	   input <- getLine
	   parseTest integer input
