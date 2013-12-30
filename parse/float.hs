import Text.Parsec
import Control.Applicative hiding ((<|>))

(<++>) a b = (++) <$> a <*> b
(<:>) a b = (:) <$> a <*> b

number = many1 digit
plus = char '+' *> number
minus = (:) <$> char '-' <*> number
integer = plus <|> minus <|> number

float = fmap rd $ integer <++> decimal <++> exp'
    where rd = read :: String -> Float
	  decimal = option "" $ char '.' <:> number
	  exp' = option "" $ oneOf "eE" <:> integer

main = do
    let input = words "123 -12.3 +1234.56 123e-2"
    mapM_ print (map  (parse float "" ) input)

