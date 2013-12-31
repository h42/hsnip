{-# LANGUAGE OverloadedStrings #-}
import Data.Attoparsec.Char8
import Control.Applicative
import qualified Data.ByteString.Char8 as B

s="this is the end"  :: B.ByteString

type BString = B.ByteString
--type BString = [Char]

p1 :: Parser (BString, BString, BString)
p1 = do
    many space
    --w0 <- count 4 digit -- BAD
    w1 <- count 4 anyChar
    many space
    w2 <- takeTill isSpace
    many space
    --w3 <- takeTill isEndOfLine
    w3 <- takeByteString
    return (B.pack w1, w2,w3)

main = do
    print $ parseOnly p1 s
