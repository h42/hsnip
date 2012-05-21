{-# LANGUAGE ViewPatterns #-}
import Data.List
import Control.Monad
import qualified Data.ByteString as B
import Data.Word
import Data.Char
import Data.Bits

default (Int)

u8Lines :: B.ByteString -> [B.ByteString]
u8Lines ps
    | B.null ps = []
    | otherwise = case search ps of
	 Nothing -> [ps]
	 Just n  -> B.take n ps : u8Lines (B.drop (n+1) ps)
    where search = B.elemIndex 10

u8Unlines :: [B.ByteString] -> B.ByteString
u8Unlines [] = B.empty
u8Unlines ss = (B.concat $ intersperse nl ss) `B.append` nl
    where nl = B.singleton 10

u8fromString :: String -> B.ByteString
u8fromString s = B.pack $ concatMap u8fromChar s

u8fromChar :: Char -> [Word8]
u8fromChar c
    | oc <= 0x7f   = [oc]
    | oc <= 0x7ff  = [ 0xc0 + (oc `shiftR` 6)
			, 0x80 + oc .&. 0x3f ]
    | oc <= 0xffff = [ 0xe0 + (oc `shiftR` 12)
			, 0x80 + ((oc `shiftR` 6) .&. 0x3f)
			, 0x80 + oc .&. 0x3f ]
    | otherwise    = [ 0xf0 + (oc `shiftR` 18)
                        , 0x80 + ((oc `shiftR` 12) .&. 0x3f)
                        , 0x80 + ((oc `shiftR` 6) .&. 0x3f)
			, 0x80 + oc .&. 0x3f ]
  where oc = fromIntegral $ ord c

getfile fn = do
    liftM u8Lines (B.readFile fn)

main = do
    us <- getfile "BIG.TXT"
    print $ length us
