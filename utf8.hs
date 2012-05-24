{-# LANGUAGE ViewPatterns #-}
import Data.List
import Control.Monad
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C
import Data.Word
import Data.Char
import Data.Bits
import Text.Printf

default (Int)

--
-- LINES
--
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

--
-- FROM STRING
--
pack :: String -> B.ByteString
pack s = B.pack $ concatMap u8fromChar s

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

--
-- UNCONS
--
badchar = '\xfffd'

uncons :: B.ByteString -> Maybe (Char, B.ByteString)
uncons (B.null -> True) = Nothing
uncons (B.uncons -> Just (w, bs))
    | w < 0x80 = Just (chr $ fromEnum w, bs)
    | w < 0xc0 = Just (badchar, bs)
    | w < 0xe0 = Just $ if B.length bs >= 1  then uncons2 w bs
					     else (badchar, bs)
    | w < 0xf0 = Just $ if B.length bs >= 2  then (uncons3 w bs)
					     else (badchar, bs)
    | w < 0xf8 = Just $ if B.length bs >= 3  then (uncons4 w bs)
					     else (badchar, bs)

uncons2 w bs = uc  where
    w2 = B.head bs
    uc = if chk2 w2  then (byte2 w w2, B.tail bs)
	 else (badchar, B.tail bs)

uncons3 w bs = uc  where
    w2 = B.head bs
    w3 = B.index bs 1
    uc = if chk3 w2 w3  then (byte3 w w2 w3, B.drop 2 bs)
	 else (badchar, bs)

uncons4 w bs = uc  where
    w2 = B.head bs
    w3 = B.index bs 1
    w4 = B.index bs 2
    uc = if chk4 w2 w3 w4 then (byte4 w w2 w3 w4, B.drop 3 bs)
	 else (badchar, bs)

byte2 :: Word8 -> Word8 -> Char
byte2 w w2 =
    chr $ ((fromEnum w .&. 0x1f) `shiftL` 6)
	  + (fromEnum w2 .&. 0x3f )

byte3 :: Word8 -> Word8 -> Word8 -> Char
byte3 w w2 w3 =
    chr $  ((fromEnum w .&. 0x0f) `shiftL` 12)
	 + ((fromEnum w2 .&. 0x3f) `shiftL` 6)
	 +  (fromEnum w3 .&. 0x3f )

byte4 :: Word8 -> Word8 -> Word8 -> Word8 -> Char
byte4 w w2 w3 w4 =
	chr $  ((fromEnum w .&. 0x07) `shiftL` 18)
	      + ((fromEnum w2 .&. 0x3f) `shiftL` 12)
	      + ((fromEnum w3 .&. 0x3f) `shiftL` 6)
	      +  (fromEnum w4 .&. 0x3f )

chk2 w2 = w2 .&. 0xc0 == 0x80
chk3 w2 w3 = w2 .&. 0xc0 == 0x80 && w3 .&. 0xc0 == 0x80
chk4 w2 w3 w4 = w2 .&. 0xc0 == 0x80 && w3 .&. 0xc0 == 0x80 && w4 .&. 0xc0 == 0x80

--
-- UNPACK
--
unpack :: B.ByteString -> String
unpack bs | B.null bs = ""
unpack bs = unpack' bs (B.length bs - 1) 0 0 []

unpack' :: B.ByteString -> Int -> Int -> Int -> [Char] -> String
unpack' bs ind acc bytes str
    | ind < 0 = str
    | w < 0x80 = unpack' bs (ind-1) 0 0 (chr (fromEnum w) : str)
    | w < 0xc0 =  unpack' bs (ind-1) ((acc `shiftL` 6) + (fromEnum w .&. 0x3f))
			  (bytes+1)  str
    | ismb2 && bytes == 1 = unpack' bs (ind-1) 0 0 (mbyte2 : str)
    | ismb3 && bytes == 2 = unpack' bs (ind-1) 0 0 (mbyte3 : str)
    | ismb4 && bytes == 3 = unpack' bs (ind-1) 0 0 (mbyte4 : str)
    | otherwise = unpack' bs (ind-1) 0 0 (badchar : str)
    where
	w = fromEnum $ B.index bs ind
	mbyte2 = chr $ ((w .&. 0x1f) `shiftL` 6) + acc
	mbyte3 = chr $ ((w .&. 0x0f) `shiftL` 12) + acc
	mbyte4 = chr $ ((w .&. 0x07) `shiftL` 18) + acc
	ismb2 = w.&.0xe0 == 0xc0
	ismb3 = w.&.0xf0 == 0xe0
	ismb4 = w.&.0xf8 == 0xf0

--------------------------------------------------
--------------------------------------------------

t_unpack = do
    let ua = map (\i -> chr (0xa0 + i)) [1..10]
	sx = "Hey man, " ++ ua ++ "  ,this is only a test"
	bs2 = pack sx
	sx2 = unpack bs2
    putStrLn sx
    putStrLn $ sx2 ++ " - len = " ++ (show (length sx2))
	       ++ " - " ++ (show (B.length bs2))

t1a i n c cnt
    | i >= n  = cnt
    | head (u8fromChar c) == 27
	= t1a (i+1) n (chr (mod i 127)) (cnt+1)
    | otherwise
	= t1a (i+1) n (chr (mod i 127)) cnt

t1 = do
    us <- liftM u8Lines (B.readFile "BIG.TXT")
    print $ length us
    let cnt = t1a 0 (10^7) 'a' 0
    putStrLn $ "cnt = " ++ show cnt


t2 = do
    let ua = map (\i -> chr (0xa0 + i)) [1..10]
	bs = pack ua

	ucloop (uncons -> Nothing) = putChar '\n'
	ucloop (uncons -> Just (c,bs2)) = do
	    putChar c
	    ucloop bs2

    putStrLn ua
    B.putStrLn bs
    ucloop bs


main = do
--    t1
--    t2
    t_unpack
