{-# LANGUAGE ViewPatterns #-}
import Data.List
import Control.Monad
import qualified Data.ByteString as B
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

--
-- UNCONS
--

badchar = '\xfffd'

{-
u8toChar :: [Word8] -> (Char, [Word8])
u8toChar (w:ws)
    | w < 0x80 = (chr $ fromIntegral w, ws)
    | w < 0xc0 = (badchar, ws)
    | w < 0xe0 = if not $ null ws then byte2 w ws else (badchar, ws)
    | w < 0xf0 = if length ws >= 2 then byte3 w ws else (badchar, ws)
    | w < 0xf8 = if length ws >= 3 then byte4 w ws else (badchar, ws)
    | otherwise = (badchar, ws)
-}

uncons :: B.ByteString -> Maybe (Char, B.ByteString)
uncons (B.null -> True) = Nothing
uncons (B.uncons -> Just (w, bs))
    | w < 0x80 = Just (chr $ fromIntegral w, bs)
    | w < 0xc0 = Just (badchar, bs)
    | w < 0xe0 = if not (B.null bs)  then Just (uncons2 w bs)
				     else Just (badchar, bs)
    | w < 0xf0 = if B.length bs >= 2  then Just (uncons3 w bs)
				      else Just (badchar, bs)
    | w < 0xf0 = if B.length bs >= 2  then Just (uncons4 w bs)
				      else Just (badchar, bs)

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
    chr $ ((fromIntegral w .&. 0x1f) `shiftL` 6)
	  + (fromIntegral w2 .&. 0x3f )

byte3 :: Word8 -> Word8 -> Word8 -> Char
byte3 w w2 w3 =
    chr $  ((fromIntegral w .&. 0x0f) `shiftL` 12)
	 + ((fromIntegral w2 .&. 0x3f) `shiftL` 6)
	 +  (fromIntegral w3 .&. 0x3f )

byte4 :: Word8 -> Word8 -> Word8 -> Word8 -> Char
byte4 w w2 w3 w4 =
	chr $  ((fromIntegral w .&. 0x07) `shiftL` 18)
	      + ((fromIntegral w2 .&. 0x3f) `shiftL` 12)
	      + ((fromIntegral w3 .&. 0x3f) `shiftL` 6)
	      +  (fromIntegral w4 .&. 0x3f )

chk2 w2 = w2 .&. 0xc0 == 0x80
chk3 w2 w3 = w2 .&. 0xc0 == 0x80 && w3 .&. 0xc0 == 0x80
chk4 w2 w3 w4 = w2 .&. 0xc0 == 0x80 && w3 .&. 0xc0 == 0x80 && w4 .&. 0xc0 == 0x80

--------------------------------------------------
--------------------------------------------------

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

{-
t2 c = do
    let w = u8fromChar c
	wi = map fromIntegral w :: [Int]
    mapM (printf "%x ")  wi
    putStrLn ""

    let (oc,_) = u8toChar w
    printf "c=%c oc=%c\n\n" c oc
-}

main = do
    t1
 --   t2 (chr 0xa2)
 --   t2 ('a')
