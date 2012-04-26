import Data.Char

encrypt :: Int -> String -> String
encrypt n s =  map (f1 n) s where
    f1 n' c = chr ((ord c + n') `mod` 256)

decrypt :: Int -> String -> String
decrypt n s =  map (f2 n) s where
    f2 n' c = chr ((ord c - n') `mod` 256)

main = do
    let s="hey now"
    let se = encrypt 50 s
    let sd = decrypt 50 se
    print $ show s
    print $ show se
    print $ show sd
