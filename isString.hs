{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
-- {-# LANGUAGE OverlappingInstances #-}
-- {-# LANGUAGE NamedFieldPuns #-}

module Main (
    main
) where

import Data.String

data Str = Str{str :: String, iii :: Int}  deriving (Eq,Ord,Show)

instance IsString Str where
    fromString cs = (Str cs 0)

putter :: Str -> String
putter s = str s

------------------------

data Jpd a = Jpd a deriving (Show)

type Stack = [String]

pop :: Jpd Stack -> Jpd Stack
pop (Jpd (x:xs)) = Jpd xs

push :: String -> Stack -> Jpd Stack
push a xs = Jpd (a:xs)

instance Monad Jpd where
    return a = Jpd a
    Jpd a >>= f = f a

instance IsString (Stack -> Jpd Stack) where
    fromString cs = push cs

-- you can not have 2 string literals in a row
-- ex. xxx = push "Now" [] >>= "Hey" >>= ("!") will fail to type check
-- I need to find out if another compiler option will help
xxx :: Jpd Stack
xxx = push "Now" [] >>= "Hey" >>= push "now"  >>= ("!")

yyy :: Jpd Stack
yyy = do
    a <- return ["Now"]
    push "hey" a


------------------------

main = do
    putStrLn $ putter "hey again"
    print xxx
