module Ch8 where

import Data.List (intersperse)

mc91 :: Integral a => a -> a
mc91 n
    | n > 100 = n - 10
    | otherwise = mc91(mc91(n + 11))


digitToWord :: Int -> String
digitToWord n
    | n == 0 = "zero"
    | n == 1 = "one"
    | n == 2 = "two"
    | n == 3 = "three"
    | n == 4 = "four"
    | n == 5 = "five"
    | n == 6 = "six"
    | n == 7 = "seven"
    | n == 8 = "eight"
    | n == 9 = "nine"
    | otherwise = error "Put in a number 0-9!"

digits :: Int -> [Int]
digits n
    | n < 10 = [n]
    | otherwise = (digits (div n 10)) ++ [mod n 10]

wordNumber :: Int -> String
wordNumber = concat . intersperse "-" . map digitToWord . digits
