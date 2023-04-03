module Ch9 where

import Data.Char


myWords :: String -> [String]
myWords "" = []
myWords s = (takeWhile (/= ' ') s) : myWords (dropWhile (/= ' ') s)

mySqr = [x^2 | x <- [1..5]]
myCube = [x^3 | x <- [1..5]]

tuples = [(x, y) | x <- mySqr, y <- myCube]
filteredTuples = [(x, y) | x <- mySqr, y <- myCube, x < 50, y < 50]

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (a:as) (b:bs) = [f a b] ++ zipWith' f as bs


caesar :: String -> Int -> String
caesar "" _ = ""
caesar (c:cs) n = translate c n : caesar cs n
    where translate c n = chr (mod ((ord c - ord 'a') + n) 26 + ord 'a')

myOr :: [Bool] -> Bool
myOr [] = False
myOr (b:bs)
    | b == True = True
    | otherwise = myOr bs

myAny :: (a -> Bool) -> [a] -> Bool
myAny _ [] = False
myAny f (a:as)
    | f a == True = True
    | otherwise = myAny f as

myReverse :: [a] -> [a]
myReverse [] = []
myReverse (a:as) = myReverse as ++ [a]

squish :: [[a]] -> [a]
squish [] = []
squish (l:ls) = l ++ squish ls