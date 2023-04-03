module Ch10 where

import Data.Time

data DatabaseItem = DbString String
                | DbNumber Integer
                | DbDate UTCTime
                deriving (Eq, Ord, Show)

theDatabase :: [DatabaseItem]
theDatabase =
    [ DbDate (UTCTime
            (fromGregorian 1911 5 1)
        (secondsToDiffTime 34123))
    , DbNumber 9001
    , DbString "Hello, world!"
    , DbDate (UTCTime
            (fromGregorian 1921 5 1)
            (secondsToDiffTime 34123))
    ]

filterDbDate :: [DatabaseItem] -> [UTCTime]
filterDbDate = foldr foldDate []

foldDate :: DatabaseItem -> [UTCTime] -> [UTCTime]
foldDate (DbString _) ts = ts
foldDate (DbNumber _) ts = ts
foldDate (DbDate t) ts = t : ts

filterDbNumber :: [DatabaseItem] -> [Integer]
filterDbNumber = foldr foldNumber []

foldNumber :: DatabaseItem -> [Integer] -> [Integer]
foldNumber (DbString _) ns = ns
foldNumber (DbDate _) ns = ns
foldNumber (DbNumber n) ns = n : ns

mostRecent :: [DatabaseItem] -> UTCTime
mostRecent = maximum . filterDbDate

sumDb :: [DatabaseItem] -> Integer
sumDb = sum . filterDbNumber

avgDb :: [DatabaseItem] -> Double
avgDb db = (fromIntegral $ sumDb db) / (fromIntegral $ length db)

myOr :: [Bool] -> Bool
myOr = foldr (||) False

myAny :: (a -> Bool) -> [a] -> Bool
myAny f as = myOr $ map f as

myReverse :: [a] -> [a]
myReverse = foldr (\a as -> as ++ [a]) []

myFilter :: (a -> Bool) -> [a] -> [a]
myFilter f = foldr (\a as -> if f a then a : as else as) []