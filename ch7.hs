module Ch7 where 

addFive = \x -> \y -> (if x > y then y else x) + 5

mflip f x y = f y x

functionC x y = case x > y of
    True -> x
    False -> y

addTwoIfEven x = case even x of
    True -> x + 2
    False -> x

g :: (a, b) -> (a, c) -> (b, c)
g (a, b) (c, d) = (b, d)

roundTrip :: (Show a, Read a) => a -> a
roundTrip x = read (show x)

roundTripPF :: (Show a, Read a) => a -> a
roundTripPF = read . show

roundTrip3 :: (Show a, Read b) => a -> b
roundTrip3 x = (read (show x)) :: a
