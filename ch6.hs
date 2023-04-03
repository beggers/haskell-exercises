module Ch6 where

data TwoIntegers =
    Two Integer Integer

instance Eq TwoIntegers where
    (==) (Two a b) (Two x y) =
        a == x && b == y

data StringOrInt =
    AnInt Int
  | AString String

instance Eq StringOrInt where
    (==) (AnInt x) (AnInt y) = x == y
    (==) (AString s) (AString s') = s == s'
    (==) (AnInt _) (AString _) = False
    (==) (AString _) (AnInt _) = False

data EitherOr a b =
    Hello a
  | Goodbye b

instance (Eq a, Eq b) => Eq (EitherOr a b)  where
    (==) (Hello a) (Hello a') = a == a'
    (==) (Goodbye b) (Goodbye b') = b == b'
    (==) (Hello _) (Goodbye _) = False
    (==) (Goodbye _) (Hello _) = False
