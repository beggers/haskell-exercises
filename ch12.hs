module Ch12 where

isJust :: Maybe a -> Bool
isJust Nothing = False
isJust (Just _) = True

isNothing :: Maybe a -> Bool
isNothing = not . isJust

mayybe :: b -> (a -> b) -> (Maybe a) -> b
mayybe b _ Nothing = b
mayybe b f (Just a) = f a