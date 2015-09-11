myTakeWhile_r :: (a -> Bool) -> [a] -> [a]
myTakeWhile_r f xs = case xs of
    []     -> []
    (y:ys) | f(y)      -> y : myTakeWhile_r f ys
           | otherwise -> []

myTakeWhile_fr' :: (a -> Bool) -> [a] -> [a]
myTakeWhile_fr' f xs = foldr step [] xs
    where step x ys    | f x = x : ys
                       | otherwise = []