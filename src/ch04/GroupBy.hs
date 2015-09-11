myGroupBy :: (a -> a -> Bool) -> [a] -> [[a]]
myGroupBy f e = foldr step [] e
    where step x acc = case acc of
                           ((y:ys) : zs) | f x y     -> (x:y:ys) : zs
                                         | otherwise -> [x] : (y : ys) : zs
                           (_)                       -> [[x]]

