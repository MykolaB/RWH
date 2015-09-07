intersperse :: a -> [[a]] -> [a]
intersperse s xs = case xs of
    [] -> []
    [y] -> y
    (y : ys) -> y ++ [s] ++ intersperse s ys
