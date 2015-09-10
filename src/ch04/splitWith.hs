splitWith :: (a -> Bool) -> [a] -> [[a]]
splitWith f xs = splitWith_ f [] xs

splitWith_ :: (a -> Bool) -> [a] -> [a] -> [[a]]
splitWith_ _ [] [] = []
splitWith_ _ acc [] = [reverse acc]
splitWith_ f acc (x : xs) =
    if not (f(x))
    then (reverse acc) : splitWith_ f [] xs
    else splitWith_ f (x : acc) xs
