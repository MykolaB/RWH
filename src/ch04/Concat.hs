myConcat :: [[a]] -> [a]

myConcat = foldr (++) []
