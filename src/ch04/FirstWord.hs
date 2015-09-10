module FirstWord where

firstWords :: String -> [String]
firstWords t = getFirstWords (lines t)
    where
    getFirstWords [] = []
    getFirstWords (x : xs) =
        if not (null (words x))
        then (head (words x)) : getFirstWords xs
        else getFirstWords xs

