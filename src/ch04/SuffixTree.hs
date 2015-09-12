import Data.List

mySuffixes :: [a] -> [[a]]
mySuffixes xs@(_:xs') = xs : mySuffixes xs'
mySuffixes _ = []

suffixes2 xs = init(tails xs)

compose :: (b -> c) -> (a -> b) -> a -> c
compose f g x = f (g x)

suffixes3 xs = compose init tails xs

suffixes5 = init . tails