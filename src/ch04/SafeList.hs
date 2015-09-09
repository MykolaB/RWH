safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x:xs) = Just(x)


safeTail :: [a] -> Maybe [a]
safeTail [] = Nothing
safeTail [x] = Nothing
safeTail (x:xs) = Just(xs)

safeLast :: [a] -> Maybe a
safeLast [] = Nothing
safeLast [x] = Just(x)
safeLast (x: xs) = safeLast(xs)

safeInit :: [a] -> Maybe [a]
safeInit xs = let l = safeInit_ xs
              in
              if (null l) then Nothing
              else Just(l)
         where
            safeInit_ :: [a] -> [a]
            safeInit_ [] = []
            safeInit_ [x] = []
            safeInit_ (x:xs) = x : safeInit_ xs

