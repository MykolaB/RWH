import Data.Char (digitToInt, isDigit)

asInt :: String -> Int
asInt xs = fst (asInt' xs)

asInt' :: String -> (Int, Int)
asInt' xs = foldr (f) (0, 1) xs
    where f c (a, p) = (a + digitToInt c * p, p * 10)


asInt_l :: String -> Int
asInt_l xs =
    let
        (sign, num) = case xs of
                      ('-':ys) -> (-1,ys)
                      (_)    -> (1, xs)
    in sign * (foldl step 0 num)
    where step x y
            | isDigit(y) = x * 10 + digitToInt y
            | otherwise = error (y : " - non digit symbol")