module Fold where

import Data.List

myFoldl :: (a -> b -> a) -> a -> [b] -> a

myFoldl step zero (x:xs) = myFoldl step (step zero x) xs
myFoldl _ zero [] = zero


myFoldr :: (b -> a -> a) -> a -> [b] -> a
myFoldr step zero (x:xs) = step x (myFoldr step zero xs)
myFoldr _ zero [] = zero

myFilter :: (a -> Bool) -> [a] -> [a]
myFilter p xs = myFoldr step [] xs
    where step x ys | p x = x : ys
                    | otherwise = ys

myFoldl2 :: (a -> b -> a) -> a -> [b] -> a
myFoldl2 f z xs = foldr step id xs z
    where step x g a  = g (f a x)

identity :: [a] -> [a]
identity xs = foldr (:) [] xs

append :: [a] -> [a] -> [a]
append xs ys = foldr (:) ys xs

myAny :: (a -> Bool) -> [a] -> Bool
myAny f xs = foldl' step False xs
    where step b a = b || f a

myCycle :: [a] -> [a]
myCycle xs = foldr step [] [1..]
    where step _ ys = xs ++ ys

myWords :: String -> [String]
myWords ws = foldr step [] ws
    where step c acc | c == ' '  = [] : acc
                     | otherwise = case acc of
                                 (x: xs) -> (c : x) : xs
                                 (_)     -> [c] : acc

myUnlines :: [String] -> String
myUnlines lines = foldr (++) "" lines
    --where step line acc = line ++ acc