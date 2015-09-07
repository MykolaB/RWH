data List a = Cons a (List a)
    | Nil
      deriving (Show)

--fromList (x:xs) = Cons x (fromList xs)
--fromList [] = Nil

fromList (Cons x xs) = x : fromList(xs)
fromList Nil = []

