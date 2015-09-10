transpose :: String -> String
transpose text = unlines (transposeLines (lines text))

transposeLines :: [String] -> [String]
transposeLines [] = []
transposeLines xs = let (f, r) = popFirstCharFromLines xs
                    in if (not (null f))
                       then f : (transposeLines r)
                       else transposeLines r

popFirstCharFromLines :: [String] -> (String, [String])
popFirstCharFromLines [] = ("", [])
popFirstCharFromLines (x : xs) = case x of
                                 "" -> ("", snd (popFirstCharFromLines xs))
                                 (c:cs) -> (c : fst (popFirstCharFromLines xs), cs : snd (popFirstCharFromLines xs))

