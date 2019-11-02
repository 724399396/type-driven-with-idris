palindrome : Nat -> String -> Bool
palindrome len str = let lowerStr = toLower str
                 in if length lowerStr > len then lowerStr == (reverse lowerStr) else False


counts : String -> (Nat,Nat)
counts str = (length $ words str, length str)

top_ten : Ord a => List a -> List a
top_ten = List.take 10 . reverse . sort

over_length : Nat -> List String -> Nat
over_length n xs = List.length $ filter ((> n) . Strings.length) xs
