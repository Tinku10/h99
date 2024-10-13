-- (**) Replicate the elements of a list a given number of times
-- Example:

-- * (repli '(a b c) 3)
-- (A A A B B B C C C)
-- Example in Haskell:

-- λ> repli "abc" 3
-- "aaabbbccc"

repli :: [a] -> Int -> [a]
repli _ 0 = []
repli xs n = concatMap (replicate n) xs

main = do
  print $ repli [1..4] 4
