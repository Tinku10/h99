-- (**) Eliminate consecutive duplicates of list elements


-- If a list contains repeated elements they should be replaced with a single copy of the element. The order of the elements should not be changed.

-- Example:

-- * (compress '(a a a a b c c a a d e e e e))
-- (A B C A D E)
-- Example in Haskell:

-- λ> compress "aaaabccaadeeee"
-- "abcade"

compress :: Eq a => [a] -> [a]
-- compress xs = reverse $ foldl (\a x -> if x == head a then a else x:a) [head xs] (tail xs)
compress [] = []
compress [x] = [x]
compress (x:xs)
  | x /= head xs = x: compress xs
  | otherwise = compress xs

main = do
  print $ compress "aafffffdfcdfdfdfdffffff"
