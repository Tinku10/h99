-- (*) Duplicate the elements of a list


-- Example:

-- * (dupli '(a b c c d))
-- (A A B B C C C C D D)
-- Example in Haskell:

-- λ> dupli [1, 2, 3]
-- [1,1,2,2,3,3]

dup :: [a] -> [a]
dup = concatMap (\x -> [x, x])

main = do
  print $ dup [1..5]
