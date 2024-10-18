-- Insert an element at a given position into a list
 

-- Example:

-- * (insert-at 'alfa '(a b c d) 2)
-- (A ALFA B C D)
-- Example in Haskell:

-- λ> insertAt 'X' "abcd" 2
-- "aXbcd"

insertAt :: a -> [a] -> Int -> [a]
insertAt c xs n = (transform . splitAt (n - 1)) xs
  where transform (a, b) = a ++ [c] ++ b

main = do
  print $ insertAt 'X' "abcd" 2
