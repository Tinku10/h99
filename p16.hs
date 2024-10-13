-- (**) Drop every N'th element from a list
 

-- Example:

-- * (drop '(a b c d e f g h i k) 3)
-- (A B D E G H K)
-- Example in Haskell:

-- λ> dropEvery "abcdefghik" 3
-- "abdeghk"

dropEvery :: [a] -> Int -> [a]
dropEvery xs n = reverse $ map snd $ filter (\x -> mod (fst x) n /= 0) $ foldl (\a x -> (1 + length a, x):a) [] xs

main = do
  print $ dropEvery "abcdefgh" 2
