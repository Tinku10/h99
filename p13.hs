-- (**) Run-length encoding of a list (direct solution)
 

-- Implement the so-called run-length encoding data compression method directly. I.e. don't explicitly create the sublists containing the duplicates, as in problem 9, but only count them. As in problem P11, simplify the result list by replacing the singleton lists (1 X) by X.

-- Example:

-- * (encode-direct '(a a a a b c c a a d e e e e))
-- ((4 A) B (2 C) (2 A) D (4 E))
-- Example in Haskell:

-- λ> encodeDirect "aaaabccaadeeee"
-- [Multiple 4 'a',Single 'b',Multiple 2 'c',
--  Multiple 2 'a',Single 'd',Multiple 4 'e']

removeDup :: Eq a => [(a, Int)] -> [(a, Int)]
removeDup xs = foldl (\a (b, c) -> 
  if b /= fst (head a) 
    then (b, c):a 
    else a) 
    [head xs] (tail xs)

encode :: Eq a => [a] -> [(a, Int)]
encode xs = removeDup $ foldl (\a x ->
  if x == fst (head a) 
    then (x, 1 + snd (head a)):a 
    else (x, 1):a) 
    [(head xs, 1)] (tail xs)


main = do
  print $ encode "aaaabbcddddef"
