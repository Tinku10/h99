-- (*) Reverse a list
 

-- Example in Haskell:

-- λ> myReverse "A man, a plan, a canal, panama!"
-- "!amanap ,lanac a ,nalp a ,nam A"
-- λ> myReverse [1,2,3,4]
-- [4,3,2,1]

rev :: [a] -> [a]
rev [] = []
rev x = last x: rev (init x)
-- rev = foldl (\a x -> x:a) []

main = do
  print $ rev [1..100]
