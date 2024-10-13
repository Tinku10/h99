-- (*) Find the number of elements in a list
 

-- Example in Haskell:

-- λ> myLength [123, 456, 789]
-- 3
-- λ> myLength "Hello, world!"
-- 13
--

-- len = foldlr (\x -> (+) 1) 0
len :: [a] -> Int
len [] = 0
len (x:xs) = 1 + len xs

main = do
  print $ len [1..100]
