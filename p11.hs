import Data.List (group)
-- Problem 11
-- (*) Modified run-length encoding
 

-- Modify the result of problem 10 in such a way that if an element has no duplicates it is simply copied into the result list. Only elements with duplicates are transferred as (N E) lists.

-- Example:

-- * (encode-modified '(a a a a b c c a a d e e e e))
-- ((4 A) B (2 C) (2 A) D (4 E))
-- Example in Haskell:

-- λ> encodeModified "aaaabccaadeeee"
-- [Multiple 4 'a',Single 'b',Multiple 2 'c',
--  Multiple 2 'a',Single 'd',Multiple 4 'e']

data Tuple a = Single a | Multiple Int a -- deriving Show

instance Show a => Show (Tuple a) where
    show (Single x) = "(" ++ show x ++ ")"
    show (Multiple x y) = "(" ++ show x ++ ", " ++ show y ++ ")"

encode :: Eq a => [a] -> [Tuple a]
encode [] = []
encode xs = (map (\x -> 
  if length x == 1 
    then Single (head x) 
    else Multiple (length x) (head x)) . group) xs


main = do
  print $ encode "aaaabbcddddefgh"
