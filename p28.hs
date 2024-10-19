import Data.Foldable (toList)
import Data.List (sortBy, groupBy)
-- Sorting a list of lists according to length of sublists
 

-- a) We suppose that a list contains elements that are lists themselves. The objective is to sort the elements of this list according to their length. E.g. short lists first, longer lists later, or vice versa.

-- Example:

-- * (lsort '((a b c) (d e) (f g h) (d e) (i j k l) (m n) (o)))
-- ((O) (D E) (D E) (M N) (A B C) (F G H) (I J K L))
-- Example in Haskell:

-- λ> lsort ["abc","de","fgh","de","ijkl","mn","o"]
-- ["o","de","de","mn","abc","fgh","ijkl"]
-- b) Again, we suppose that a list contains elements that are lists themselves. But this time the objective is to sort the elements of this list according to their length frequency; i.e., in the default, where sorting is done ascendingly, lists with rare lengths are placed first, others with a more frequent length come later.

-- Example:

-- * (lfsort '((a b c) (d e) (f g h) (d e) (i j k l) (m n) (o)))
-- ((i j k l) (o) (a b c) (f g h) (d e) (d e) (m n))
-- Example in Haskell:

-- λ> lfsort ["abc", "de", "fgh", "de", "ijkl", "mn", "o"]
-- ["ijkl","o","abc","fgh","de","de","mn"]

lsort :: (Ord a, Foldable c, Ord (c a)) => [c a] -> [c a]
lsort = map snd . sortBy compareTuple . map (\x -> (length x, x))
  where 
    compareTuple (n, a) (m, b) = case compare n m of
      EQ -> compare a b
      other -> other

lfsort :: (Ord a, Foldable c, Ord (c a)) => [c a] -> [c a]
lfsort = concat . lsort . groupBy equalLen . lsort
  where equalLen a b = length a == length b

main = do
  print $ lsort ["efg", "abc", "def", "a", "bc", "defg"]
  print $ lfsort ["efg", "abc", "def", "a", "bc", "defg"]
