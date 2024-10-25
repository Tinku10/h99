-- import Symmetric
-- (**) Binary search trees


-- Use the predicate add/3, developed in chapter 4 of the course, to write a predicate to construct a binary search tree from a list of integer numbers.

-- Example:

-- * construct([3,2,5,7,1],T).
-- T = t(3, t(2, t(1, nil, nil), nil), t(5, nil, t(7, nil, nil)))
-- Then use this predicate to test the solution of Problem 56.

-- Example:

-- * test-symmetric([5,3,18,1,4,12,21]).
-- Yes
-- * test-symmetric([3,2,5,7,4]).
-- No
-- Example in Haskell:

-- λ> construct [3, 2, 5, 7, 1]
-- Branch 3 (Branch 2 (Branch 1 Empty Empty) Empty) (Branch 5 Empty (Branch 7 Empty Empty))
-- λ> symmetric . construct $ [5, 3, 18, 1, 4, 12, 21]
-- True
-- λ> symmetric . construct $ [3, 2, 5, 7, 1]
-- True

data Tree a = Empty | Branch a (Tree a) (Tree a) deriving Show

symmetric :: Tree a -> Bool
symmetric Empty = True
symmetric (Branch _ left right) = isSymmetric left right
  where
    isSymmetric :: Tree a -> Tree a -> Bool
    isSymmetric Empty Empty = True
    isSymmetric Empty (Branch {}) = False
    isSymmetric (Branch {})  Empty = False
    isSymmetric (Branch _ l1 r1) (Branch _ l2 r2) = isSymmetric l1 r2 && isSymmetric l2 r1

helper :: Ord a => Tree a -> a -> Tree a
helper Empty a = Branch a Empty Empty
helper (Branch a left right) b = case compare b a of
  LT -> Branch a (helper left b) right
  GT -> Branch a left (helper right b)
  _  -> error "Invalid binary tree"

construct :: Ord a => [a] -> Tree a
construct = foldl helper Empty

main = do
  print $ (symmetric . construct) [3, 2, 5, 7, 1]
  print $ (symmetric . construct) [5, 3, 18, 1, 4, 12, 21]

