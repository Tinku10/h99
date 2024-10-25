-- module Symmetric where 
-- (**) Symmetric binary trees


-- Let us call a binary tree symmetric if you can draw a vertical line through the root node and then the right subtree is the mirror image of the left subtree. Write a predicate symmetric/1 to check whether a given binary tree is symmetric. Hint: Write a predicate mirror/2 first to check whether one tree is the mirror image of another. We are only interested in the structure, not in the contents of the nodes.

-- Example in Haskell:

-- λ> symmetric (Branch 'x' (Branch 'x' Empty Empty) Empty)
-- False
-- λ> symmetric (Branch 'x' (Branch 'x' Empty Empty) (Branch 'x' Empty Empty))
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


main = do
  print $ symmetric (Branch 'x' (Branch 'x' Empty Empty) Empty)
  print $ symmetric (Branch 'x' (Branch 'x' Empty Empty) (Branch 'x' Empty Empty))
