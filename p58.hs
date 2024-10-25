-- (**) Generate-and-test paradigm


-- Apply the generate-and-test paradigm to construct all symmetric, completely balanced binary trees with a given number of nodes.

-- Example:

-- * sym-cbal-trees(5,Ts).
-- Ts = [t(x, t(x, nil, t(x, nil, nil)), t(x, t(x, nil, nil), nil)), t(x, t(x, t(x, nil, nil), nil), t(x, nil, t(x, nil, nil)))] 
-- Example in Haskell:

-- λ> symCbalTrees 5
-- [Branch 'x' (Branch 'x' Empty (Branch 'x' Empty Empty)) (Branch 'x' (Branch 'x' Empty Empty) Empty),Branch 'x' (Branch 'x' (Branch 'x' Empty Empty) Empty) (Branch 'x' Empty (Branch 'x' Empty Empty))]

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

balTrees :: Int -> [Tree Char]
balTrees 0 = [Empty]
balTrees 1 = [Branch 'x' Empty Empty]
balTrees n 
  | even n = [Branch 'x' l r | l <- lcomb n, r <- rcomb n] ++ [Branch 'x' l r | l <- rcomb n, r <- lcomb n]
  | otherwise = [Branch 'x' l r | l <- rcomb n, r <- rcomb n]
    where 
      lcomb :: Int -> [Tree Char]
      lcomb n = balTrees ((n `div` 2) - 1)
      rcomb :: Int -> [Tree Char]
      rcomb n = balTrees (n `div` 2)

symCbalTrees :: Int -> [Tree Char]
symCbalTrees n = filter symmetric (balTrees n)

main = do
  print $ symCbalTrees 5
