-- (***) Construct height-balanced binary trees with a given number of nodes


-- Consider a height-balanced binary tree of height H. What is the maximum number of nodes it can contain?

-- Clearly, MaxN = 2^H - 1. However, what is the minimum number MinN? This question is more difficult. Try to find a recursive statement and turn it into a function minNodes that returns the minimum number of nodes in a height-balanced binary tree of height H.

-- On the other hand, we might ask: what is the maximum height H a height-balanced binary tree with N nodes can have? Write a function maxHeight that computes this.

-- Now, we can attack the main problem: construct all the height-balanced binary trees with a given number of nodes. Find out how many height-balanced trees exist for N = 15.

-- Example in Prolog:

-- ?- count_hbal_trees(15,C).
-- C = 1553
-- Example in Haskell:

-- λ> length $ hbalTreeNodes 'x' 15
-- 1553
-- λ> map (hbalTreeNodes 'x') [0..3]
-- [[Empty],
--  [Branch 'x' Empty Empty],
--  [Branch 'x' Empty (Branch 'x' Empty Empty),Branch 'x' (Branch 'x' Empty Empty) Empty],
--  [Branch 'x' (Branch 'x' Empty Empty) (Branch 'x' Empty Empty)]]

data Tree a = Empty | Branch a (Tree a) (Tree a) deriving Show

maxHeight :: Int -> Int
maxHeight n = (ceiling. logBase 2.0 . fromIntegral) (n + 1)

hbalTreesNodes :: a -> Int -> [(Tree a, Int)]
hbalTreesNodes c 0 = [(Empty, 0)]
hbalTreesNodes c 1 = [(Branch c Empty Empty, 1)]
hbalTreesNodes c n = [(Branch c l r, h) |
                        -- (lh, rh) <- zip [0 .. maxHeight n - 1] [maxHeight n - 1 .. 0],
                        -- let h = max lh rh + 1,
                        -- abs (lh - rh) <= 1,
                        (ln, rn) <- zip [0 .. n - 1] [n - 1, n - 2 .. 0],
                        (l, lh) <- hbalTreesNodes c ln,
                        (r, rh) <- hbalTreesNodes c rn, 
                        let h = max lh rh + 1,
                        abs (lh - rh) <= 1]
-- hbalTreesNodes c n = [Branch c l r |
--                            (x, y) <- if even n then [(n `div` 2, n `div` 2 - 1), (n `div` 2 - 1, n `div` 2)] else [(n `div` 2, n `div` 2)],
--                            l <- hbalTreesNodes c x,
--                            r <- hbalTreesNodes c y]

main = do
  -- print $ sum $ map (length . hbalTreesNodes 'x') [0..15]
  print $ maxHeight 2
  print $ length $ hbalTreesNodes 'x' 15
