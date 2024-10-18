import Data.Bits (popCount, (.&.))
-- (**) Generate combinations of K distinct objects chosen from the N elements of a list


-- In how many ways can a committee of 3 be chosen from a group of 12 people? We all know that there are C(12,3) = 220 possibilities (C(N,K) denotes the well-known binomial coefficients). For pure mathematicians, this result may be great. But we want to really generate all the possibilities in a list.

-- Example:

-- * (combinations 3 '(a b c d e f))
-- ((A B C) (A B D) (A B E) ... )
-- Example in Haskell:

-- λ> combinations 3 "abcdef"
-- ["abc","abd","abe",...]

mask :: Int -> Int -> [Int]
mask n m = filter (\x -> popCount x == m) [0..(2 ^ n - 1)]
-- mask n m = filter ((== m) . popCount) [0..(2 ^ n - 1)]

combinations :: Int -> [a] -> [[a]]
combinations n xs =  map (\m -> [y | (i, y) <- indexed, (2 ^ i) .&. m /= 0]) (mask (length xs) n)
  where indexed = zip [0..length xs - 1] xs

main = do
  print $ combinations 2 [1, 2, 3, 4, 5]
