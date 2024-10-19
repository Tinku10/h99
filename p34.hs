import Data.List (group)
-- (**) Calculate Euler's totient function phi(m)
 

-- Euler's so-called totient function phi(m) is defined as the number of positive integers r (1 <= r < m) that are coprime to m.

-- Example: m = 10: r = 1,3,7,9; thus phi(m) = 4. Note the special case: phi(1) = 1.

-- Example:

-- * (totient-phi 10)
-- 4
-- Example in Haskell:

-- λ> totient 10
-- 4

totient :: Int -> Int
totient 1 = 1
totient n = length $ [x | x <- [1..n-1], gcd n x == 1]

main = do
  print $ totient 16
  print $ totient 10
