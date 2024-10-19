import Data.List (group)
-- (**) Determine the prime factors and their multiplicities of a given positive integer
 

-- Construct a list containing each prime factor and its multiplicity.

-- Example:

-- * (prime-factors-mult 315)
-- ((3 2) (5 1) (7 1))
-- Example in Haskell:

-- λ> prime_factors_mult 315
-- [(3,2),(5,1),(7,1)]

primeFactors :: Int -> [Int]
primeFactors 1 = [1]
primeFactors n = go n [2.. (ceiling . sqrt. fromIntegral) n + 1]
  where
    go 1 _ = []
    go m (x:xs)
      | m `mod` x == 0 =  x: go (m `div` x) (x:xs)
      | otherwise = go m xs

primeFactorsMul :: Int -> [(Int, Int)]
primeFactorsMul = map (\x -> (head x, length x)) . group . primeFactors

main = do
  print $ primeFactorsMul 315
