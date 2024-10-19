-- (**) Determine the prime factors of a given positive integer


-- Construct a flat list containing the prime factors in ascending order.

-- Example:

-- * (prime-factors 315)
-- (3 3 5 7)
-- Example in Haskell:

-- λ> primeFactors 315
-- [3, 3, 5, 7]

divide :: Int -> Int -> Int
divide n m
  | n == 0 = 0
  | n `mod` m == 0 = divide (n `div` m) m
  | otherwise = n


primeFactors :: Int -> [Int]
primeFactors 1 = [1]
primeFactors n = go n [2.. (ceiling . sqrt. fromIntegral) n + 1]
  where
    go 1 _ = []
    go m (x:xs)
      | m `mod` x == 0 =  x: go (m `div` x) (x:xs)
      | otherwise = go m xs

primes :: Int -> [Int]
primes 1 = [1]
primes n = filter (\x -> 0 `notElem` ([x `mod` m | m <- [2..x-1] ++ [x+1.. (ceiling . sqrt. fromIntegral) n + 1]])) [2..n - 1]
-- primes n = filter (\x -> not $ any (== 0) [x `mod` m | m <- [2..x-1] ++ [x+1.. (ceiling . sqrt. fromIntegral) n + 1]]) [2..n - 1]

main = do
  print $ primeFactors 315
  print $ primes 10
