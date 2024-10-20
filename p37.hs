import Data.List (group)
-- (**) Calculate Euler's totient function phi(m) (improved)


-- See Problem 34 for the definition of Euler's totient function. If the list of the prime factors of a number m is known in the form of problem 36 then the function phi(m) can be efficiently calculated as follows: Let ((p1 m1) (p2 m2) (p3 m3) ...) be the list of prime factors (and their multiplicities) of a given number m. Then phi(m) can be calculated with the following formula:

-- phi(m) = (p1 - 1) * p1 ** (m1 - 1) * 
--          (p2 - 1) * p2 ** (m2 - 1) * 
--          (p3 - 1) * p3 ** (m3 - 1) * ...
-- Note that a ** b stands for the b'th power of a.

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

phi :: Int -> Int
phi n =  product  [(x - 1) * x  ^ (y - 1) | (x, y) <- primeFactorsMul n]

main = do
  print $ primeFactorsMul 10
  print $ phi 10

