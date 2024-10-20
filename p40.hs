-- (**) Goldbach's conjecture


-- Goldbach's conjecture says that every positive even number greater than 2 is the sum of two prime numbers. Example: 28 = 5 + 23. It is one of the most famous facts in number theory that has not been proved to be correct in the general case. It has been numerically confirmed up to very large numbers (much larger than we can go with our Prolog system). Write a predicate to find the two prime numbers that sum up to a given even integer.

-- Example:

-- * (goldbach 28)
-- (5 23)
-- Example in Haskell:

-- λ> goldbach 28
-- (5, 23)

primes n = takeWhile (< n) $ helper [2..n]
  where
    helper [] = []
    helper (x:xs) = x: helper [y | y <- xs, y `mod` x /= 0]

isPrime :: Int -> Bool
isPrime n = not ( any ((== 0) . (n `mod`)) [2..n-1])

goldbach :: Int -> (Int, Int)
goldbach n = (head . filter (isPrime . snd) . map (\x -> (x, n-x)) . primes) n

main = do
  print $ goldbach 28

