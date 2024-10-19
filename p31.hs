-- (**) Determine whether a given integer number is prime


-- Example:

-- * (is-prime 7)
-- T
-- Example in Haskell:

-- λ> isPrime 7
-- True

isPrime :: Int -> Bool
isPrime n = not ( any ((== 0) . (n `mod`)) [2..n-1])

main = do
  print $ isPrime 7
  print $ isPrime 9
