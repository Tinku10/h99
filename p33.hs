-- (*) Determine whether two positive integer numbers are coprime
 

-- Two numbers are coprime if their greatest common divisor equals 1.

-- Example:

-- * (coprime 35 64)
-- T
-- Example in Haskell:

-- λ> coprime 35 64
-- True

coprime :: Int -> Int -> Bool
coprime a b = (== 1) $ gcd a b

main = do
  print $ coprime 35 64
