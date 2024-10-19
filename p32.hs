-- (**) Determine the greatest common divisor of two positive integer numbers
 

-- Use Euclid's algorithm.

-- Example:

-- * (gcd 36 63)
-- 9
-- Example in Haskell:

-- λ> [myGCD 36 63, myGCD (-3) (-6), myGCD (-3) 6]
-- [9,3,3]

myGCD :: Int -> Int -> Int
myGCD n m
  | m == 0 = abs n
  | otherwise = myGCD m (n `mod` m)

main = do
  print $ myGCD 36 63
