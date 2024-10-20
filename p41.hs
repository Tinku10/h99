import Data.List (groupBy, sortBy)
-- (**) A list of even numbers and their Goldbach compositions in a given range


-- Given a range of integers by its lower and upper limit, print a list of all even numbers and their Goldbach composition.

-- In most cases, if an even number is written as the sum of two prime numbers, one of them is very small. Very rarely, the primes are both bigger than say 50. Try to find out how many such cases there are in the range 2..3000.

-- Example:

-- * (goldbach-list 9 20)
-- 10 = 3 + 7
-- 12 = 5 + 7
-- 14 = 3 + 11
-- 16 = 3 + 13
-- 18 = 5 + 13
-- 20 = 3 + 17
-- * (goldbach-list 1 2000 50)
-- 992 = 73 + 919
-- 1382 = 61 + 1321
-- 1856 = 67 + 1789
-- 1928 = 61 + 1867
-- Example in Haskell:

-- λ> goldbachList 9 20
-- [(3,7),(5,7),(3,11),(3,13),(5,13),(3,17)]
-- λ> goldbachList' 4 2000 50
-- [(73,919),(61,1321),(67,1789),(61,1867)]

primes n = takeWhile (< n) $ helper [2..n]
  where
    helper [] = []
    helper (x:xs) = x: helper [y | y <- xs, y `mod` x /= 0]

isPrime :: Int -> Bool
isPrime n = not ( any ((== 0) . (n `mod`)) [2..n-1])

-- goldbachList :: Int -> Int -> [(Int, Int)]
goldbachList n m = (
  map (snd . head) 
  . groupBy (\x y -> fst x == fst y) 
  . sortBy (\x y -> compare (fst x) (fst y)) 
  . go 
  . filter (even . uncurry (+))) 
    [(x, y) | x <- primes m, y <- primes m, x /= y, x < y, x + y <= m, x + y >= n]
  where
    go [] = []
    go (x:xs) = (uncurry (+) x, x): go xs
-- goldbachList n m = (filter (even . uncurry (+)) . filter (isPrime . snd) . map (\x -> (x, m-x)) . primes) m

main = do
  print $ goldbachList 9 20

