import Data.List (groupBy, sort)
-- (*) A list of prime numbers in a given range


-- Given a range of integers by its lower and upper limit, construct a list of all prime numbers in that range.

-- Example in Haskell:

-- λ> primesR 10 20
-- [11,13,17,19]

sieve :: Int -> [Int]
sieve n = map (fst . head) $ filter (\x -> length x == 1) $ groupBy (\a b -> fst a == fst b) $ 
  sort $ map snd $ 
    concatMap (\(x:xs) -> (fst x, ((fst . snd) x, 1)):xs) 
      (groupBy (\a b -> fst a == fst b)  
        [(x, y) | x <- [2..n], y <- map (, -1) [x,x*2..n]])
-- sieve n = helper [2..n]
--   where
--     helper [] = []
--     helper (x:xs) = x: helper [y | y <- xs, y `mod` x /= 0]

primesR :: Int -> Int -> [Int]
primesR n m = filter (\x -> x >= n && x < m) $ sieve m
main = do
  -- print $ sieve 16
  print $ primesR 10 20
