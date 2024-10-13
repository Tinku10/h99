-- (**) Decode a run-length encoded list
 

-- Given a run-length code list generated as specified in problem 11. Construct its uncompressed version.

-- Example in Haskell:

-- λ> decodeModified 
--        [Multiple 4 'a',Single 'b',Multiple 2 'c',
--         Multiple 2 'a',Single 'd',Multiple 4 'e']
-- "aaaabccaadeeee"

data Tuple a = Single a | Multiple Int a

decode :: [Tuple a] -> [a]
decode = concatMap expand where
  expand (Multiple x y) = take x $ repeat y 
  expand (Single x) = [x]

main = do
  print $ decode [Multiple 4 'a', Single 'b', Multiple 5 'c', Multiple 6 'd', Single 'a']
