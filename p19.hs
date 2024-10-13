-- (**) Rotate a list N places to the left
 

-- Hint: Use the predefined functions length and (++).

-- Examples:

-- * (rotate '(a b c d e f g h) 3)
-- (D E F G H A B C)

-- * (rotate '(a b c d e f g h) -2)
-- (G H A B C D E F)
-- Examples in Haskell:

-- λ> rotate ['a','b','c','d','e','f','g','h'] 3
-- "defghabc"

-- λ> rotate ['a','b','c','d','e','f','g','h'] (-2)
-- "ghabcdef"

rotate :: [a] -> Int -> [a]
rotate xs n
  | n > 0 = drop (mod n (length xs)) xs ++ take (mod n (length xs)) xs
  | otherwise = drop (mod (length xs + n) (length xs)) xs ++ take (mod (length xs + n) (length xs)) xs

main = do
  print $ rotate [1..10] (-16)
