import Distribution.Simple.Utils (xargs)
-- (*) Remove the K'th element from a list


-- Example in Prolog:

-- ?- remove_at(X,[a,b,c,d],2,R).
-- X = b
-- R = [a,c,d]
-- Example in Lisp:

-- * (remove-at '(a b c d) 2)
-- (A C D)
-- (Note that this only returns the residue list, while the Prolog version also returns the deleted element.)

-- Example in Haskell:

-- λ> removeAt 2 "abcd"
-- ('b',"acd")

removeAt :: Int -> [a] -> (a, [a])
removeAt n [] = error "invalid index"
removeAt n xs
  | n > length xs = error "invalid index"
  | otherwise = (xs !! (n - 1), init (fst parts) ++ snd parts)
    where parts = splitAt n xs
  

main = do
  print $ removeAt 3 "abcdef"
  print $ removeAt 10 [1..20]
