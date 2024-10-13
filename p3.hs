-- (*) Find the K'th element of a list
 

-- The first element in the list is number 1. Example:

-- * (element-at '(a b c d e) 3)
-- c
-- Example in Haskell:

-- λ> elementAt [1,2,3] 2
-- 2
-- λ> elementAt "haskell" 5
-- 'e'

getKth :: [a] -> Int -> a
getKth [] _ = error "invalid index"
getKth (x:_) 0 = x
getKth (x:xs) n = getKth xs (n - 1)

main = do
  print $ getKth [1, 2, 3, 4, 5] 2
  print $ getKth [1..100] 50

