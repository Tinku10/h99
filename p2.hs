-- (*) Find the last-but-one (or second-last) element of a list
 

-- (Note that the Lisp transcription of this problem is incorrect.)

-- Example in Haskell:

-- λ> myButLast [1,2,3,4]
-- 3
-- λ> myButLast ['a'..'z']
-- 'y'

-- this is also good
-- getLastEle = last . init
getLastEle :: [a] -> a
getLastEle [x, y] = x
getLastEle (x:xs) = getLastEle xs

main = do
  print $ getLastEle [1, 2, 3, 4]
  print $ getLastEle ['a', 'b', 'c', 'd']
