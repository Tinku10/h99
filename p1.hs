-- (*) Find the last element of a list.
-- λ> myLast [1,2,3,4]
-- 4
-- λ> myLast ['x','y','z']
-- 'z'
--

-- we could always use the `last` function
getLastEle :: [a] -> a
getLastEle [x] = x
getLastEle (x:xs) = getLastEle xs

main = do
  print $ getLastEle [1, 2, 3, 4]
  print $ getLastEle ['a', 'b', 'c', 'd']
  
