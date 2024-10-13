-- (**) Flatten a nested list structure


-- Transform a list, possibly holding lists as elements into a `flat' list by replacing each list with its elements (recursively).

-- Example:

-- * (my-flatten '(a (b (c d) e)))
-- (A B C D E)
-- Example in Haskell:

-- We have to define a new data type, because lists in Haskell are homogeneous.

--  data NestedList a = Elem a | List [NestedList a]
-- λ> flatten (Elem 5)
-- [5]
-- λ> flatten (List [Elem 1, List [Elem 2, List [Elem 3, Elem 4], Elem 5]])
-- [1,2,3,4,5]
-- λ> flatten (List [])
-- []

data Array a = Elem a | List [Array a]

flatten :: Array a -> [a]
flatten (Elem a) = [a]
flatten (List xs) = foldl (\a x -> a ++ flatten x) [] xs

main = do
  print $ flatten (List [Elem 1, Elem 2, List [Elem 3, Elem 4, List [Elem 5]]])
