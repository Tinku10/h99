import Data.List (permutations)
-- Group the elements of a set into disjoint subsets


-- a) In how many ways can a group of 9 people work in 3 disjoint subgroups of 2, 3 and 4 persons? Write a function that generates all the possibilities and returns them in a list.

-- Example:

-- * (group3 '(aldo beat carla david evi flip gary hugo ida))
-- ( ( (ALDO BEAT) (CARLA DAVID EVI) (FLIP GARY HUGO IDA) )
-- ... )
-- b) Generalize the above predicate in a way that we can specify a list of group sizes and the predicate will return a list of groups.

-- Example:

-- * (group '(aldo beat carla david evi flip gary hugo ida) '(2 2 5))
-- ( ( (ALDO BEAT) (CARLA DAVID) (EVI FLIP GARY HUGO IDA) )
-- ... )
-- Note that we do not want permutations of the group members; i.e. ((ALDO BEAT) ...) is the same solution as ((BEAT ALDO) ...). However, we make a difference between ((ALDO BEAT) (CARLA DAVID) ...) and ((CARLA DAVID) (ALDO BEAT) ...).

-- You may find more about this combinatorial problem in a good book on discrete mathematics under the term "multinomial coefficients".

-- Example in Haskell:

-- λ> group [2,3,4] ["aldo","beat","carla","david","evi","flip","gary","hugo","ida"]
-- [[["aldo","beat"],["carla","david","evi"],["flip","gary","hugo","ida"]],...]
-- (altogether 1260 solutions)

-- λ> group [2,2,5] ["aldo","beat","carla","david","evi","flip","gary","hugo","ida"]
-- [[["aldo","beat"],["carla","david"],["evi","flip","gary","hugo","ida"]],...]
-- (altogether 756 solutions)

altogether :: [Int] -> [a] -> [[[a]]]
altogether ns xs = map (\x -> map (\(a, b) -> (take b . drop a) x) (trans ns)) (permutations xs)
  where trans = zip rx
        rx = (reverse . foldl (\a x -> (head a + x):a) [0]) ns

main = do
  print $ altogether [1, 2] ["aldo","beat","carla"]
