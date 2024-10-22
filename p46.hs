-- (**) Truth tables for logical expressions


-- Define predicates and/2, or/2, nand/2, nor/2, xor/2, impl/2 and equ/2 (for logical equivalence) which succeed or fail according to the result of their respective operations; e.g. and(A,B) will succeed, if and only if both A and B succeed.

-- A logical expression in two variables can then be written as in the following example: and(or(A,B),nand(A,B)).

-- Now, write a predicate table/3 which prints the truth table of a given logical expression in two variables.

-- Example:

-- (table A B (and A (or A B)))
-- true true true
-- true fail true
-- fail true fail
-- fail fail fail
-- Example in Haskell:

-- λ> table (\a b -> (and' a (or' a b)))
-- True True True
-- True False True
-- False True False
-- False False False

table :: (Bool -> Bool -> Bool) -> IO ()
table f = putStrLn $ concatMap (++ "\n") ([show x ++ " " ++ show y ++ " " ++ show (f x y) | x <- [True, False], y <- [True, False]])

main = do
  table (||)
