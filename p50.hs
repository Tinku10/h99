import Data.List (sortBy)
import Data.Function (on)
import Data.Ord (comparing)
-- (***) Huffman codes


-- We suppose a set of symbols with their frequencies, given as a list of fr(S,F) terms. Example: [fr(a,45),fr(b,13),fr(c,12),fr(d,16),fr(e,9),fr(f,5)]. Our objective is to construct a list hc(S,C) terms, where C is the Huffman code word for the symbol S. In our example, the result could be Hs = [hc(a,'0'), hc(b,'101'), hc(c,'100'), hc(d,'111'), hc(e,'1101'), hc(f,'1100')] [hc(a,'01'),...etc.]. The task shall be performed by the predicate huffman/2 defined as follows:

-- % huffman(Fs,Hs) :- Hs is the Huffman code table for the frequency table Fs
-- Example in Haskell:

-- λ> huffman [('a',45),('b',13),('c',12),('d',16),('e',9),('f',5)]
-- [('a',"0"),('b',"101"),('c',"100"),('d',"111"),('e',"1101"),('f',"1100")s[k:n]]

data Tree = Empty | Node Tree String Int Tree deriving Show

showTree :: Tree -> String
showTree Empty = ""
showTree (Node left a b right) = showTree left ++ "(" ++ a ++ ", " ++ show b ++ ")" ++ showTree right

mergeTree :: Tree -> Tree -> Tree
mergeTree Empty right = right
mergeTree left Empty = left
mergeTree left@(Node _ c1 f1 _) right@(Node _ c2 f2 _)
  | f1 < f2 = Node left (c1 ++ c2) (f1 + f2) right
  | otherwise = Node right (c2 ++ c1) (f1 + f2) left

getFreq :: Tree -> Int
getFreq Empty = 0
getFreq (Node _ _ a _) = a

orchestrate :: [Tree] -> [Tree]
orchestrate [] = []
orchestrate [x] = [x]
orchestrate tree@(x:y:xs) = orchestrate $ sortBy (comparing getFreq) (mergeTree x y:xs)

buildTree :: [(String, Int)] -> [Tree]
buildTree = orchestrate . nodes
  where
    nodes :: [(String, Int)] -> [Tree]
    nodes = sortBy (\x y -> compare (getFreq x) (getFreq y)) . map (\x -> uncurry (Node Empty) x Empty)

traverseTree :: String -> Tree -> [(String, String)]
traverseTree c (Node Empty x _ Empty) = [(x, c)]
traverseTree c (Node left x _ right) = (x, c): traverseTree (c ++ "0") left ++ traverseTree (c ++ "1") right

huffman :: [(String, Int)] -> [(String, String)]
huffman = filter (\x -> length (fst x) == 1) . traverseTree "" . head . buildTree

main = do
  print $ huffman [("a",45),("b",13),("c",12),("d",16),("e",9),("f",5)]
  -- showTree $ mergeTree (Node Empty "a" 1 Empty) (Node (Node Empty "c" 5 Empty) "cd" 7 (Node Empty "d" 2 Empty))

