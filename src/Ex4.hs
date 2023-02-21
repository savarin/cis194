-- https://www.cis.upenn.edu/~cis1940/spring13/hw/03-rec-poly.pdf

module Ex4
    ( fun1,
      fun2,
      seed,
      grow,
      foldTree
    ) where

-- Exercise 1: Wholemeal programming
-- Reimplement each of the following functions in a more idiomatic Haskell
-- style. Use wholemeal programming practices, breaking each function into a
-- pipeline of incremental transformations to an entire data structure. Name
-- your functions fun1’ and fun2’ respectively.

-- 1. fun1 :: [Integer] -> Integer
--    fun1 [] = 1
--    fun1 (x:xs)
--      | even x    = (x - 2) * fun1 xs
--      | otherwise = fun1 xs

-- 2. fun2 :: Integer -> Integer
--    fun2 1 = 0
--    fun2 n
--      | even n    = n + fun2 (n ‘div‘ 2)
--      | otherwise = fun2 (3 * n + 1)

-- Hint: For this problem you may wish to use the functions iterate and
-- takeWhile. Look them up in the Prelude documentation to see what they do.

fun1 :: [Integer] -> Integer
fun1 = foldl (\acc x -> acc * (x - 2)) 1 . filter even

fun2 :: Integer -> Integer
fun2 = sum . filter even .takeWhile (>1) . iterate (\n -> if even n then div n 2 else 3 * n + 1)


-- Exercise 2: Folding with trees
-- Recall the definition of a binary tree data structure. The height of a binary
-- tree is the length of a path from the root to the deepest node. For example,
-- the height of a tree with a single node is 0; the height of a tree with three
-- nodes, whose root has two children, is 1; and so on. A binary tree is
-- balanced if the height of its left and right subtrees differ by no more than
-- 1, and its left and right subtrees are also balanced.

-- You should use the following data structure to represent binary trees. Note
-- that each node stores an extra Integer representing the height at that node.

--   data Tree a = Leaf
--               | Node Integer (Tree a) a (Tree a)
--     deriving (Show, Eq)

-- For this exercise, write a function
--   foldTree :: [a] -> Tree a
--   foldTree = ...
-- which generates a balanced binary tree from a list of values using foldr.

data Tree a = Leaf
            | Node Integer (Tree a) a (Tree a)
  deriving (Show, Eq)

seed :: Tree a
seed = Leaf

countNodes :: Tree a -> Integer
countNodes Leaf = 0
countNodes (Node _ left x right) = (countNodes left) + 1 + (countNodes right)

isBalanced :: Tree a -> Bool
isBalanced Leaf = True
isBalanced tree@(Node n _ _ _) = countNodes tree == 2^(n + 1) - 1

addHeight :: Integer -> Tree a -> Integer
addHeight n tree = if isBalanced tree then n + 1 else n

grow :: a -> Tree a -> Tree a
grow x Leaf = Node 0 Leaf x Leaf
grow x (Node n left y right)
  | countLeft > countRight = Node n left y (grow x right)
  | countLeft < countRight = Node n (grow x left) y right
  | otherwise              = Node (addHeight n left) (grow x left) y right
  where countLeft  = countNodes left
        countRight = countNodes right

foldTree :: [a] -> Tree a
foldTree = foldr grow seed
