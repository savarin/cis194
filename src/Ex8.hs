{-# OPTIONS_GHC -fno-warn-orphans #-}
-- https://www.seas.upenn.edu/~cis1940/spring13/hw/08-IO.pdf

module Ex8
    ( glCons,
      moreFun,
      treeFold,
      nextLevel
    ) where

import Data.Tree
import Employee


-- Exercise 1 Now define the following tools for working with GuestLists:

-- 1. A function
--   glCons :: Employee -> GuestList -> GuestList
-- which adds an Employee to the GuestList (updating the cached Fun score
-- appropriately). Of course, in general this is impossible: the updated fun
-- score should depend on whether the Employee being added is already in the
-- list, or if any of their direct subor- dinates are in the list, and so on.
-- For our purposes, though, you may assume that none of these special cases
-- will hold: that is, glCons should simply add the new Employee and add their
-- fun score without doing any kind of checks.

-- 2. A Monoid instance for GuestList. (How is the Monoid instance supposed to
-- work, you ask? You figure it out!)

-- 3. A function moreFun :: GuestList -> GuestList -> GuestList which takes two
-- GuestLists and returns whichever one of them is more fun, i.e. has the higher
-- fun score. (If the scores are equal it does not matter which is returned.)

instance Semigroup GuestList where
  (<>) (GL e1 s1) (GL e2 s2) = GL (e1 ++ e2) (s1 + s2)

instance Monoid GuestList where
  mempty = GL [] 0

glCons :: Employee -> GuestList -> GuestList
glCons e@(Emp _ s) (GL es ss) = GL (e : es) (s + ss)

moreFun :: GuestList -> GuestList -> GuestList
moreFun g1@(GL _ s1) g2@(GL _ s2)
  | s1 >= s2  = g1
  | otherwise = g2


-- Exercise 2 The Data.Tree module from the standard Haskell libraries defines
-- the type of “rose trees”, where each node stores a data element and has any
-- number of children (i.e. a list of subtrees):
--   data Tree a = Node {
--           rootLabel :: a,         -- label value
--           subForest :: [Tree a]   -- zero or more child trees
--       }

-- Strangely, Data.Tree does not define a fold for this type! Rectify the
-- situation by implementing
--   treeFold :: ... -> Tree a -> b

treeFold :: b -> (a -> [b] -> b) -> Tree a -> b
treeFold e _ (Node _ []) = e
treeFold e f (Node v c) = f v (map (treeFold e f) c)


-- Exercise 3 Write a function
--   nextLevel :: Employee -> [(GuestList, GuestList)]
--                -> (GuestList, GuestList)

-- which takes two arguments. The first is the “boss” of the current subtree
-- (let’s call him Bob). The second argument is a list of the results for each
-- subtree under Bob. Each result is a pair of GuestLists: the first GuestList
-- in the pair is the best possible guest list with the boss of that subtree;
-- the second is the best possible guest list without the boss of that subtree.
-- nextLevel should then compute the overall best guest list that includes Bob,
-- and the overall best guest list that doesn’t include Bob.

nextLevel :: Employee -> [(GuestList, GuestList)] -> (GuestList, GuestList)
nextLevel e@(Emp _ s) gs = foldr f (GL [e] s, mempty) gs
  where f = (\(g1, g2) acc -> (fst acc <> g2, snd acc <> (moreFun g1 g2)))
