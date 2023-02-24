-- https://www.seas.upenn.edu/~cis1940/spring13/hw/08-IO.pdf

module Ex8
    ( glCons,
      moreFun
    ) where

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
