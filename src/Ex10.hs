-- https://www.seas.upenn.edu/~cis1940/spring13/hw/10-applicative.pdf

module Ex10
    ( 
    ) where

import AParser


-- Exercise 1
-- First, you’ll need to implement a Functor instance for Parser.
-- Hint: You may find it useful to implement a function
--   first :: (a -> b) -> (a, c) -> (b, c)

first :: (a -> b) -> (a, c) -> (b, c)
first f (x, y) = (f x, y)

instance Functor Parser where
  fmap f (Parser p) = Parser (fmap (first f) . p)
