-- https://www.seas.upenn.edu/~cis1940/spring13/hw/10-applicative.pdf

module Ex10
    ( abParser,
    ) where

import AParser
import Control.Applicative


-- Exercise 1  First, you’ll need to implement a Functor instance for Parser.
-- Hint: You may find it useful to implement a function
--   first :: (a -> b) -> (a, c) -> (b, c)

first :: (a -> b) -> (a, c) -> (b, c)
first f (x, y) = (f x, y)

instance Functor Parser where
  fmap f (Parser p) = Parser (fmap (first f) . p)


-- Exercise 2 Now implement an Applicative instance for Parser:

-- pure a represents the parser which consumes no input and successfully returns
-- a result of a.

-- p1 <*> p2 represents the parser which first runs p1 (which will consume some
-- input and produce a function), then passes the remaining input to p2 (which
-- consumes more input and produces some value), then returns the result of
-- applying the function to the value. However, if either p1 or p2 fails then
-- the whole thing should also fail (put another way, p1 <*> p2 only succeeds
-- if both p1 and p2 succeed).

-- instance Applicative Parser where
--   pure  :: a -> f a
--   (<*>) :: f (a -> b) -> f a -> f b

instance Applicative Parser where
  pure a = Parser (\s -> Just (a, s))
  p1 <*> p2 = Parser (\s ->
      case runParser p1 s of
        Nothing -> Nothing
        Just (f, s') -> runParser (f <$> p2) s'
    )
