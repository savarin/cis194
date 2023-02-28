-- https://www.seas.upenn.edu/~cis1940/spring13/hw/10-applicative.pdf

module Ex10
    ( abParser,
      abParser_,
      intPair
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


-- Exercise 3 We can also test your Applicative instance using other simple
-- applications of functions to multiple parsers. You should implement each of
-- the following exercises using the Applicative interface to put together
-- simpler parsers into more complex ones. Do not implement them using the low
-- -level definition of a Parser! In other words, pretend that you do not have
-- access to the Parser constructor or even know how the Parser type is defined.

-- Create a parser
--   abParser :: Parser (Char, Char)
-- which expects to see the characters ’a’ and ’b’ and returns them as a pair.
-- That is,
--   *AParser> runParser abParser "abcdef"
--   Just ((’a’,’b’),"cdef")
--   *AParser> runParser abParser "aebcdf"
--   Nothing

-- Now create a parser
--   abParser_ :: Parser ()
-- which acts in the same way as abParser but returns () instead of the
-- characters ’a’ and ’b’.
--   *AParser> runParser abParser_ "abcdef"
--   Just ((),"cdef")
--   *AParser> runParser abParser_ "aebcdf"
--   Nothing

-- Create a parser intPair which reads two integer values separated by a space
-- and returns the integer values in a list. You should use the provided posInt
-- to parse the integer values.
--   *Parser> runParser intPair "12 34"
--   Just ([12,34],"")

abParser :: Parser (Char, Char)
abParser = (,) <$> char 'a' <*> char 'b'

abParser_ :: Parser ()
abParser_ = const () <$> abParser

intPair :: Parser [Integer]
intPair = liftA3 f posInt (char ' ') posInt
  where f a _ b = [a, b]
