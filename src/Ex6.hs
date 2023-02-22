-- https://www.cis.upenn.edu/~cis1940/spring13/hw/06-laziness.pdf

module Ex6
    ( fib,
      fibs1,
      fibs2,
      streamToList,
      streamRepeat,
      streamMap,
      streamFromSeed
    ) where


-- Exercise 1
-- Translate the above definition of Fibonacci numbers directly into a recursive
-- function definition of type
--   fib :: Integer -> Integer
-- so that fib n computes the nth Fibonacci number Fn.

-- Now use fib to define the infinite list of all Fibonacci numbers
--   fibs1 :: [Integer]

fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)

fibs1 :: [Integer]
fibs1 = map fib [0..]


-- Exercise 2
-- Your task for this exercise is to come up with more efficient implementation.
-- Specifically, define the infinite list
--   fibs2 :: [Integer]
-- so that it has the same elements as fibs1, but computing the first n elements
-- of fibs2 requires only O(n) addition operations. Be sure to use standard
-- recursion pattern(s) from the Prelude as appropriate.

fibs2 :: [Integer]
fibs2 = 0 : 1 : (zipWith (+) fibs2 (tail fibs2))


-- Exercise 3
-- Define a data type of polymorphic streams, Stream.

-- Write a function to convert a Stream to an infinite list,
--   streamToList :: Stream a -> [a]

-- To test your Stream functions in the succeeding exercises, it will be useful
-- to have an instance of Show for Streams. However, if you put deriving Show
-- after your definition of Stream, as one usually does, the resulting instance
-- will try to print an entire Stream — which, of course, will never finish.

-- Instead, you should make your own instance of Show for Stream,
--   instance Show a => Show (Stream a) where
--      show ...
-- which works by showing only some prefix of a stream (say, the first 20
-- elements).

data Stream a = Cons a (Stream a)

streamToList :: Stream a -> [a]
streamToList (Cons x xs) = x : (streamToList xs)

instance Show a => Show (Stream a)
  where show = show . take 10 . streamToList


-- Exercise 4
-- Write a function
--    streamRepeat :: a -> Stream a
-- which generates a stream containing infinitely many copies of the given
-- element.

-- Write a function
--   streamMap :: (a -> b) -> Stream a -> Stream b
-- which applies a function to every element of a Stream.

-- Write a function
--   streamFromSeed :: (a -> a) -> a -> Stream a
-- which generates a Stream from a “seed” of type a, which is the first element
-- of the stream, and an “unfolding rule” of type a -> a which specifies how to
-- transform the seed into a new seed, to be used for generating the rest of the
-- stream.

listToStream :: [a] -> Stream a
listToStream (x : xs) = Cons x (listToStream xs)

streamRepeat :: a -> Stream a
streamRepeat = listToStream . repeat

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (Cons x xs) = Cons (f x) (streamMap f xs)

streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed f a = Cons a (streamMap f (streamFromSeed f a))
