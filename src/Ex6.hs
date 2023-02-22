-- https://www.cis.upenn.edu/~cis1940/spring13/hw/06-laziness.pdf

module Ex6
    ( fib,
      fibs1,
      fibs2
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
