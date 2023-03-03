-- https://www.cis.upenn.edu/~cis1940/spring13/hw/01-intro.pdf

module Ex1
    ( toDigitsRev,
      toDigits,
      doubleEveryOther,
      sumDigits,
      validate,
      hanoi,
    ) where


-- Exercise 1 We need to first find the digits of a number. Define the functions
--   toDigits    :: Integer -> [Integer]
--   toDigitsRev :: Integer -> [Integer]

-- toDigits should convert positive Integers to a list of digits. (For 0 or
-- negative inputs, toDigits should return the empty list.) toDigitsRev should
-- do the same, but with the digits reversed.

-- Example: toDigits 1234 == [1,2,3,4]
-- Example: toDigitsRev 1234 == [4,3,2,1]
-- Example: toDigits 0 == []
-- Example: toDigits (-17) == []

toDigitsRev :: Integer -> [Integer]
toDigitsRev 0 = []
toDigitsRev x = mod x 10 : toDigitsRev (div x 10)

toDigits :: Integer -> [Integer]
toDigits = reverse . toDigitsRev


-- Exercise 2 Once we have the digits in the proper order, we need to double
-- every other one. Define a function
--   doubleEveryOther :: [Integer] -> [Integer]

-- Remember that doubleEveryOther should double every other number beginning
-- from the right, that is, the second-to-last, fourth-to-last, ... numbers are
-- doubled.

-- Example: doubleEveryOther [8,7,6,5] == [16,7,12,5]
-- Example: doubleEveryOther [1,2,3] == [1,4,3]

doubleEveryOther' :: [Integer] -> [Integer]
doubleEveryOther' []           = []
doubleEveryOther' (x : [])     = [x]
doubleEveryOther' (x : y : zs) = x : (2 * y) : doubleEveryOther' zs

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther = reverse . doubleEveryOther' . reverse


-- Exercise 3 The output of doubleEveryOther has a mix of one-digit and two
-- -digit numbers. Define the function
--   sumDigits :: [Integer] -> Integer
-- to calculate the sum of all digits.

-- Example: sumDigits [16,7,12,5] = 1 + 6 + 7 + 1 + 2 + 5 = 22

sumDigits :: [Integer] -> Integer
sumDigits []       = 0
sumDigits (x : xs) = sum (toDigits x) + sumDigits xs


-- Exercise 4 Define the function
--   validate :: Integer -> Bool
-- that indicates whether an Integer could be a valid credit card number. This
-- will use all functions defined in the previous exercises.

-- Example: validate 4012888888881881 = True
-- Example: validate 4012888888881882 = False

validate :: Integer -> Bool
validate = (== 0) . (`mod` 10) . sumDigits . doubleEveryOther . toDigits


-- Exercise 5 For this exercise, define a function hanoi with the following type:
--   type Peg = String
--   type Move = (Peg, Peg)
--   hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]

-- Given the number of discs and names for the three pegs, hanoi should return a
-- list of moves to be performed to move the stack of discs from the first peg
-- to the second.

type Peg = String
type Move = (Peg, Peg)

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 0 _ _ _ = []
hanoi 1 a b _ = [(a, b)]
hanoi 2 a b c = [(a, c)] ++ hanoi 1 a b c ++ [(c, b)]
hanoi n a b c = hanoi (n - 1) a c b ++ [(a, b)] ++ hanoi (n - 1) c b a
