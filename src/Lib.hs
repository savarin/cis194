module Lib
    ( someFunc,
      toDigitsRev,
      toDigits,
      doubleEveryOther,
      sumDigits,
      validate
    ) where

someFunc :: IO ()
someFunc = putStrLn "someFunc"


-- https://www.cis.upenn.edu/~cis1940/spring13/hw/01-intro.pdf

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
toDigitsRev n = (mod n 10) : toDigitsRev (div n 10)

toDigits :: Integer -> [Integer]
toDigits n = reverse (toDigitsRev n)


-- Exercise 2 Once we have the digits in the proper order, we need to double
-- every other one. Define a function
--   doubleEveryOther :: [Integer] -> [Integer]

-- Remember that doubleEveryOther should double every other number beginning
-- from the right, that is, the second-to-last, fourth-to-last, ... numbers are
-- doubled.

-- Example: doubleEveryOther [8,7,6,5] == [16,7,12,5]
-- Example: doubleEveryOther [1,2,3] == [1,4,3]

doubleEveryOther' :: [Integer] -> [Integer]
doubleEveryOther' []       = []
doubleEveryOther' (x:[])   = [x]
doubleEveryOther' (x:y:zs) = x : (2 * y) : doubleEveryOther' zs

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther = reverse . doubleEveryOther' . reverse


-- Exercise 3 The output of doubleEveryOther has a mix of one-digit and two
-- -digit numbers. Define the function
--   sumDigits :: [Integer] -> Integer
-- to calculate the sum of all digits.

-- Example: sumDigits [16,7,12,5] = 1 + 6 + 7 + 1 + 2 + 5 = 22

sumDigits :: [Integer] -> Integer
sumDigits [] = 0
sumDigits (x:ys) = sum (toDigits x) + sumDigits ys


-- Exercise 4 Define the function
--   validate :: Integer -> Bool
-- that indicates whether an Integer could be a valid credit card number. This
-- will use all functions defined in the previous exercises.

-- Example: validate 4012888888881881 = True
-- Example: validate 4012888888881882 = False

divisibleByTen :: Integer -> Bool
divisibleByTen n = mod n 10 == 0

validate :: Integer -> Bool
validate = divisibleByTen. sumDigits . doubleEveryOther . toDigits
