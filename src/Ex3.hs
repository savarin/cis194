-- https://www.cis.upenn.edu/~cis1940/spring13/hw/03-rec-poly.pdf

module Ex3
    ( skips,
      localMaxima
    ) where

-- Exercise 1 Hopscotch
-- Your first task is to write a function
--   skips :: [a] -> [[a]]

-- The output of skips is a list of lists. The first list in the output should
-- be the same as the input list. The second list in the output should contain
-- every second element from the input list. . . and the nth list in the output
-- should contain every nth element from the input list.

-- For example:
--   skips "ABCD"       == ["ABCD", "BD", "C", "D"]
--   skips "hello!"     == ["hello!", "el!", "l!", "l", "o", "!"]
--   skips [1]          == [[1]]
--   skips [True,False] == [[True,False], [False]]
--   skips []           == []

-- Note that the output should be the same length as the input.

skips' :: Int -> [a] -> [a]
skips' n x
  | (z:zs) <- drop n x = z : skips' n zs
  | otherwise          = []

skips :: [a] -> [[a]]
skips x = [skips' n x | n <- [0 .. (length x - 1)]]


-- Exercise 2 Local maxima
-- A local maximum of a list is an element of the list which is strictly greater
-- than both the elements immediately before and after it. For example, in the
-- list [2,3,4,1,5], the only local maximum is 4, since it is greater than the
-- elements immediately before and after it (3 and 1). 5 is not a local maximum
-- since there is no element that comes after it.

-- Write a function
--   localMaxima :: [Integer] -> [Integer]
-- which finds all the local maxima in the input list and returns them in order.

-- For example:
--   localMaxima [2,9,5,6,1] == [9,6]
--   localMaxima [2,3,4,1,5] == [4]
--   localMaxima [1,2,3,4,5] == []

localMaxima :: [Integer] -> [Integer]
localMaxima (x:y:z:zs)
  | x < y && y > z = y : localMaxima (z:zs)
  | otherwise      = localMaxima (y:z:zs)
localMaxima _ = []
