-- https://www.cis.upenn.edu/~cis1940/spring13/hw/02-ADTs.pdf

module Ex2
    ( MessageType,
      TimeStamp,
      parseMessage
    ) where

import Control.Applicative

data MessageType = Info
                 | Warning
                 | Error Int
  deriving (Show, Eq)

type TimeStamp = Int

data LogMessage = LogMessage MessageType TimeStamp String
                | Unknown String
  deriving (Show, Eq)

data MessageTree = Leaf
                 | Node MessageTree LogMessage MessageTree
  deriving (Show, Eq)

-- | @testParse p n f@ tests the log file parser @p@ by running it
--   on the first @n@ lines of file @f@.
testParse :: (String -> [LogMessage])
          -> Int
          -> FilePath
          -> IO [LogMessage]
testParse parse n file = take n . parse <$> readFile file

-- | @testWhatWentWrong p w f@ tests the log file parser @p@ and
--   warning message extractor @w@ by running them on the log file
--   @f@.
testWhatWentWrong :: (String -> [LogMessage])
                  -> ([LogMessage] -> [String])
                  -> FilePath
                  -> IO [String]
testWhatWentWrong parse whatWentWrong file
  = whatWentWrong . parse <$> readFile file


-- Exercise 1 The first step is figuring out how to parse an individual message.
-- Define a function
--   parseMessage :: String -> LogMessage
-- which parses an individual line from the log file.

-- parseMessage "E 2 562 help help" == LogMessage (Error 2) 562 "help help"
-- parseMessage "I 29 la la la" == LogMessage Info 29 "la la la"
-- parseMessage "This is not in the right format" == Unknown "This is not in the right format"

parseMessage :: String -> LogMessage
parseMessage s
  | ("E" : x : y : zs) <- w = parseError (read x :: Int) (read y :: Int) zs
  | ("I" : x : ys) <- w = parseInfoWarning Info (read x :: Int) ys
  | ("W" : x : ys) <- w = parseInfoWarning Warning (read x :: Int) ys
  | otherwise = Unknown s
  where w = words s

parseError :: Int -> Int -> [String] -> LogMessage
parseError err ts msg = LogMessage (Error err) ts (unwords msg)

parseInfoWarning :: MessageType -> Int -> [String] -> LogMessage
parseInfoWarning Info ts msg = LogMessage Info ts (unwords msg)
parseInfoWarning Warning ts msg = LogMessage Warning ts (unwords msg)
parseInfoWarning _ _ msg = Unknown (unwords msg)
