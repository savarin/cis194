-- https://www.cis.upenn.edu/~cis1940/spring13/hw/02-ADTs.pdf

module Ex2
    ( MessageType,
      TimeStamp,
      LogMessage,
      MessageTree,
      parseInt,
      parseTimeStamp,
      parseMessage,
      parseError,
      parseInfoWarning,
      parse,
      initialize,
      insert,
      build,
      inOrder
    ) where

import Text.Read (readMaybe)

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
-- testParse :: (String -> [LogMessage])
--           -> Int
--           -> FilePath
--           -> IO [LogMessage]
-- testParse parse n file = take n . parse <$> readFile file

-- | @testWhatWentWrong p w f@ tests the log file parser @p@ and
--   warning message extractor @w@ by running them on the log file
--   @f@.
-- testWhatWentWrong :: (String -> [LogMessage])
--                   -> ([LogMessage] -> [String])
--                   -> FilePath
--                   -> IO [String]
-- testWhatWentWrong parse whatWentWrong file
--   = whatWentWrong . parse <$> readFile file


-- Exercise 1 The first step is figuring out how to parse an individual message.
-- Define a function
--   parseMessage :: String -> LogMessage
-- which parses an individual line from the log file.

-- parseMessage "E 2 562 help help" == LogMessage (Error 2) 562 "help help"
-- parseMessage "I 29 la la la" == LogMessage Info 29 "la la la"
-- parseMessage "This is not in the right format" == Unknown "This is not in the right format"

parseMessage :: String -> LogMessage
parseMessage s
  | ("E" : x : y : zs) <- w = parseError (parseInt x) (parseTimeStamp y) zs
  | ("I" : x : ys) <- w = parseInfoWarning Info (parseTimeStamp x) ys
  | ("W" : x : ys) <- w = parseInfoWarning Warning (parseTimeStamp x) ys
  | otherwise = Unknown s
  where w = words s

parseInt :: String -> Maybe Int
parseInt = readMaybe

parseTimeStamp :: String -> Maybe TimeStamp
parseTimeStamp = readMaybe

parseError :: Maybe Int -> Maybe TimeStamp -> [String] -> LogMessage
parseError (Just err) (Just ts) msg = LogMessage (Error err) ts (unwords msg)
parseError _ _ msg = Unknown (unwords msg)

parseInfoWarning :: MessageType -> Maybe TimeStamp -> [String] -> LogMessage
parseInfoWarning Info (Just ts) msg = LogMessage Info ts (unwords msg)
parseInfoWarning Warning (Just ts) msg = LogMessage Warning ts (unwords msg)
parseInfoWarning _ _ msg = Unknown (unwords msg)

parse :: String -> [LogMessage]
parse = (map parseMessage) . lines


-- Exercise 2 Define a function
--   insert :: LogMessage -> MessageTree -> MessageTree
-- which inserts a new LogMessage into an existing MessageTree, producing a new
-- MessageTree. insert may assume that it is given a sorted MessageTree, and
-- must produce a new sorted MessageTree containing the new LogMessage in
-- addition to the contents of the original MessageTree.

-- However, note that if insert is given a LogMessage which is Unknown, it
-- should return the MessageTree unchanged.

initialize :: MessageTree
initialize = Leaf

insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) msgtree = msgtree
insert logmsg Leaf = Node Leaf logmsg Leaf
insert logmsg@(LogMessage _ ts _) (Node left logmsg'@(LogMessage _ ts' _) right)
  | ts <= ts' = Node (insert logmsg left) logmsg' right
  | otherwise = Node left logmsg' (insert logmsg right)
insert _ _ = Leaf


-- Exercise 3 Once we can insert a single LogMessage into a MessageTree, we can
-- build a complete MessageTree from a list of messages. Specifically, define a
-- function
--   build :: [LogMessage] -> MessageTree
-- which builds up a MessageTree containing the messages in the list, by
-- successively inserting the messages into a MessageTree (beginning with a
-- Leaf).

build :: [LogMessage] -> MessageTree
build []     = Leaf
build (x:xs) = insert x (build xs)


-- Exercise 4 Finally, define the function
--   inOrder :: MessageTree -> [LogMessage]
-- which takes a sorted MessageTree and produces a list of all the LogMessages
-- it contains, sorted by timestamp from smallest to biggest (This is known as
-- an in-order traversal of the MessageTree.)

inOrder :: MessageTree -> [LogMessage]
inOrder Leaf                     = []
inOrder (Node left logmsg right) = inOrder left ++ [logmsg] ++ inOrder right
