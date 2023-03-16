{-# OPTIONS_GHC -Wall #-}

module LogAnalysis (parseMessage, parse, insert, build) where

import Log
import Text.Read (readMaybe)

-- Exercise 1
-- Define a function `parseMessage` to parse an individual message.
parseMessage :: String -> LogMessage
parseMessage message =
  case messageParts of
    [] -> Unknown ""
    [m] -> Unknown m
    (first : second : rest) -> case messageType of
      Just e@(Error _) -> constructLogMessage e rest
      Just t -> constructLogMessage t (second : rest)
      Nothing -> Unknown message
      where
        messageType = parseMessageType first second
  where
    messageParts = words message

parseMessageType :: String -> String -> Maybe MessageType
parseMessageType "I" _ = Just Info
parseMessageType "W" _ = Just Warning
parseMessageType "E" severityStr =
  case severity of
    Just s -> Just (Error s)
    Nothing -> Nothing
  where
    severity = readMaybe severityStr :: Maybe Int
parseMessageType _ _ = Nothing

constructLogMessage :: MessageType -> [String] -> LogMessage
constructLogMessage messageType [] = LogMessage messageType (-1) ""
constructLogMessage messageType (first : rest) = case timestamp of
  Just time -> LogMessage messageType time (unwords rest)
  Nothing -> LogMessage messageType (-1) (unwords (first : rest))
  where
    timestamp = readMaybe first :: Maybe Int

-- Define a function `parse` to parse an entire log file.
parse :: String -> [LogMessage]
parse messages = map parseMessage (lines messages)

-- Exercise 2
-- Implement `insert()` function which insert a LogMessage
-- into binary tree in a way such that the LogMessage's timestamp
-- will always be larger than all the messages in the left subtree
-- and always be smaller than all the messages in the right subtree.
-- Note: Unknown and LogMessage without timestamp
-- should not modify the tree structure.
insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) tree = tree
insert (LogMessage _ (-1) _) tree = tree
insert _ tree@(Node _ (Unknown _) _) = tree
insert m@(LogMessage {}) Leaf = Node Leaf m Leaf
insert newMessage@(LogMessage _ newTime _) (Node leftSubtree oldMessage@(LogMessage _ oldTime _) rightSubtree) =
  if oldTime >= newTime
    then Node (insert newMessage leftSubtree) oldMessage rightSubtree
    else Node leftSubtree oldMessage (insert newMessage rightSubtree)

-- Exercise 3.
-- Implement function `build()` o build a MessageTree
-- from a list of LogMessage.
build :: [LogMessage] -> MessageTree
build = foldl insertFlip Leaf
  where
    insertFlip = flip insert