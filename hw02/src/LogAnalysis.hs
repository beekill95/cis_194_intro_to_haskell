{-# OPTIONS_GHC -Wall #-}

module LogAnalysis (parseMessage, parse) where

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